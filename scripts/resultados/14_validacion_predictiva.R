# =============================================================================
# 14_validacion_predictiva.R
# Componente predictivo de ciencia de datos (complementa al diseno causal).
# Pregunta: ?conocer la exposicion a la violencia mejora la PREDICCION fuera de
# muestra del rango de destino, una vez que se conocen el origen, la entidad y
# la cohorte? Si la violencia cambiara la persistencia, un modelo flexible
# deberia aprovechar la dosis para predecir mejor; si no, su aporte sera nulo.
#
# Diseno de la validacion:
#   - Particion entrenamiento/prueba 80/20 POR UNIDAD PRIMARIA DE MUESTREO (UPM),
#     de modo que ninguna UPM aparece en ambos lados (evita fuga por
#     conglomeracion del diseno muestral). Semilla fija.
#   - Validacion cruzada de 5 pliegues, tambien por UPM, dentro del
#     entrenamiento para ajustar los hiperparametros del bosque aleatorio
#     (mtry y tamano minimo de nodo).
#   - Todo se pondera con el factor de expansion: ajuste y metricas.
# Modelos:
#   M0 Referencia: rango-rango lineal (r_des ~ r_ori).
#   M1 Lineal con EF de entidad y nacimiento y sus pendientes, mas controles (sin dosis).
#   M2 M1 + dosis + dosis x r_ori (la triple diferencia usada como predictor).
#   M3 Bosque aleatorio con todas las variables, sin dosis.
#   M4 Bosque aleatorio con todas las variables y la dosis.
# Metricas en prueba: RMSE, MAE y R2 ponderados; diferencias M2-M1 y M4-M3 con
# IC bootstrap (1,000 remuestreos de UPM de prueba). Interpretacion: importancia
# por permutacion en prueba y perfiles de la pendiente predicha por el bosque
# para expuestos y no expuestos.
# El resultado es PREDICTIVO: no identifica efectos causales (ver texto, 4.6).
# Insumo : data/clean/base_modelo.rds
# =============================================================================
source(here::here("scripts", "00_configuracion.R"))
SCRIPT <- "14_validacion_predictiva"
if (!requireNamespace("ranger", quietly = TRUE)) stop("Instala el paquete ranger: install.packages('ranger')")
base <- readRDS(file.path(RUTAS$clean, "base_modelo.rds"))
M <- base |> filter(muestra_principal) |>
  mutate(ed_pad_falta = as.integer(is.na(ed_pad)),
         ed_pad = ifelse(is.na(ed_pad), median(ed_pad, na.rm = TRUE), ed_pad),
         cve_14 = factor(cve_14), nac_f = factor(nac), tam_14 = as.numeric(tam_14),
         upm = as.character(upm_muestra)) |>
  as.data.frame()

# ---- 1. Particion por UPM ----------------------------------------------------------------
set.seed(SEMILLA)
upms <- unique(M$upm)
prueba_upm <- sample(upms, round(0.2 * length(upms)))
M$prueba <- M$upm %in% prueba_upm
E <- M[!M$prueba, ]; P <- M[M$prueba, ]
pliegue_upm <- setNames(sample(rep(1:5, length.out = length(unique(E$upm)))), unique(E$upm))
E$pliegue <- pliegue_upm[E$upm]
registrar_n(SCRIPT, "Entrenamiento (80 % de UPM)", nrow(E), nrow(M))
registrar_n(SCRIPT, "Prueba (20 % de UPM)", nrow(P), nrow(M))

wmet <- function(y, yhat, w) {
  e <- y - yhat
  c(rmse = sqrt(weighted.mean(e^2, w)), mae = weighted.mean(abs(e), w),
    r2 = 1 - sum(w * e^2) / sum(w * (y - weighted.mean(y, w))^2))
}
CONTROLES <- "mujer + origen_rural + ed_pad + ed_pad_falta + factor(tam_14)"
f_lin <- list(
  M0 = r_des ~ r_ori,
  M1 = as.formula(paste("r_des ~ r_ori * (cve_14 + nac_f) +", CONTROLES)),
  M2 = as.formula(paste("r_des ~ r_ori * (cve_14 + nac_f) +", CONTROLES, "+ dosis + dosis:r_ori")))
X_RF <- c("r_ori", "nac", "cve_14", "mujer", "origen_rural", "ed_pad", "ed_pad_falta", "tam_14")
X_RF_D <- c(X_RF, "dosis", "int_sd", "frac")

ajustar_rf <- function(d, vars, mtry, nodo, arboles = 500) {
  ranger::ranger(x = d[, vars], y = d$r_des, case.weights = d$factor, num.trees = arboles,
                 mtry = min(mtry, length(vars)), min.node.size = nodo,
                 respect.unordered.factors = "order", seed = SEMILLA, num.threads = 2)
}

# ---- 2. Ajuste de hiperparametros por validacion cruzada (5 pliegues por UPM) ----------------
rejilla <- expand.grid(mtry = c(2, 3, 5), nodo = c(25, 100, 400))
cv <- map_dfr(seq_len(nrow(rejilla)), function(i) {
  map_dfr(1:5, function(k) {
    tr <- E[E$pliegue != k, ]; va <- E[E$pliegue == k, ]
    map_dfr(list(sin_dosis = X_RF, con_dosis = X_RF_D), .id = "variables", function(v) {
      m <- ajustar_rf(tr, v, rejilla$mtry[i], rejilla$nodo[i], arboles = 200)
      as_tibble(as.list(wmet(va$r_des, predict(m, va[, v])$predictions, va$factor))) |>
        mutate(mtry = rejilla$mtry[i], nodo = rejilla$nodo[i], pliegue = k)
    })
  })
})
cv_res <- cv |> group_by(variables, mtry, nodo) |>
  summarise(rmse_cv = mean(rmse), rmse_cv_de = sd(rmse), r2_cv = mean(r2), .groups = "drop")
guardar_tabla(cv_res, "t14_validacion_cruzada_rejilla")
mejor <- cv_res |> group_by(variables) |> slice_min(rmse_cv, n = 1, with_ties = FALSE) |> ungroup()
# Validacion cruzada de los modelos lineales (mismos pliegues), para comparacion
cv_lin <- map_dfr(names(f_lin), function(nm) map_dfr(1:5, function(k) {
  tr <- E[E$pliegue != k, ]; va <- E[E$pliegue == k, ]
  m <- lm(f_lin[[nm]], data = tr, weights = factor)
  as_tibble(as.list(wmet(va$r_des, predict(m, va), va$factor))) |> mutate(modelo = nm, pliegue = k)
})) |> group_by(modelo) |> summarise(rmse_cv = mean(rmse), rmse_cv_de = sd(rmse), r2_cv = mean(r2))

# ---- 3. Ajuste final en entrenamiento y evaluacion en prueba -------------------------------
lin <- map(f_lin, ~ lm(.x, data = E, weights = factor))
hp <- function(v) mejor[mejor$variables == v, ]
rf3 <- ajustar_rf(E, X_RF, hp("sin_dosis")$mtry, hp("sin_dosis")$nodo)
rf4 <- ajustar_rf(E, X_RF_D, hp("con_dosis")$mtry, hp("con_dosis")$nodo)
pred <- tibble(y = P$r_des, w = P$factor, upm = P$upm,
               M0 = predict(lin$M0, P), M1 = predict(lin$M1, P), M2 = predict(lin$M2, P),
               M3 = predict(rf3, P[, X_RF])$predictions, M4 = predict(rf4, P[, X_RF_D])$predictions)
etq <- c(M0 = "M0 Referencia: rango-rango lineal", M1 = "M1 Lineal con EF y controles, sin dosis",
         M2 = "M2 Lineal con EF, controles y dosis (triple diferencia)",
         M3 = "M3 Bosque aleatorio, sin dosis", M4 = "M4 Bosque aleatorio, con dosis")
prueba <- map_dfr(names(etq), function(m) as_tibble(as.list(wmet(pred$y, pred[[m]], pred$w))) |>
                    mutate(modelo = m, descripcion = etq[[m]], .before = 1)) |>
  left_join(bind_rows(cv_lin, mejor |> transmute(modelo = ifelse(variables == "sin_dosis", "M3", "M4"),
                                                  rmse_cv, rmse_cv_de, r2_cv)), by = "modelo") |>
  left_join(mejor |> transmute(modelo = ifelse(variables == "sin_dosis", "M3", "M4"), mtry, nodo), by = "modelo") |>
  mutate(n_entrenamiento = nrow(E), n_prueba = nrow(P), upm_prueba = length(prueba_upm))
guardar_tabla(prueba, "t14_desempeno_prueba")

# ---- 4. Diferencias con IC bootstrap por UPM de prueba ------------------------------------
set.seed(SEMILLA)
u <- unique(pred$upm); idx <- split(seq_len(nrow(pred)), pred$upm)
rmse_w <- function(d, m) sqrt(weighted.mean((d$y - d[[m]])^2, d$w))
boot <- map_dfr(1:1000, function(b) {
  d <- pred[unlist(idx[sample(u, length(u), replace = TRUE)]), ]
  tibble(d21 = rmse_w(d, "M2") - rmse_w(d, "M1"), d43 = rmse_w(d, "M4") - rmse_w(d, "M3"),
         d10 = rmse_w(d, "M1") - rmse_w(d, "M0"), d30 = rmse_w(d, "M3") - rmse_w(d, "M0"))
})
dif <- tibble(comparacion = c("M2 - M1: aporte de la dosis (lineal)", "M4 - M3: aporte de la dosis (bosque)",
                              "M1 - M0: aporte de EF y controles", "M3 - M0: aporte del bosque sin dosis"),
              dif_rmse = c(rmse_w(pred, "M2") - rmse_w(pred, "M1"), rmse_w(pred, "M4") - rmse_w(pred, "M3"),
                           rmse_w(pred, "M1") - rmse_w(pred, "M0"), rmse_w(pred, "M3") - rmse_w(pred, "M0")),
              ic_inf = c(quantile(boot$d21, .025), quantile(boot$d43, .025), quantile(boot$d10, .025), quantile(boot$d30, .025)),
              ic_sup = c(quantile(boot$d21, .975), quantile(boot$d43, .975), quantile(boot$d10, .975), quantile(boot$d30, .975)))
guardar_tabla(dif, "t14_diferencias_rmse")

# ---- 5. Importancia por permutacion en prueba (bosque con dosis) -----------------------------
set.seed(SEMILLA)
base_rmse <- rmse_w(pred, "M4")
imp <- map_dfr(X_RF_D, function(v) {
  inc <- replicate(10, { Q <- P[, X_RF_D]; Q[[v]] <- sample(Q[[v]])
    sqrt(weighted.mean((P$r_des - predict(rf4, Q)$predictions)^2, P$factor)) - base_rmse })
  tibble(variable = v, aumento_rmse = mean(inc), de = sd(inc))
}) |> arrange(desc(aumento_rmse))
guardar_tabla(imp, "t14_importancia_permutacion")

# ---- 6. Perfiles: pendiente predicha por el bosque, expuestos vs no expuestos ----------------
# Dependencia parcial: se fija r_ori en una rejilla y la dosis en 0 o en el p90
# de los expuestos (con frac = 1 e intensidad correspondiente), promediando sobre P.
p90 <- quantile(M$int_sd[M$frac > 0], 0.9)
perfil <- map_dfr(c(0, 1), function(esc) map_dfr(seq(0.05, 0.95, 0.1), function(r) {
  Q <- P[, X_RF_D]; Q$r_ori <- r
  if (esc == 1) { Q$frac <- 1; Q$int_sd <- p90; Q$dosis <- p90 } else { Q$frac <- 0; Q$dosis <- 0 }
  tibble(escenario = ifelse(esc == 1, "Dosis alta (p90 de intensidad, ventana completa)", "Sin dosis"),
         r_ori = r, r_des_pred = weighted.mean(predict(rf4, Q)$predictions, P$factor))
}))
pend_perfil <- perfil |> group_by(escenario) |>
  summarise(pendiente_predicha = coef(lm(r_des_pred ~ r_ori))[2], .groups = "drop")
guardar_tabla(perfil, "t14_perfiles_dependencia_parcial")
guardar_tabla(pend_perfil, "t14_pendiente_perfiles")

g <- ggplot(perfil, aes(r_ori, r_des_pred, linetype = escenario, shape = escenario)) +
  geom_line() + geom_point(size = 2) +
  labs(x = "Rango de origen", y = "Rango de destino predicho (bosque aleatorio)", linetype = NULL, shape = NULL,
       title = "Dependencia parcial del rango de destino respecto del origen, con y sin exposicion",
       subtitle = sprintf("Pendiente predicha: %s",
                          paste(sprintf("%s = %.3f", pend_perfil$escenario, pend_perfil$pendiente_predicha), collapse = "; ")),
       caption = paste(FUENTE_EMOVI, FUENTE_SESNSP, sep = "\n")) + tema_paper(9)
guardar_figura(g, "fig11_perfiles_bosque", 7.5, 4.5)
message(sprintf("14 terminado. RMSE prueba: M0 %.4f, M1 %.4f, M2 %.4f, M3 %.4f, M4 %.4f",
                prueba$rmse[1], prueba$rmse[2], prueba$rmse[3], prueba$rmse[4], prueba$rmse[5]))
