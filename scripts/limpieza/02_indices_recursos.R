# =============================================================================
# 02_indices_recursos.R
# Posicion economica del hogar de origen (a los 14) y de destino (hogar actual).
#
# Principal (igual que la Entrega 2): primer componente principal de los
# reactivos de activos, servicios, vivienda y productos financieros
# (origen: 42 dicotomicos + automoviles; destino: 39 dicotomicos + automoviles),
# con imputacion por la media. Despues, rango percentil ponderado (0-1) DENTRO de
# cada cohorte de edad del CEEY, para comparar a cada persona con su generacion.
#
# Robustez:
#   - ACM por cohorte de edad (metodo del CEEY y de la v2 del equipo), con casos
#     completos.
#   - Destino medido por INGRESO: rango del ingreso per capita del hogar imputado
#     por el CEEY (ingc_pc). Solo existe para el destino: la EMOVI no pregunta el
#     ingreso del hogar a los 14 anios, por eso el origen siempre es de activos.
#
# Insumo : data/clean/emovi_limpia.rds
# Salida : data/clean/emovi_indices.rds
# =============================================================================
source(here::here("scripts", "00_configuracion.R"))
SCRIPT <- "02_indices_recursos"
emovi <- readRDS(file.path(RUTAS$clean, "emovi_limpia.rds"))

# ---- 1. Componentes principales (principal) ---------------------------------------
pca_o <- indice_pca(emovi, REACTIVOS_ORIGEN, AUTOS_ORIGEN)
pca_d <- indice_pca(emovi, REACTIVOS_DESTINO, AUTOS_DESTINO)
emovi$idx_ori <- pca_o$puntaje
emovi$idx_des <- pca_d$puntaje
registrar_decision(SCRIPT, "No sabe / no responde imputado con la media del reactivo antes del PCA",
                   "Igual que la Entrega 2; conserva la muestra completa",
                   pca_o$celdas_imputadas + pca_d$celdas_imputadas)

# ---- 2. ACM por cohorte de edad (robustez) ------------------------------------------
recod_bin <- function(x) {
  x <- a_na(x, COD_NS)
  factor(ifelse(x == 1, "si", ifelse(x == 2, "no", NA)), levels = c("no", "si"))
}
recod_autos <- function(x) {
  x <- a_na(x, COD_NS)
  factor(cut(x, c(-Inf, 0, 1, Inf), labels = c("0", "1", "2+")), levels = c("0", "1", "2+"))
}
acm_por_cohorte <- function(vars_bin, var_autos) {
  X_all <- emovi |> transmute(across(all_of(vars_bin), recod_bin),
                              autos = recod_autos(.data[[var_autos]]))
  out <- rep(NA_real_, nrow(emovi))
  for (g in levels(emovi$cohorte_ceey)) {
    sel <- which(emovi$cohorte_ceey == g)
    X <- as.data.frame(X_all[sel, ]); w <- emovi$factor[sel]
    cc <- stats::complete.cases(X); X <- X[cc, , drop = FALSE]; w <- w[cc]
    prop_min <- sapply(X, function(v) min(prop.table(tapply(w, v, sum, default = 0))))
    X <- X[, names(prop_min)[!is.na(prop_min) & prop_min >= 0.005], drop = FALSE]
    X[] <- lapply(X, droplevels)
    res <- FactoMineR::MCA(X, ncp = 1, graph = FALSE, row.w = w)
    s <- res$ind$coord[, 1]
    favorable <- rowSums(sapply(X, function(v) (as.integer(v) - 1) / (nlevels(v) - 1)))
    if (cor(s, favorable) < 0) s <- -s
    out[sel[cc]] <- s
  }
  out
}
emovi$idx_ori_acm <- acm_por_cohorte(REACTIVOS_ORIGEN, AUTOS_ORIGEN)
emovi$idx_des_acm <- acm_por_cohorte(REACTIVOS_DESTINO, AUTOS_DESTINO)

# ---- 3. Rangos dentro de cohorte de edad ----------------------------------------------
rango_cohorte <- function(x, empates = "promedio") {
  r <- rep(NA_real_, length(x))
  for (g in levels(emovi$cohorte_ceey)) {
    i <- which(emovi$cohorte_ceey == g)
    r[i] <- rango_ponderado(x[i], emovi$factor[i], empates)
  }
  r
}
# Principal (Entregable 3, 3.4): [cumsum(w) - w/2] / sum(w) sobre el ORDEN del
# indice; los empates se resuelven por el orden de fila (igual que modelo.py).
# Correccion v4 (robustez): los empates comparten rango (r_ori_v4, r_des_v4).
emovi <- emovi |>
  mutate(r_ori = rango_cohorte(idx_ori, "orden"), r_des = rango_cohorte(idx_des, "orden"),
         r_ori_v4 = rango_cohorte(idx_ori), r_des_v4 = rango_cohorte(idx_des),
         # destino alternativo: rango de los anios de escolaridad (Tabla 6, panel B)
         r_des_ed = rango_cohorte(ed_yo, "orden"),
         r_des_ed_v4 = rango_cohorte(ed_yo),
         r_ori_acm = rango_cohorte(idx_ori_acm), r_des_acm = rango_cohorte(idx_des_acm),
         r_des_ing = rango_cohorte(ingc_pc),
         q_ori = quintil_de_rango(r_ori), q_des = quintil_de_rango(r_des))
registrar_decision(SCRIPT, "Rangos principales con la regla del Entregable 3 (empates por orden de fila)",
                   "La version con empates compartidos (correccion v4) queda como robustez",
                   sum(duplicated(emovi$idx_ori)))

# ---- 4. Validacion y registro ---------------------------------------------------------------
cor_s <- function(a, b) {
  ok <- !is.na(a) & !is.na(b)
  cov.wt(cbind(rank(a[ok]), rank(b[ok])), wt = emovi$factor[ok], cor = TRUE)$cor[1, 2]
}
validacion <- tribble(
  ~indicador, ~valor,
  "Reactivos del indice de origen (PCA)", pca_o$k,
  "Reactivos del indice de destino (PCA)", pca_d$k,
  "Varianza del primer componente, origen (%)", 100 * pca_o$var_pc1,
  "Varianza del primer componente, destino (%)", 100 * pca_d$var_pc1,
  "Spearman PCA vs ACM, origen", cor_s(emovi$idx_ori, emovi$idx_ori_acm),
  "Spearman PCA vs ACM, destino", cor_s(emovi$idx_des, emovi$idx_des_acm),
  "Spearman destino PCA vs ingreso imputado CEEY (ingc_pc)",
  cor_s(emovi$idx_des, a_na(emovi$ingc_pc, numeric(0))),
  "Casos con ACM completo (origen y destino)",
  sum(!is.na(emovi$r_ori_acm) & !is.na(emovi$r_des_acm))
)
guardar_tabla(validacion, "t02_validacion_indices")
registrar_n(SCRIPT, "Con rango de origen y destino (PCA)", sum(!is.na(emovi$r_ori) & !is.na(emovi$r_des)),
            nrow(emovi))
saveRDS(emovi, file.path(RUTAS$clean, "emovi_indices.rds"))
message("02 terminado. Varianza PC1: origen ", round(100 * pca_o$var_pc1, 1),
        " %, destino ", round(100 * pca_d$var_pc1, 1), " %")
