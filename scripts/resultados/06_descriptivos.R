# =============================================================================
# 06_descriptivos.R
# Flujo de la muestra, estadisticos ponderados con diseno muestral, matriz de
# transicion, pendiente rango-rango, las dos "comparaciones obvias" de la lamina
# "El obstaculo" y la descomposicion de la varianza de la dosis.
# v5: agrega la Tabla 1 del Entregable 3 (expuestos vs no expuestos), la
# pendiente por las cinco bandas de nacimiento de la seccion 4.1 y la
# correlacion entre entidades con su intervalo de Fisher (con las 31 entidades y
# sin la entidad con menos de 100 observaciones).
# Correcciones respecto de la Entrega 2 (se conservan):
#   - La pendiente por cohortes (0.516 -> 0.652 en la lamina) se calcula con la
#     MUESTRA ANALITICA; la muestra completa (con migrantes) queda como referencia.
#   - La descomposicion de la varianza de la dosis se reporta PONDERADA (como todo
#     lo demas); la version sin ponderar (21/21/59 en la lamina) queda al lado.
# Todo es preliminar, ponderado y sin controles.
# Insumo : data/clean/base_modelo.rds
# =============================================================================
source(here::here("scripts", "00_configuracion.R"))
SCRIPT <- "06_descriptivos"
base <- readRDS(file.path(RUTAS$clean, "base_modelo.rds"))
M <- base |> filter(muestra_principal)

# ---- 1. Flujo de la muestra -----------------------------------------------------------------
flujo <- tibble(
  paso = c("ESRU-EMOVI 2023 completa", "Entidad a los 14 dentro del pais",
           "Misma entidad hoy que a los 14 (muestra principal)",
           "Muestra principal sin corresidentes con padres",
           "Muestra placebo (nacidos 1988 o antes)"),
  n = c(nrow(base), sum(base$en_pais), nrow(M), sum(M$corresidencia == 0),
        sum(M$nac <= NAC_PLACEBO_MAX)),
  poblacion_millones = c(sum(base$factor), sum(base$factor[base$en_pais]), sum(M$factor),
                         sum(M$factor[M$corresidencia == 0]),
                         sum(M$factor[M$nac <= NAC_PLACEBO_MAX])) / 1e6) |>
  mutate(perdida = lag(n) - n)
guardar_tabla(flujo, "t06_flujo_muestra")

# ---- 2. Estadisticos ponderados con diseno muestral ----------------------------------------
dis <- survey::svydesign(ids = ~upm_muestra, strata = ~est, weights = ~factor,
                         nest = TRUE, data = base)
dis_m <- subset(dis, muestra_principal)
vars <- ~ r_ori + r_des + dosis + frac + edad + mujer + origen_rural + corresidencia
desc_tot <- survey::svymean(vars, dis_m)
desc_coh <- survey::svyby(vars, ~cohorte_exp, dis_m, survey::svymean)
desc <- bind_rows(
  tibble(cohorte_exp = "Total", variable = names(coef(desc_tot)),
         media = coef(desc_tot), ee = survey::SE(desc_tot)),
  as_tibble(desc_coh) |> select(cohorte_exp, !starts_with("se.")) |>
    pivot_longer(-cohorte_exp, names_to = "variable", values_to = "media") |>
    left_join(as_tibble(desc_coh) |> select(cohorte_exp, starts_with("se.")) |>
                pivot_longer(-cohorte_exp, names_to = "variable", values_to = "ee") |>
                mutate(variable = sub("^se\\.", "", variable)),
              by = c("cohorte_exp", "variable")) |>
    mutate(cohorte_exp = as.character(cohorte_exp))) |>
  mutate(ic_inf = media - 1.96 * ee, ic_sup = media + 1.96 * ee) |>
  left_join(M |> count(cohorte_exp) |> mutate(cohorte_exp = as.character(cohorte_exp)) |>
              bind_rows(tibble(cohorte_exp = "Total", n = nrow(M))), by = "cohorte_exp")
guardar_tabla(desc, "t06_descriptivos_ponderados")

# ---- 2b. Tabla 1 del Entregable 3 ------------------------------------------------------------
wstat <- function(x, w) {
  ok <- !is.na(x); x <- x[ok]; w <- w[ok]
  m <- weighted.mean(x, w); c(media = m, de = sqrt(weighted.mean((x - m)^2, w)), n = sum(ok))
}
vars_t1 <- c(r_ori = "Rango de origen (activos del hogar a los 14)",
             r_des = "Rango de destino (activos del hogar actual)",
             r_des_ed = "Rango de destino (anios de escolaridad)",
             ed_yo = "Anios de escolaridad, respondente",
             ed_pad = "Anios de escolaridad, padre o madre mas escolarizado",
             int_crudo = "Intensidad de violencia de la entidad (indice 2007-2017)",
             frac = "Fraccion de la ventana formativa desde 2007",
             dosis_cruda = "Dosis (intensidad x fraccion)",
             mujer = "Mujer", origen_rural = "Origen rural (localidad < 2,500)",
             corresidencia = "Vive con sus padres")
exp_ <- M$frac > 0
t1 <- map_dfr(names(vars_t1), function(v) {
  a <- wstat(M[[v]], M$factor); e <- wstat(M[[v]][exp_], M$factor[exp_])
  u <- wstat(M[[v]][!exp_], M$factor[!exp_])
  tibble(variable = v, etiqueta = vars_t1[[v]], media = a[["media"]], de = a[["de"]],
         media_expuestos = e[["media"]], media_no_expuestos = u[["media"]],
         diferencia = e[["media"]] - u[["media"]], n = a[["n"]])
})
# Error estandar de la media con el diseno completo (UPM, estrato y factor de expansion)
ee_dis <- map_dbl(names(vars_t1), function(v) {
  f <- as.formula(paste0("~", v))
  as.numeric(survey::SE(survey::svymean(f, dis_m, na.rm = TRUE)))
})
t1$ee_disenio <- ee_dis
guardar_tabla(t1, "t06_tabla1_descriptivos")
guardar_tabla(tibble(expuestos = sum(exp_), pct_expuestos = 100 * mean(exp_),
                     no_expuestos = sum(!exp_), municipios_origen = n_distinct(na.omit(M$mun_14)),
                     nac_min = min(M$nac), nac_max = max(M$nac)), "t06_expuestos")

# ---- 3. Matriz de transicion nacional (quintiles de origen x destino) -----------------------
for (k in 1:5) dis_m$variables[[paste0("d", k)]] <- as.numeric(dis_m$variables$q_des == k)
mt <- survey::svyby(~ d1 + d2 + d3 + d4 + d5, ~q_ori, dis_m, survey::svymean)
matriz <- as_tibble(mt) |> select(q_ori, d1:d5) |>
  pivot_longer(-q_ori, names_to = "q_des", values_to = "pct") |>
  mutate(q_des = as.integer(sub("d", "", q_des)), pct = 100 * pct)
guardar_tabla(matriz, "t06_matriz_transicion_nacional")
g <- ggplot(matriz, aes(factor(q_des), factor(q_ori, levels = 5:1), fill = pct)) +
  geom_tile(colour = "white") + geom_text(aes(label = sprintf("%.1f", pct)), size = 3) +
  scale_fill_gradient(low = "white", high = COLORES[["bajo"]], guide = "none") +
  labs(x = "Quintil de destino", y = "Quintil de origen (a los 14)",
       title = "Matriz de transicion nacional (%, por fila)",
       subtitle = "Preliminar, ponderado, sin controles. Muestra principal.",
       caption = FUENTE_EMOVI) + tema_paper(9)
guardar_figura(g, "fig06_matriz_transicion", 5.5, 4.5)

# ---- 4. Pendiente rango-rango: base y comparacion entre cohortes --------------------------------
pend <- function(d) unname(coef(lm(r_des ~ r_ori, data = d, weights = factor))[2])
completa <- base |> filter(con_rangos)
pendientes <- bind_rows(
  tibble(muestra = "Analitica (principal)", grupo = "Todos", n = nrow(M), pendiente = pend(M)),
  map_dfr(list(c(1959, 1966), c(1975, 1984)), function(r) bind_rows(
    tibble(muestra = "Analitica (principal)", grupo = sprintf("Nacidos %d-%d", r[1], r[2]),
           n = sum(M$nac >= r[1] & M$nac <= r[2]),
           pendiente = pend(M |> filter(nac >= r[1], nac <= r[2]))),
    tibble(muestra = "Completa con migrantes (como la lamina)",
           grupo = sprintf("Nacidos %d-%d", r[1], r[2]),
           n = sum(completa$nac >= r[1] & completa$nac <= r[2]),
           pendiente = pend(completa |> filter(nac >= r[1], nac <= r[2])))))) |>
  bind_rows(map_dfr(BANDAS_PENDIENTE, function(r) {
    d <- M |> filter(nac >= r[1], nac <= r[2])
    tibble(muestra = "Analitica, bandas del Entregable 3 (4.1)",
           grupo = sprintf("Nacidos %d-%d", r[1], r[2]), n = nrow(d), pendiente = pend(d),
           pct_expuestos = 100 * mean(d$frac > 0))
  })) |>
  bind_rows(M |> group_by(grupo = paste("Cohorte", cohorte_exp)) |>
              summarise(n = n(), pendiente = pend(pick(everything())), .groups = "drop") |>
              mutate(muestra = "Analitica (principal)"))
guardar_tabla(pendientes, "t06_pendiente_rango_rango")

bins <- M |> mutate(bin = cut(r_ori, seq(0, 1, 0.05), include.lowest = TRUE),
                    grupo = ifelse(frac > 0, "Nacidos 1989-1998 (ventana con repunte)",
                                   "Nacidos 1959-1988")) |>
  group_by(grupo, bin) |>
  summarise(x = weighted.mean(r_ori, factor), y = weighted.mean(r_des, factor), .groups = "drop")
g <- ggplot(bins, aes(x, y, colour = grupo)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ x, linewidth = 0.6) +
  scale_colour_manual(values = c(COLORES[["neutro"]], COLORES[["alto"]]), name = NULL) +
  labs(x = "Rango de origen", y = "Rango de destino",
       title = "Relacion rango-rango (binscatter de 20 grupos)",
       subtitle = "Preliminar, ponderado, sin controles", caption = FUENTE_EMOVI) + tema_paper(9)
guardar_figura(g, "fig07_binscatter_rango_rango", 6, 4.5)

# ---- 5. Comparacion entre entidades (no identifica) ------------------------------------------
ent <- M |> group_by(cve_14) |>
  summarise(pendiente = pend(pick(everything())), n = n(), int_sd = first(int_sd),
            .groups = "drop")
fisher <- function(x, y, etiqueta) {
  r <- cor(x, y); n <- length(x); z <- atanh(r); se <- 1 / sqrt(n - 3)
  tibble(comparacion = etiqueta, r = r, ic_inf = tanh(z - Z_CRIT * se),
         ic_sup = tanh(z + Z_CRIT * se), entidades = n)
}
grandes <- ent$n >= 100
guardar_tabla(bind_rows(
  fisher(ent$int_sd, ent$pendiente, "Todas las entidades de la muestra"),
  fisher(ent$int_sd[grandes], ent$pendiente[grandes],
         sprintf("Sin entidades con menos de 100 observaciones (%s)",
                 paste(ENTIDADES$abrev[match(ent$cve_14[!grandes], ENTIDADES$cve_ent)],
                       collapse = ", ")))),
  "t06_correlacion_entidades")

# ---- 6. Que variacion de la dosis queda para identificar b3 ------------------------------------
r2 <- function(f, ponderar) {
  m <- if (ponderar) lm(f, data = M, weights = factor) else lm(f, data = M)
  summary(m)$r.squared
}
vd <- tibble(
  ponderacion = c("Con factor de expansion (principal)", "Sin ponderar (como la lamina)"),
  entre_entidades = c(r2(dosis ~ factor(cve_14), TRUE), r2(dosis ~ factor(cve_14), FALSE)),
  entre_anios_nacimiento = c(r2(dosis ~ factor(nac), TRUE), r2(dosis ~ factor(nac), FALSE)),
  residual = 1 - c(r2(dosis ~ factor(cve_14) + factor(nac), TRUE),
                   r2(dosis ~ factor(cve_14) + factor(nac), FALSE))) |>
  mutate(across(-ponderacion, ~ 100 * .x))
guardar_tabla(vd, "t06_descomposicion_varianza_dosis")
message("06 terminado.")
