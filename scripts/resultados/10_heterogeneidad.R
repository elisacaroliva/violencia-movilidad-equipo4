# =============================================================================
# 10_heterogeneidad.R  (Entregable 3, Tablas 4C y 6, seccion 5.7)
# Panel A. b3 por submuestra: sexo, origen rural (< 2,500 hab.) y quintil de origen.
# Panel B. Destinos alternativos:
#   - rango de anios de escolaridad como destino (triple diferencia completa);
#   - transiciones entre quintiles: modelo lineal de probabilidad sobre la
#     submuestra con un origen dado, con EF de entidad y de anio de nacimiento y
#     control por rango de origen; b es el cambio en la probabilidad (no hay
#     interaccion). Se traduce su EMD a unidades de pendiente con el "tipo de
#     cambio" estimado en celdas entidad x cohorte del CEEY.
# Panel C. ?Hay primera etapa? Desenlaces en niveles sobre la dosis con EF de
#   entidad y de anio de nacimiento: anios de escolaridad, indice de activos de
#   destino (nivel) y migracion interestatal (muestra con migrantes). La ultima
#   columna es la cota superior del IC 95 %: el mayor efecto que no se descarta.
# Por conglomerado: b3 dentro de cada grupo de trayectorias, con permutacion
#   SOLO entre las entidades del grupo (2,000). Con 6 a 15 entidades manda la
#   permutacion; se reportan ambos para que la brecha sea visible.
# Insumo : data/clean/base_modelo.rds
# =============================================================================
source(here::here("scripts", "00_configuracion.R"))
SCRIPT <- "10_heterogeneidad"
base <- readRDS(file.path(RUTAS$clean, "base_modelo.rds"))
M <- base |> filter(muestra_principal)
pend_base <- unname(coef(lm(r_des ~ r_ori, data = M, weights = factor))[2])
de_ent <- function(d) sd(distinct(d, cve_14, int_crudo)$int_crudo)   # escala del Entregable 3

# ---- Panel A: submuestras ------------------------------------------------------------
grupos <- list(
  "Mujeres" = M |> filter(mujer == 1), "Hombres" = M |> filter(mujer == 0),
  "Origen rural (< 2,500 hab.)" = M |> filter(origen_rural == 1),
  "Origen urbano" = M |> filter(origen_rural == 0),
  "Origen en Q1 y Q2" = M |> filter(q_ori <= 2),
  "Origen en Q4 y Q5" = M |> filter(q_ori >= 4))
panel_a <- imap_dfr(grupos, function(d, nombre) {
  out <- tryCatch(estimar_ddd(d, "dosis")$fila, error = function(e)
    tibble(n = nrow(d), nota = conditionMessage(e)))
  out |> mutate(panel = "A. Heterogeneidad", grupo = nombre,
                n_con_dosis = sum(d$dosis != 0), .before = 1)
})

# ---- Panel B: destinos alternativos ----------------------------------------------------
ed <- bind_rows(
  estimar_ddd(M, "dosis", y = "r_des_ed")$fila |>
    mutate(grupo = "Rango de anios de escolaridad (empates por orden, Entregable 3)"),
  estimar_ddd(M, "dosis", y = "r_des_ed_v4")$fila |>
    mutate(grupo = "Rango de anios de escolaridad (empates compartidos)")) |>
  mutate(panel = "B. Destinos alternativos", .before = 1)

q1 <- M |> filter(q_ori == 1); q5 <- M |> filter(q_ori == 5); q12 <- M |> filter(q_ori <= 2)
transiciones <- bind_rows(
  estimar_nivel(q1 |> mutate(y = as.numeric(q_des == 1)), "y", con_rango = TRUE) |>
    mutate(grupo = "Origen Q1 -> sigue en Q1"),
  estimar_nivel(q1 |> mutate(y = as.numeric(q_des >= 4)), "y", con_rango = TRUE) |>
    mutate(grupo = "Origen Q1 -> llega a Q4 o Q5"),
  estimar_nivel(q5 |> mutate(y = as.numeric(q_des == 5)), "y", con_rango = TRUE) |>
    mutate(grupo = "Origen Q5 -> sigue en Q5"),
  estimar_nivel(q12 |> mutate(y = as.numeric(q_des <= 2)), "y", con_rango = TRUE) |>
    mutate(grupo = "Origen Q1-Q2 -> sigue en Q1-Q2")) |>
  mutate(panel = "B. Transiciones entre quintiles", .before = 1)

# Tipo de cambio entre escalas: celdas entidad x cohorte del CEEY con >= 60 casos y >= 12 en Q1
celdas <- M |> group_by(cve_14, cohorte_ceey) |>
  filter(n() >= 60, sum(q_ori == 1) >= 12) |>
  summarise(pend = unname(coef(lm(r_des ~ r_ori, weights = factor))[2]),
            p11 = weighted.mean(q_des[q_ori == 1] == 1, factor[q_ori == 1]), .groups = "drop")
tcm <- lm(p11 ~ pend, data = celdas)
tc <- unname(coef(tcm)[2]); tc_se <- unname(summary(tcm)$coefficients[2, 2])
mde_tr <- transiciones$mde[1] / abs(tc)
cambio <- tibble(celdas = nrow(celdas), tipo_de_cambio = tc, tipo_de_cambio_ee = tc_se,
                 mde_transicion_tasa = transiciones$mde[1], mde_en_pendiente = mde_tr,
                 mde_pct_pendiente = 100 * mde_tr / pend_base,
                 n_q1 = transiciones$n[1])
guardar_tabla(cambio, "t10_tipo_de_cambio_transiciones")

# ---- Panel C: primera etapa ---------------------------------------------------------------
con_migr <- base |> filter(en_pais, !is.na(dosis))
primera <- bind_rows(
  estimar_nivel(M, "ed_yo", "dosis_cruda", escala = de_ent(M)) |> mutate(grupo = "Anios de escolaridad"),
  estimar_nivel(M, "idx_des", "dosis_cruda", escala = de_ent(M)) |>
    mutate(grupo = "Indice de activos de destino (nivel)"),
  estimar_nivel(con_migr, "migrante", "dosis_cruda", escala = de_ent(con_migr)) |>
    mutate(grupo = "Migracion interestatal")) |>
  mutate(panel = "C. Primera etapa", .before = 1)
guardar_tabla(bind_rows(panel_a, ed, transiciones |> rename(b3 = b), primera |> rename(b3 = b)) |>
                relocate(panel, grupo), "t10_tabla6_heterogeneidad")
guardar_tabla(panel_a, "t10_heterogeneidad")

# ---- Por conglomerado, con permutacion dentro del grupo ---------------------------------------
por_conglomerado <- function(var_grupo, particion) {
  map_dfr(sort(unique(M[[var_grupo]])), function(gname) {
    d <- M |> filter(.data[[var_grupo]] == gname) |> as.data.frame()
    ents <- sort(unique(d$cve_14))
    E <- d |> distinct(cve_14, int_sd) |> arrange(cve_14)
    pos <- match(d$cve_14, ents)
    dosis_de <- function(perm) E$int_sd[match(perm, ents)][pos] * d$frac
    fc <- estimar_ddd(d, "dosis")$fila
    pm <- permutacion_ddd(d, d$dosis, dosis_de, ents)
    fc |> mutate(particion = particion, conglomerado = gname, entidades = length(ents),
                 p_permutacion = pm$p, nulo_p2.5 = pm$nulo_p2.5, nulo_p97.5 = pm$nulo_p97.5,
                 .before = 1)
  })
}
cong <- bind_rows(por_conglomerado("conglomerado", "Principal (Entregable 3)"),
                  por_conglomerado("conglomerado_v4", "v4 (indice corregido)"))
guardar_tabla(cong, "t10_conglomerados_permutacion")
message(sprintf("10 terminado. Transiciones: EMD = %.0f %% de la pendiente (tipo de cambio %.3f)",
                cambio$mde_pct_pendiente, tc))
