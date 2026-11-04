# =============================================================================
# 03_panel_violencia.R
# Panel entidad-anio de violencia asociada al crimen organizado, 1997-2017, con
# la serie estatal del SESNSP en su METODOLOGIA ANTERIOR (la unica que llega a
# 1997 y cubre el periodo previo al repunte). No se mezcla con la metodologia
# nueva: todo el panel sale de una sola fuente, sin empalmes.
#
#   1. Cuatro delitos: homicidio doloso, secuestro, extorsion y robo en carreteras
#      con violencia (todos sus subtipos).
#   2. CONSTRUCCION PRINCIPAL = Entregable 3 (seccion 3.3, nota de la Fig. 1):
#      tasas por 100 mil con poblacion interpolada log-lineal entre los censos
#      2010 y 2020; el robo en carreteras sin registro nacional (1997-2001) entra
#      como cero; cada tasa se estandariza en el panel 32 x 21 y el indice es el
#      PROMEDIO de las cuatro tasas estandarizadas.
#   3. Intensidad de la entidad = promedio del indice en 2007-2017.
#   4. Correcciones de la v4 como robustez (no se pierden):
#        indice_v4       : robo faltante antes de 2002 + poblacion CONAPO (D07, D08)
#        indice_conapo   : solo cambia la poblacion (CONAPO)
#        indice_faltante : solo cambia el robo (faltante)
#        indice_pc1      : primer componente (carga negativa del robo, D06)
#        indice_hom      : solo homicidio doloso (poblacion censal)
# Insumos : data/raw/sesnsp/IDEFC_MAnterior*.csv, data/raw/conapo/*.csv
# Salida  : data/clean/panel_violencia.rds
# =============================================================================
source(here::here("scripts", "00_configuracion.R"))
SCRIPT <- "03_panel_violencia"
DELITOS <- MAPA_DELITOS$delito_id

# ---- 1. Lectura y asignacion de categorias ----------------------------------------------
ant <- leer_sesnsp_anterior(buscar_archivo(RUTAS$raw_sesnsp, "anterior.*\\.csv$", TRUE,
                                           "la serie estatal de metodologia anterior (IDEFC_MAnterior)"))
stopifnot(min(ant$anio) <= min(ANIOS_PANEL), max(ant$anio) >= max(ANIOS_PANEL))
guardar_tabla(ant |> group_by(nivel1, nivel2, nivel3) |>
                summarise(anios = paste(range(anio), collapse = "-"), total = sum(total),
                          .groups = "drop"),
              "t03_catalogo_sesnsp")
asig <- asignar_delitos(ant, MAPA_DELITOS)
guardar_tabla(asig, "t03_asignacion_categorias")

casos <- ant |> filter(anio %in% ANIOS_PANEL) |>
  inner_join(asig |> select(nivel1, nivel2, nivel3, delito_id),
             by = c("nivel1", "nivel2", "nivel3")) |>
  group_by(cve_ent, anio, delito_id) |>
  summarise(casos = sum(total), .groups = "drop") |>
  complete(cve_ent = 1:32, anio = ANIOS_PANEL, delito_id = DELITOS, fill = list(casos = 0)) |>
  # un delito sin ningun registro nacional en un anio = no registrado (faltante)
  group_by(delito_id, anio) |>
  mutate(registrado = sum(casos) > 0) |> ungroup() |>
  mutate(casos_e2 = casos,                         # Entrega 2: el no registro cuenta como 0
         casos = ifelse(registrado, casos, NA_real_))
sin_registro <- casos |> filter(!registrado) |> distinct(delito_id, anio)
guardar_tabla(sin_registro, "t03_delitos_sin_registro")
registrar_decision(SCRIPT, "Robo en carreteras sin registro nacional = faltante, no cero",
                   "La Entrega 2 lo trataba como cero; eso hacia parecer 'pacificos' esos anios",
                   nrow(sin_registro) * 32)

# ---- 2. Poblacion y tasas ------------------------------------------------------------------
pob_conapo <- poblacion_conapo(buscar_archivo(RUTAS$raw_conapo, "\\.csv$", TRUE, "poblacion CONAPO"),
                               ANIOS_PANEL)
pob_censo <- poblacion_censal_interpolada(ANIOS_PANEL)
comp_pob <- pob_conapo |> rename(pob_conapo = pob) |>
  left_join(pob_censo |> rename(pob_censal = pob), by = c("cve_ent", "anio")) |>
  mutate(dif_pct = 100 * (pob_censal / pob_conapo - 1))
guardar_tabla(comp_pob |> group_by(anio) |>
                summarise(dif_media_pct = mean(dif_pct), dif_min_pct = min(dif_pct),
                          dif_max_pct = max(dif_pct)),
              "t03_poblacion_censal_vs_conapo")

# ---- 3. Indices entidad-anio ----------------------------------------------------------------
construir_indices <- function(pob, var_casos = "casos") {
  z <- casos |> mutate(casos_v = .data[[var_casos]]) |>
    left_join(pob, by = c("cve_ent", "anio")) |>
    mutate(tasa = casos_v / pob * 1e5) |>
    group_by(delito_id) |> mutate(z = estandarizar(tasa)) |> ungroup()
  ancho <- z |> select(cve_ent, anio, delito_id, z) |>
    pivot_wider(names_from = delito_id, values_from = z) |> arrange(cve_ent, anio)
  Z <- as.matrix(ancho[, DELITOS])
  pc <- indice_pc1_violencia(Z)
  list(panel = ancho |>
         mutate(indice_pc1 = pc$puntaje,
                indice_promedio = rowMeans(Z, na.rm = TRUE),
                indice_hom = hom_doloso,
                tasa_hom = z |> filter(delito_id == "hom_doloso") |>
                  arrange(cve_ent, anio) |> pull(tasa)),
       pc = pc)
}
ix_paper    <- construir_indices(pob_censo, "casos_e2")   # principal (Entregable 3)
ix_v4       <- construir_indices(pob_conapo)               # correccion v4 completa
ix_conapo   <- construir_indices(pob_conapo, "casos_e2")
ix_faltante <- construir_indices(pob_censo)
cargas <- tibble(delito_id = DELITOS,
                 carga_principal = ix_paper$pc$cargas, carga_v4 = ix_v4$pc$cargas) |>
  mutate(var_pc1_principal = ix_paper$pc$var_pc1, var_pc1_v4 = ix_v4$pc$var_pc1,
         filas_completas_v4 = ix_v4$pc$filas_completas)
guardar_tabla(cargas, "t03_cargas_pc1_violencia")
registrar_decision(SCRIPT, "Indice principal = promedio de las 4 tasas estandarizadas, no PC1",
                   sprintf("PC1 (v4) explica %.0f %% y la carga del robo en carreteras es %.2f",
                           100 * ix_v4$pc$var_pc1,
                           cargas$carga_v4[cargas$delito_id == "robo_carr_cv"]))
registrar_decision(SCRIPT, "Construccion principal = Entregable 3 (poblacion censal interpolada, robo = 0 en 1997-2001)",
                   "Decision del equipo para la v5; la construccion v4 queda como robustez (D18)")

panel <- ix_paper$panel |>
  select(cve_ent, anio, all_of(DELITOS), indice = indice_promedio, indice_pc1,
         indice_hom, tasa_hom) |>
  left_join(ix_v4$panel |> select(cve_ent, anio, indice_v4 = indice_promedio),
            by = c("cve_ent", "anio")) |>
  left_join(ix_conapo$panel |> select(cve_ent, anio, indice_conapo = indice_promedio),
            by = c("cve_ent", "anio")) |>
  left_join(ix_faltante$panel |> select(cve_ent, anio, indice_faltante = indice_promedio),
            by = c("cve_ent", "anio")) |>
  left_join(ENTIDADES, by = "cve_ent")
stopifnot(nrow(panel) == 32 * length(ANIOS_PANEL), !anyNA(panel$indice), !anyNA(panel$indice_v4))
guardar_tabla(panel, "t03_panel_entidad_anio")

# ---- 4. Intensidad por entidad (promedio 2007-2017) -------------------------------------------
intensidad <- panel |> filter(anio %in% ANIOS_INTENSIDAD) |>
  group_by(cve_ent, entidad, abrev) |>
  summarise(int = mean(indice), int_pc1 = mean(indice_pc1), int_v4 = mean(indice_v4),
            int_conapo = mean(indice_conapo), int_faltante = mean(indice_faltante),
            int_hom = mean(tasa_hom), .groups = "drop")
guardar_tabla(intensidad, "t03_intensidad_entidad")
guardar_tabla(intensidad |> select(int, starts_with("int_")) |> cor() |>
                as_tibble(rownames = "version"),
              "t03_correlacion_versiones_intensidad")

# ---- 5. Figura 1 del Entregable 3: (a) homicidio doloso; (b) los delitos de prediccion ----------
nac_tasas <- casos |> left_join(pob_censo, by = c("cve_ent", "anio")) |>
  group_by(delito_id, anio) |>
  summarise(tasa = sum(casos_e2) / sum(pob) * 1e5, registrado = all(registrado),
            .groups = "drop") |>
  left_join(MAPA_DELITOS |> distinct(delito_id, etiqueta), by = "delito_id") |>
  mutate(panel = ifelse(delito_id == "hom_doloso", "(a) Homicidio doloso",
                        "(b) Extorsión, secuestro y robo en carreteras con violencia"),
         etiqueta = recode(etiqueta, "Extorsion" = "Extorsión"))
g <- ggplot(nac_tasas, aes(anio, tasa, colour = etiqueta, shape = etiqueta)) +
  annotate("rect", xmin = 2007, xmax = 2012, ymin = -Inf, ymax = Inf, fill = "grey90") +
  geom_line() + geom_point(size = 1.2) +
  facet_wrap(~ panel, scales = "free_y") +
  labs(x = NULL, y = "Carpetas por 100 mil habitantes", colour = NULL, shape = NULL,
       title = "Delitos del indice, tasa nacional 1997-2017",
       subtitle = "Banda gris: 2007-2012. Poblacion interpolada entre los censos 2010 y 2020.",
       caption = FUENTE_SESNSP) + tema_paper(9)
guardar_figura(g, "fig01_delitos_nacional", 9, 4.5)

# ---- 6. Guardado -----------------------------------------------------------------------------------
saveRDS(list(panel = panel, intensidad = intensidad, cargas = cargas),
        file.path(RUTAS$clean, "panel_violencia.rds"))
registrar_n(SCRIPT, "Panel entidad-anio", nrow(panel), NA,
            sprintf("32 entidades x %d anios (%d-%d)", length(ANIOS_PANEL),
                    min(ANIOS_PANEL), max(ANIOS_PANEL)))
message("03 terminado.")
