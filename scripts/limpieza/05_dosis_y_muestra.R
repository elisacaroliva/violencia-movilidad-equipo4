# =============================================================================
# 05_dosis_y_muestra.R
# Dosis individual de exposicion formativa y muestra analitica.
#
# Dosis (presentacion, "Donde creciste por cuando creciste"):
#   D = intensidad de la entidad donde vivia a los 14 (promedio del indice
#       2007-2017, en desviaciones estandar entre las 31 entidades de la muestra)
#     x fraccion de la ventana de 10 a 18 anios que cayo en 2007 o despues.
# El segundo factor depende solo del anio de nacimiento: 0 para quien nacio en
# 1988 o antes, 1/9 para 1989, ..., 8/9 para 1996 y 1 para 1997-1998.
# (La lamina dice "nacido en 1996, uno"; el valor correcto es 8/9.)
# Como D esta en DE de intensidad, b3 se lee directamente "por DE de intensidad".
#
# La intensidad es la del Entregable 3 (promedio 2007-2017 del indice con
# poblacion censal y robo = 0 antes de 2002); su DE entre las 31 entidades de la
# muestra es 0.409. "dosis_cruda" (sin dividir entre la DE) es la de la Tabla 1.
# Alternativas: construccion v4 completa (CONAPO + robo faltante), solo CONAPO,
# solo robo faltante, PC1, solo homicidio, calendario por conglomerado (particion
# principal y particion v4), ventana 12-18, anio de nacimiento - 1 y placebo.
# Insumos : data/clean/emovi_indices.rds, panel_violencia.rds, conglomerados.rds
# Salida  : data/clean/base_modelo.rds
# =============================================================================
source(here::here("scripts", "00_configuracion.R"))
SCRIPT <- "05_dosis_y_muestra"
emovi <- readRDS(file.path(RUTAS$clean, "emovi_indices.rds"))
pv <- readRDS(file.path(RUTAS$clean, "panel_violencia.rds"))
cg <- readRDS(file.path(RUTAS$clean, "conglomerados.rds"))

# ---- 1. Muestra analitica ---------------------------------------------------------------------
n0 <- nrow(emovi)
emovi <- emovi |>
  mutate(en_pais = !is.na(cve_14),
         con_rangos = !is.na(r_ori) & !is.na(r_des),
         muestra_principal = en_pais & con_rangos & migrante == 0)
registrar_n(SCRIPT, "Con entidad a los 14 dentro del pais", sum(emovi$en_pais), n0,
            "Excluye a quien vivia en el extranjero a los 14 (p19 = 33)")
registrar_n(SCRIPT, "Viven en la misma entidad que a los 14 (muestra principal)",
            sum(emovi$muestra_principal), sum(emovi$en_pais),
            "Robustez: con migrantes interestatales")
registrar_n(SCRIPT, "Muestra principal sin corresidentes",
            sum(emovi$muestra_principal & emovi$corresidencia == 0), sum(emovi$muestra_principal))
registrar_decision(SCRIPT, "Se excluye a quienes cambiaron de entidad desde los 14",
                   "Igual que la Entrega 2. Su inclusion va en robustez",
                   sum(emovi$en_pais & emovi$migrante == 1))

# ---- 2. Intensidad en DE entre las entidades de la muestra -------------------------------------
ents <- sort(unique(emovi$cve_14[emovi$muestra_principal]))
de_ <- function(x) sd(x[pv$intensidad$cve_ent %in% ents])
int <- pv$intensidad |>
  mutate(de_int = de_(int), int_sd = int / de_int, int_crudo = int,
         int_v4_sd = int_v4 / de_(int_v4), int_pc1_sd = int_pc1 / de_(int_pc1),
         int_conapo_sd = int_conapo / de_(int_conapo),
         int_faltante_sd = int_faltante / de_(int_faltante),
         # homicidio: tasa estandarizada entre las 32 entidades y llevada a DE entre las 31
         int_hom_z = estandarizar(int_hom), int_hom_sd = int_hom_z / de_(int_hom_z)) |>
  left_join(cg$asignacion |> select(cve_ent, conglomerado = nombre, anio_inicio), by = "cve_ent") |>
  left_join(cg$asignacion_v4 |> select(cve_ent, conglomerado_v4 = nombre,
                                       anio_inicio_v4 = anio_inicio), by = "cve_ent")
guardar_tabla(int |> mutate(en_muestra = cve_ent %in% ents) |>
                select(cve_ent, entidad, abrev, en_muestra, int, int_sd, int_v4_sd, int_pc1_sd,
                       int_hom_sd, conglomerado, anio_inicio, conglomerado_v4) |>
                arrange(desc(int_sd)),
              "t05_intensidad_por_entidad")
guardar_tabla(tibble(version = c("principal (Entregable 3)", "v4 (CONAPO + robo faltante)", "PC1",
                                 "solo CONAPO", "solo robo faltante", "solo homicidio (z)"),
                     de_entre_entidades = c(de_(int$int), de_(int$int_v4), de_(int$int_pc1),
                                            de_(int$int_conapo), de_(int$int_faltante),
                                            de_(int$int_hom_z)),
                     entidades = length(ents)),
              "t05_de_intensidad")

# ---- 3. Dosis ----------------------------------------------------------------------------------
emovi <- emovi |>
  left_join(int |> select(cve_ent, int_sd, int_crudo, int_v4_sd, int_pc1_sd, int_conapo_sd,
                          int_faltante_sd, int_hom_sd, conglomerado, anio_inicio,
                          conglomerado_v4, anio_inicio_v4),
            by = c("cve_14" = "cve_ent")) |>
  mutate(
    frac        = fraccion_ventana(nac, ANIO_REPUNTE),
    dosis       = int_sd * frac,                 # por DE de intensidad
    dosis_cruda = int_crudo * frac,              # unidades del indice (Tabla 1)
    dosis_v4    = int_v4_sd * frac,
    dosis_pc1   = int_pc1_sd * frac,
    dosis_conapo = int_conapo_sd * frac,
    dosis_faltante = int_faltante_sd * frac,
    dosis_hom   = int_hom_sd * frac,
    # calendario propio: el repunte empieza en el anio de inicio del conglomerado;
    # el conglomerado sin repunte no tiene exposicion posterior
    dosis_calendario = ifelse(is.na(anio_inicio), 0,
                              int_sd * fraccion_ventana(nac, anio_inicio)),
    dosis_calendario_v4 = ifelse(is.na(anio_inicio_v4), 0,
                                 int_sd * fraccion_ventana(nac, anio_inicio_v4)),
    dosis_ventana_alt = int_sd * fraccion_ventana(nac, ANIO_REPUNTE, EDAD_VENTANA_ALT),
    dosis_nac_menos1  = int_sd * fraccion_ventana(nac - 1, ANIO_REPUNTE),
    frac_placebo  = fraccion_ventana(nac, ANIO_REPUNTE, EDAD_PLACEBO),
    dosis_placebo = int_sd * frac_placebo
  )
stopifnot(all(emovi$frac[emovi$nac <= 1988] == 0), all(emovi$frac[emovi$nac >= 1997] == 1),
          all(emovi$frac_placebo[emovi$nac <= 1979] == 0),
          !anyNA(emovi$dosis[emovi$muestra_principal]))

M <- emovi |> filter(muestra_principal)
tab <- M |> group_by(nac) |>
  summarise(n = n(), fraccion_ventana = first(frac),
            dosis_media = weighted.mean(dosis, factor),
            dosis_min = min(dosis), dosis_max = max(dosis),
            dosis_calendario_media = weighted.mean(dosis_calendario, factor),
            fraccion_placebo = first(frac_placebo), .groups = "drop")
guardar_tabla(tab, "t05_dosis_por_anio_de_nacimiento")

# ---- 4. Figura 2 del Entregable 3: los dos componentes de la dosis -------------------------------
g <- tibble(nac = 1959:1998, frac = fraccion_ventana(1959:1998, ANIO_REPUNTE)) |>
  ggplot(aes(nac, frac)) + geom_step(direction = "mid") + geom_point(size = 1) +
  labs(x = "Año de nacimiento", y = "Fracción de la ventana 10-18 desde 2007",
       title = "(a) Componente de cohorte de la dosis",
       subtitle = "0 para nacidos en 1988 o antes; 8/9 para 1996; 1 para 1997-1998.",
       caption = FUENTE_EMOVI) + tema_paper(9)
guardar_figura(g, "fig02a_fraccion_ventana", 6, 4)
g <- int |> mutate(en_muestra = ifelse(cve_ent %in% ents, "Con muestra", "Sin observaciones en la muestra"),
                   abrev = reorder(abrev, int)) |>
  ggplot(aes(abrev, int, fill = conglomerado, alpha = en_muestra)) + geom_col(colour = "grey30") +
  scale_fill_manual(values = COLORES_CONG, name = "Conglomerado",
                    labels = c("Repunte tardio" = "Repunte tardío", "Repunte temprano" = "Repunte temprano", "Sin repunte" = "Sin repunte")) +
  scale_alpha_manual(values = c("Con muestra" = 1, "Sin observaciones en la muestra" = 0.25),
                     name = NULL) +
  labs(x = NULL, y = "Índice medio 2007-2017",
       title = "(b) Intensidad de la violencia por entidad (ordenada)",
       subtitle = sprintf("DE entre las %d entidades con muestra: %.3f. Tlaxcala sin observaciones.",
                          length(ents), de_(int$int)),
       caption = FUENTE_SESNSP) + tema_paper(9) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 7))
guardar_figura(g, "fig02b_intensidad_por_entidad", 9, 4.5)

saveRDS(emovi, file.path(RUTAS$clean, "base_modelo.rds"))
message("05 terminado. Muestra principal: ", nrow(M), "; entidades: ", length(ents))
