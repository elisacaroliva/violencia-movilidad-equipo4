# =============================================================================
# 01_limpieza_emovi.R
# Lee la ESRU-EMOVI 2023 (Entrevistado + Hogar) y construye las variables de
# diseno: anio de nacimiento, entidad a los 14 anios, migracion interestatal,
# corresidencia con padres, sexo, tamano de la localidad de origen, ingreso
# (solo robustez), escolaridad propia y de los padres (anios completos, Entregable 3,
# seccion 3.4), municipio a los 14 (diseno municipal) y cohortes descriptivas. No excluye a nadie: solo marca. Las
# exclusiones se aplican en 05 y quedan en output/logs/log_n.csv.
# Insumos : data/raw/emovi/entrevistado_2023.dta, data/raw/emovi/hogar_2023.dta
# Salida  : data/clean/emovi_limpia.rds
# =============================================================================
source(here::here("scripts", "00_configuracion.R"))
SCRIPT <- "01_limpieza_emovi"

# ---- 1. Lectura -------------------------------------------------------------------
ent <- haven::read_dta(buscar_archivo(RUTAS$raw_emovi, "^entrevistado.*\\.dta$", TRUE,
                                      "la base Entrevistado de la ESRU-EMOVI 2023"))
hog <- haven::read_dta(buscar_archivo(RUTAS$raw_emovi, "^hogar.*\\.dta$", TRUE,
                                      "la base Hogar de la ESRU-EMOVI 2023"))
registrar_n(SCRIPT, "ESRU-EMOVI 2023, base Entrevistado", nrow(ent))
stopifnot(!anyDuplicated(ent$index))

necesarias <- c("index", "factor", "upm_muestra", "est", "sexo", "edad", "entidad",
                "cohorte", "p19", "p20_COD", "p21", "ingc_pc", "p9", "p42a", "p42b", REACTIVOS_ORIGEN,
                REACTIVOS_DESTINO, AUTOS_ORIGEN, AUTOS_DESTINO)
faltan <- setdiff(necesarias, names(ent))
if (length(faltan) > 0) stop("Variables ausentes: ", paste(faltan, collapse = ", "))

# ---- 2. Variables basicas -----------------------------------------------------------
num <- function(x) as.numeric(haven::zap_labels(x))
emovi <- ent |>
  mutate(
    across(c(factor, edad, entidad, p19, sexo, cohorte, p21, est, ingc_pc), num),
    nac = ANIO_ENCUESTA - edad,
    mujer = as.integer(sexo == 2),
    # cohorte de edad del CEEY (25-34, ..., 55-64): define el grupo de los rangos
    cohorte_ceey = factor(cohorte, 1:4, c("25-34", "35-44", "45-54", "55-64")),
    # entidad a los 14 anios; 33 = vivia en el extranjero (sin exposicion asignable)
    cve_14 = ifelse(p19 %in% 1:32, p19, NA_real_),
    extranjero_14 = as.integer(p19 == 33),
    # migrante interestatal: vive hoy en una entidad distinta a la de los 14 anios
    migrante = as.integer(!is.na(cve_14) & cve_14 != entidad),
    # tamano de la localidad a los 14 (p21): 5 = localidad de menos de 2,500 hab.
    origen_rural = as.integer(p21 == 5),
    origen_rural_e2 = as.integer(p21 %in% 3:5),  # definicion de la Entrega 2 (referencia)
    tam_14 = p21,                                 # tamano de localidad a los 14 (1-5)
    # municipio a los 14 (clave INEGI de 4-5 digitos); otros codigos = no respuesta
    mun_14 = suppressWarnings(as.numeric(as.character(haven::zap_labels(p20_COD)))),
    mun_14 = ifelse(mun_14 >= 1001 & mun_14 <= 32999, mun_14, NA_real_),
    # escolaridad en anios completos: se traduce la escala completa (13 niveles y
    # "ninguno" = 0). Una version previa filtraba p9 <= 9 y truncaba la escala.
    ed_yo  = unname(ANIOS_ESC[as.character(num(p9))]),
    ed_pad = pmax(unname(ANIOS_ESC[as.character(num(p42a))]),
                  unname(ANIOS_ESC[as.character(num(p42b))]), na.rm = TRUE)
  )
stopifnot(all(emovi$edad >= 25 & emovi$edad <= 64), all(emovi$factor > 0))
registrar_decision(SCRIPT, "Escolaridad: 13 niveles de p9 a anios completos; 97 (ninguno) = 0",
                   "Entregable 3, 3.4. Padres: maximo de p42a y p42b; 98 (no sabe) = faltante",
                   sum(!is.na(emovi$ed_yo)))
registrar_decision(SCRIPT, "Anio de nacimiento = 2023 - edad",
                   "La base no publica fecha de nacimiento; error posible de +/-1 (robustez en 07)",
                   nrow(emovi))
registrar_decision(SCRIPT, "Origen rural = localidad de menos de 2,500 hab. a los 14 (p21 == 5)",
                   "Definicion INEGI. La Entrega 2 usaba p21 de 3 a 5 (incluia ciudades de 15 mil a 100 mil)",
                   sum(emovi$origen_rural))

emovi <- emovi |>
  mutate(cohorte_exp = case_when(
    nac >= 1997 & nac <= 1998 ~ "A", nac >= 1989 & nac <= 1996 ~ "B",
    nac >= 1980 & nac <= 1988 ~ "C", nac >= 1959 & nac <= 1979 ~ "D"),
    cohorte_exp = factor(cohorte_exp, levels = c("A", "B", "C", "D")))
stopifnot(!anyNA(emovi$cohorte_exp))

# ---- 3. Corresidencia con padres (roster del hogar: p5 == 4, padre o madre) --------
corres <- hog |>
  mutate(p5 = num(p5)) |>
  group_by(index) |>
  summarise(corresidencia = as.integer(any(p5 == 4, na.rm = TRUE)), .groups = "drop")
emovi <- emovi |> left_join(corres, by = "index") |>
  mutate(corresidencia = coalesce(corresidencia, 0L))

# ---- 4. Guardado ----------------------------------------------------------------------
saveRDS(emovi, file.path(RUTAS$clean, "emovi_limpia.rds"))
registrar_n(SCRIPT, "Base limpia (sin exclusiones)", nrow(emovi), nrow(ent))

resumen <- emovi |>
  group_by(cohorte_exp) |>
  summarise(n = n(), poblacion_millones = sum(factor) / 1e6,
            pct_extranjero_14 = 100 * weighted.mean(extranjero_14, factor),
            pct_migrante = 100 * weighted.mean(migrante, factor),
            pct_corresidencia = 100 * weighted.mean(corresidencia, factor),
            pct_corresidencia_sin_ponderar = 100 * mean(corresidencia),
            pct_origen_rural = 100 * weighted.mean(origen_rural, factor)) |>
  left_join(COHORTES |> select(cohorte_exp, etiqueta), by = "cohorte_exp")
guardar_tabla(resumen, "t01_resumen_cohortes")
message("01 terminado.")
