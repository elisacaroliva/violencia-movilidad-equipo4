# =============================================================================
# 00_configuracion.R  (version 5: implementa la metodologia del Entregable 3,
# "Organized-crime violence and intergenerational persistence: evidence and
# bounds from Mexico". El diseno principal es por ENTIDAD; el diseno municipal
# y la combinacion EMOVI 2017 + 2023 son modulos OMITIBLES)
# Paquetes, rutas, parametros del diseno y estilo grafico.
# Cada script lo carga con:  source(here::here("scripts", "00_configuracion.R"))
# Ningun parametro analitico vive escondido en otro script: si cambia una
# decision, se cambia aqui y se anota en DECISIONES.md.
# =============================================================================

# ---- 1. Paquetes ------------------------------------------------------------
paquetes_requeridos <- c(
  "here", "dplyr", "tidyr", "readr", "stringr", "stringi", "purrr", "tibble",
  "haven", "fixest", "survey", "FactoMineR", "cluster", "dbscan", "ggplot2",
  "scales", "sessioninfo", "jsonlite", "ranger"
)
faltantes <- paquetes_requeridos[!vapply(paquetes_requeridos, requireNamespace,
                                         logical(1), quietly = TRUE)]
if (length(faltantes) > 0) {
  stop("Faltan paquetes. Instalalos con:\n  install.packages(c(",
       paste0('"', faltantes, '"', collapse = ", "), "))", call. = FALSE)
}
suppressPackageStartupMessages({
  library(here); library(dplyr); library(tidyr); library(readr)
  library(stringr); library(purrr); library(tibble); library(ggplot2)
})
options(survey.lonely.psu = "adjust", dplyr.summarise.inform = FALSE,
        readr.show_col_types = FALSE, fixest_notes = FALSE)

# ---- 2. Rutas (todas relativas a la raiz del proyecto via here) --------------
RUTAS <- list(
  raw_emovi  = here("data", "raw", "emovi"),
  raw_sesnsp = here("data", "raw", "sesnsp"),
  raw_conapo = here("data", "raw", "conapo"),
  raw_sesnsp_mun = here("data", "raw", "sesnsp_municipal"),   # modulo municipal
  raw_conapo_mun = here("data", "raw", "conapo_municipal"),   # modulo municipal
  raw_emovi17 = here("data", "raw", "emovi_2017"),            # modulo EMOVI 2017
  clean      = here("data", "clean"),
  tablas     = here("output", "tablas"),
  figuras    = here("output", "figuras"),
  logs       = here("output", "logs"),
  config     = here("config")
)
invisible(lapply(RUTAS, dir.create, recursive = TRUE, showWarnings = FALSE))

# ---- 3. Encuesta --------------------------------------------------------------
ANIO_ENCUESTA <- 2023L        # ESRU-EMOVI 2023; anio de nacimiento = 2023 - edad
SEMILLA       <- 20261102L    # semilla unica del proyecto (la que declara el Entregable 3)
COD_NS        <- c(8, 9, 98, 99)   # no sabe / no responde en reactivos de activos

# Reactivos de los indices de recursos (los mismos de la Entrega 2):
# origen = 42 dicotomicos + automoviles (p30); destino = 39 dicotomicos + automoviles (p99)
REACTIVOS_ORIGEN  <- c(paste0("p26", letters[1:5]), paste0("p29", letters[1:7]),
                       paste0("p31", letters[1:15]), paste0("p32", letters[1:15]))
REACTIVOS_DESTINO <- c(paste0("p94", letters[1:7]), paste0("p96", letters[1:18]),
                       paste0("p97", letters[1:14]))
# Escolaridad (p9 = respondente; p42a/p42b = padre/madre): 13 categorias a anios
# completos; 97 = "ninguno" -> 0 anios; 98 = no sabe -> faltante (Entregable 3, 3.4)
ANIOS_ESC <- c(`97` = 0, `1` = 0, `2` = 6, `3` = 9, `4` = 9, `5` = 12, `6` = 12, `7` = 8,
               `8` = 11, `9` = 14, `10` = 11, `11` = 16, `12` = 16, `13` = 18)
AUTOS_ORIGEN  <- "p30"
AUTOS_DESTINO <- "p99"

# Cohortes descriptivas segun la exposicion de su ventana 10-18 al repunte de 2007
# (solo para tablas; el modelo usa efectos fijos por anio de nacimiento)
COHORTES <- tribble(
  ~cohorte_exp, ~nac_min, ~nac_max, ~etiqueta,
  "A", 1997L, 1998L, "A. Ventana 10-18 completa despues de 2007 (1997-1998)",
  "B", 1989L, 1996L, "B. Ventana 10-18 parcialmente despues de 2007 (1989-1996)",
  "C", 1980L, 1988L, "C. Repunte vivido como adultos jovenes, 19-27 (1980-1988): placebo",
  "D", 1959L, 1979L, "D. Ventana y adultez temprana previas al repunte (1959-1979)"
)

# ---- 4. Violencia: serie estatal del SESNSP, metodologia anterior ---------------
ANIOS_PANEL      <- 1997:2017   # unica serie estatal que llega a 1997
ANIOS_INTENSIDAD <- 2007:2017   # intensidad = promedio del indice en estos anios
ANIO_REPUNTE     <- 2007L       # inicio del repunte en la dosis principal
# CONSTRUCCION PRINCIPAL = la del Entregable 3 (seccion 3.3 y nota de la Fig. 1):
#   - indice = promedio de las cuatro tasas estandarizadas;
#   - poblacion interpolada log-lineal entre los censos 2010 y 2020;
#   - robo en carreteras sin registro (1997-2001) contado como cero;
#   - rangos con la regla [cumsum(w) - w/2] / sum(w) sobre el ORDEN del indice.
# Las correcciones de la v4 (CONAPO, robo faltante, empates compartidos, t con
# G - 1 gl) NO se pierden: quedan como especificaciones de robustez con nombre
# propio ("Construccion v4") en las tablas 07, 08 y 04 (ver DECISIONES, D18).

# Ventana formativa y ventana placebo (edades, inclusive)
EDAD_VENTANA     <- c(10L, 18L)
EDAD_VENTANA_ALT <- c(12L, 18L)
EDAD_PLACEBO     <- c(19L, 27L)  # los 9 anios inmediatamente posteriores
NAC_PLACEBO_MAX  <- 1988L        # placebo: nacidos <= 1988 (ventana 10-18 previa a 2007)

# ---- 5. Conglomerados de trayectorias (componente de ciencia de datos) ----------
K_RANGO      <- 2:8              # k evaluados con la silueta de k-medias
K_FIJO       <- 3L               # k de la particion (maximo de la silueta en el Entregable 3)
K_INVARIANZA <- 2:4              # rango de k donde se verifica que el hallazgo no cambia
K_MEDIAS_NSTART <- 200L          # inicios aleatorios de k-medias
B_ESTABILIDAD   <- 500L          # submuestras para la estabilidad de la particion
FRAC_ESTABILIDAD <- 0.80         # fraccion de entidades en cada submuestra
DBSCAN_MINPTS <- 3L
DBSCAN_EPS    <- c(2.5, 3.0, 3.5, 4.0)   # rejilla de radios a nivel entidad (Entregable 3)

# ---- 6. Estimacion e inferencia ------------------------------------------------
# Inferencia principal (Entregable 3): normal, IC = b +/- 1.96 EE y efecto minimo
# detectable = 2.8 x EE (1.96 + 0.84). Toda tabla lleva AL LADO el valor p con
# t de G - 1 gl (correccion de la v4, D15), que es mas conservador.
Z_CRIT    <- qnorm(0.975)
MDE_NORMAL <- 2.8
B_PERMUTACIONES <- 2000L
B_WILD          <- 999L
ALFA            <- 0.05
POTENCIA        <- 0.80
# Grupos de anio de nacimiento del estudio de eventos (los de la presentacion)
EVENTO_CORTES <- c(1958, 1970, 1974, 1978, 1982, 1986, 1990, 1994, 1998)
EVENTO_ETIQ   <- c("<=1970", "1971-74", "1975-78", "1979-82", "1983-86",
                   "1987-90", "1991-94", "1995-98")
EVENTO_REF    <- "1979-82"
EVENTO_PREVIOS   <- c("<=1970", "1971-74", "1975-78", "1983-86")
EVENTO_EXPUESTOS <- c("1987-90", "1991-94", "1995-98")

# Bandas de nacimiento para la pendiente por cohorte (Entregable 3, 4.1)
BANDAS_PENDIENTE <- list(c(1959, 1966), c(1967, 1974), c(1975, 1984), c(1985, 1991), c(1992, 1998))

# ---- 6b. Modulos OMITIBLES ------------------------------------------------------
# El resultado principal es por entidad y no depende de estos modulos. Si el
# interruptor esta en FALSE o falta el insumo, el script correspondiente escribe
# su estado ("omitido") y el pipeline sigue.
EJECUTAR_MUNICIPAL <- TRUE   # 11_diseno_municipal.R (SESNSP municipal + CONAPO municipal)
EJECUTAR_POOL_2017 <- TRUE   # 12_emovi_2017_ciclo_de_vida.R (requiere entrevistado_2017.dta)

# Diseno municipal (Entregable 3, 4.5 y 5.5)
ANIOS_MUN        <- 2011:2017   # la serie municipal del SESNSP empieza en 2011
MUN_MIN_HOM_DBSCAN <- 35        # municipios con >= 35 homicidios 2011-2017 para DBSCAN
DBSCAN_MUN_EPS   <- c(0.8, 1.0, 1.2)
DBSCAN_MUN_MINPTS <- 10L

# ---- 7. Catalogos --------------------------------------------------------------
ENTIDADES    <- read_csv(file.path(RUTAS$config, "entidades.csv"))
MAPA_DELITOS <- read_csv(file.path(RUTAS$config, "mapa_delitos.csv"))

# ---- 8. Estilo grafico
FIGURAS_SIN_TITULO <- TRUE   # titulos y fuentes van en el manuscrito (formato de El Trimestre Economico) ----------------------------------------------------------
COLORES <- c(bajo = "#3f7cac", alto = "#a63a2b", medio = "#c98f2e", neutro = "#7d7d7d")
COLORES_CONG <- c("Repunte temprano" = "#a63a2b", "Repunte tardio" = "#c98f2e",
                  "Sin repunte" = "#3f7cac")
tema_paper <- function(base = 10) {
  theme_minimal(base_size = base) +
    theme(panel.grid.minor = element_blank(),
          plot.title.position = "plot",
          plot.caption = element_text(hjust = 0, size = base * 0.75, colour = "grey30"),
          legend.position = "bottom")
}
FUENTE_EMOVI  <- "Fuente: elaboracion propia con datos de la ESRU-EMOVI 2023 (CEEY)."
FUENTE_SESNSP <- "Fuente: elaboracion propia con datos del SESNSP (incidencia delictiva estatal, metodologia anterior, 1997-2017) y censos 2010 y 2020."
FUENTE_MUN <- "Fuente: elaboracion propia con datos del SESNSP (incidencia municipal, metodologia anterior, 2011-2017), CONAPO (proyecciones municipales 2010-2030) y ESRU-EMOVI 2023."

# ---- 9. Funciones ----------------------------------------------------------------
source(here("scripts", "funciones", "funciones_utiles.R"))
