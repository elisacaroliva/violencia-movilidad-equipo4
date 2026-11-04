# =============================================================================
# 12_emovi_2017_ciclo_de_vida.R   MODULO OMITIBLE (Entregable 3, 5.3 y 5.7)
# Requiere data/raw/emovi_2017/entrevistado_2017.dta (CEEY lo entrega a peticion).
# Si falta o EJECUTAR_POOL_2017 = FALSE, escribe t12_estado.csv = "omitido".
#
# Armoniza la ESRU-EMOVI 2017 con la 2023 sobre los reactivos comunes (26 de
# origen y 23 de destino, mapeo de pool.py del Entregable 3):
#   - solo reactivos presentes en ambas rondas; si 2017 parte uno en dos se toma
#     el maximo; automoviles binarizados (> 0);
#   - el componente principal se estima con las DOS rondas juntas (mismas cargas);
#   - rangos dentro de ronda x anio de nacimiento.
# Dos usos:
#   1. Ciclo de vida (5.3): pendiente de las mismas cohortes (nacidas 1959-1991)
#      observadas en 2017 y en 2023, total y por tres bandas. Con dos rondas no se
#      separan edad, periodo y cohorte: solo se reporta el signo.
#   2. Potencia (5.7): triple diferencia con las dos rondas juntas y su EMD.
# =============================================================================
source(here::here("scripts", "00_configuracion.R"))
SCRIPT <- "12_emovi_2017_ciclo_de_vida"
f17 <- buscar_archivo(RUTAS$raw_emovi17, "entrevistado.*2017.*\\.dta$", FALSE)
motivo <- if (!EJECUTAR_POOL_2017) "EJECUTAR_POOL_2017 = FALSE en 00_configuracion.R" else
  if (is.na(f17)) "Falta data/raw/emovi_2017/entrevistado_2017.dta (se solicita al CEEY)" else NA
if (!is.na(motivo)) {
  guardar_tabla(tibble(modulo = "EMOVI 2017 + 2023", estado = "omitido", motivo = motivo), "t12_estado")
  message("12 omitido: ", motivo)
} else {

# nombre comun = c(variable 2023, variable(s) 2017)
ORI <- list(agua = c("p26a", "p30_a"), luz = c("p26b", "p30_b"), banio = c("p26c", "p30_c"),
            boiler = c("p26d", "p30_d"), domestico = c("p26e", "p30_e"), estufa = c("p31a", "p33_a"),
            lavadora = c("p31b", "p33_b"), refri = c("p31c", "p33_c"), telfijo = c("p31d", "p33_d"),
            tv = c("p31e", "p33_e"), tostador = c("p31f", "p33_f"), aspiradora = c("p31g", "p33_g"),
            tvcable = c("p31h", "p33_h"), microondas = c("p31i", "p33_i"), celular = c("p31j", "p33_j"),
            computadora = c("p31k", "p33_k"), internet = c("p31l", "p33_l"), dvd = c("p31m", "p33_n"),
            otracasa = c("p32a", "p34_a"), local = c("p32b", "p34_b"),
            terreno = c("p32c", "p34_c", "p34_d"), animales = c("p32d", "p34_g", "p34_h"),
            ahorro = c("p32e", "p32_a"), tdc = c("p32f", "p32_c"), tdepto = c("p32g", "p32_d"),
            auto = c("p30", "p34_e"))
DES <- list(estufa = c("p96a", "p126a"), lavadora = c("p96b", "p126b"), refri = c("p96c", "p126c"),
            microondas = c("p96d", "p126d"), tv = c("p96e", "p126e"), tostador = c("p96f", "p126f"),
            consola = c("p96g", "p126i"), aspiradora = c("p96h", "p126g"), tvpaga = c("p96i", "p126j"),
            telfijo = c("p96j", "p126k"), celular = c("p96k", "p126l"), internet = c("p96l", "p126m"),
            tableta = c("p96m", "p126n"), computadora = c("p96n", "p126o"), agricola = c("p96o", "p126r"),
            animales = c("p96p", "p126p", "p126q"), otracasa = c("p97a", "p129a"),
            local = c("p97b", "p129b"), terreno = c("p97c", "p129d", "p129e"),
            bancaria = c("p97d", "p128b", "p128c"), tdc = c("p97e", "p128d"),
            tdepto = c("p97f", "p128e"), auto = c("p99", "p131"))
CONTEO <- c("p30", "p99", "p131")
binaria <- function(x, nombre) {
  v <- as.numeric(haven::zap_labels(x))
  if (nombre %in% CONTEO) return(ifelse(is.na(v) | v > 90, NA_real_, as.numeric(v > 0)))
  ifelse(v == 1, 1, ifelse(v == 2, 0, NA_real_))
}
bloque <- function(df, mapa, ronda) {
  as_tibble(lapply(mapa, function(v) {
    if (ronda == 2023) return(binaria(df[[v[1]]], v[1]))
    partes <- sapply(v[-1], function(x) binaria(df[[x]], x))
    partes <- matrix(partes, nrow = nrow(df))
    ifelse(rowSums(!is.na(partes)) == 0, NA_real_, apply(partes, 1, max, na.rm = TRUE))
  }))
}
pc1 <- function(X) {
  Z <- apply(as.matrix(X), 2, function(v) { v[is.na(v)] <- mean(v, na.rm = TRUE)
    s <- sd(v); (v - mean(v)) / ifelse(s > 0, s, 1) })
  sv <- svd(scale(Z, scale = FALSE))
  carga <- sv$v[, 1]; pc <- sv$u[, 1] * sv$d[1]
  if (sum(carga) < 0) pc <- -pc
  list(pc = pc, var = sv$d[1]^2 / sum(sv$d^2))
}
d17 <- haven::read_dta(f17)
d23 <- haven::read_dta(buscar_archivo(RUTAS$raw_emovi, "^entrevistado.*\\.dta$", TRUE))
num <- function(x) as.numeric(haven::zap_labels(x))
a <- tibble(ronda = 2017L, edad = num(d17$p05), sexo = num(d17$p06), factor = num(d17$factor),
            cve_14 = num(d17$p23), ent_hoy = num(d17$Estado))
b <- tibble(ronda = 2023L, edad = num(d23$edad), sexo = num(d23$sexo), factor = num(d23$factor),
            cve_14 = num(d23$p19), ent_hoy = num(d23$entidad))
P <- bind_rows(a, b) |> mutate(nac = ronda - edad)
O <- bind_rows(bloque(d17, ORI, 2017), bloque(d23, ORI, 2023))
D <- bind_rows(bloque(d17, DES, 2017), bloque(d23, DES, 2023))
po <- pc1(O); pd <- pc1(D)
P <- P |> mutate(idx_ori = po$pc, idx_des = pd$pc, migrante = as.integer(cve_14 != ent_hoy),
                 celda = paste(ronda, nac))
P$r_ori <- NA_real_; P$r_des <- NA_real_
for (g in unique(P$celda)) {
  i <- which(P$celda == g)
  P$r_ori[i] <- rango_ponderado(P$idx_ori[i], P$factor[i], "orden")
  P$r_des[i] <- rango_ponderado(P$idx_des[i], P$factor[i], "orden")
}
pv <- readRDS(file.path(RUTAS$clean, "panel_violencia.rds"))
base <- readRDS(file.path(RUTAS$clean, "base_modelo.rds"))
int <- base |> filter(muestra_principal) |> distinct(cve_14, int_sd)
P <- P |> filter(cve_14 %in% 1:32, migrante == 0, !is.na(r_ori), !is.na(r_des)) |>
  inner_join(int, by = "cve_14") |>
  mutate(frac = fraccion_ventana(nac, ANIO_REPUNTE), dosis = int_sd * frac)
registrar_n(SCRIPT, "Panel 2017 + 2023 armonizado (no migrantes)", nrow(P))

# ---- 1. Ciclo de vida -----------------------------------------------------------------------
pend <- function(d) unname(coef(lm(r_des ~ r_ori, data = d, weights = factor))[2])
bandas <- list(c(1959, 1991), c(1959, 1969), c(1970, 1980), c(1981, 1991))
ciclo <- map_dfr(bandas, function(r) {
  s17 <- P |> filter(ronda == 2017, nac >= r[1], nac <= r[2])
  s23 <- P |> filter(ronda == 2023, nac >= r[1], nac <= r[2])
  tibble(banda = sprintf("%d-%d", r[1], r[2]), n_2017 = nrow(s17), n_2023 = nrow(s23),
         pendiente_2017 = pend(s17), pendiente_2023 = pend(s23),
         diferencia_2023_menos_2017 = pend(s23) - pend(s17))
})
guardar_tabla(ciclo, "t12_ciclo_de_vida")

# ---- 2. Triple diferencia combinada --------------------------------------------------------
m <- fixest::feols(r_des ~ dosis + dosis:r_ori | cve_14[r_ori] + ronda^nac[r_ori], data = P,
                   weights = ~factor, cluster = ~cve_14, ssc = SSC_PROYECTO)
k <- grep(":", names(coef(m)), value = TRUE)
pb <- pend(P)
comb <- tibble(n = nobs(m), n_2017 = sum(P$ronda == 2017), b3 = unname(coef(m)[k]),
               ee = unname(fixest::se(m)[k]), mde = MDE_NORMAL * ee,
               mde_pct_pendiente = 100 * mde / pb, pendiente_base = pb,
               pct_expuestos_2017 = 100 * mean(P$frac[P$ronda == 2017] > 0),
               frac_max_2017 = max(P$frac[P$ronda == 2017]),
               reactivos_origen = length(ORI), reactivos_destino = length(DES),
               var_pc1_origen = po$var, var_pc1_destino = pd$var)
guardar_tabla(comb, "t12_triple_diferencia_combinada")
guardar_tabla(tibble(modulo = "EMOVI 2017 + 2023", estado = "ok",
                     motivo = sprintf("%s observaciones (%s de 2017)", comb$n, comb$n_2017)), "t12_estado")
message(sprintf("12 terminado. EMD combinado = %.1f %% de la pendiente", comb$mde_pct_pendiente))
}
