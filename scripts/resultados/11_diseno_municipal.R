# =============================================================================
# 11_diseno_municipal.R   MODULO OMITIBLE (Entregable 3, secciones 4.5 y 5.5)
# Variacion municipal DENTRO de la entidad. El resultado principal es por entidad
# y no depende de este script: si EJECUTAR_MUNICIPAL = FALSE o falta un insumo,
# se escribe t11_estado.csv con "omitido" y el pipeline continua.
#
# Construccion (como municipal.py del Entregable 3):
#   Im = log(1 + tasa anual de homicidio doloso por 100 mil), 2011-2017, con la
#        poblacion municipal de CONAPO promediada en 2011-2017; se estandariza
#        entre encuestados.
#   Dm = Im - media de su entidad (variacion puramente intraestatal).
#   dosis_m = Dm x fraccion de la ventana 10-18 desde 2007.
#   b3 se reporta por DE de Dm.
# Tabla 5: (1) conteo en logaritmos (artefacto del denominador), (2) tasa,
# (3) tasa + tamano de localidad x rango, (4) tasa con errores por entidad.
# Ademas: estudio de eventos municipal con y sin controles de tamano (Fig. 3b),
# correlaciones con ruralidad, persistencia del ranking municipal, intento de EF
# entidad x cohorte y DBSCAN sobre municipios (contraste con la escala estatal).
# Insumos : data/raw/sesnsp_municipal/IDM_MAnterior*.csv
#           data/raw/conapo_municipal/*.csv (proyecciones municipales CONAPO)
#           data/clean/base_modelo.rds
# =============================================================================
source(here::here("scripts", "00_configuracion.R"))
SCRIPT <- "11_diseno_municipal"

f_hom <- buscar_archivo(RUTAS$raw_sesnsp_mun, "^IDM.*\\.csv$", FALSE)
f_pob <- buscar_archivo(RUTAS$raw_conapo_mun, "\\.csv$", FALSE)
motivo <- if (!EJECUTAR_MUNICIPAL) "EJECUTAR_MUNICIPAL = FALSE en 00_configuracion.R" else
  if (is.na(f_hom)) "Falta data/raw/sesnsp_municipal/IDM_MAnterior*.csv" else
  if (is.na(f_pob)) "Falta data/raw/conapo_municipal/*.csv" else NA
if (!is.na(motivo)) {
  guardar_tabla(tibble(modulo = "Diseno municipal", estado = "omitido", motivo = motivo),
                "t11_estado")
  message("11 omitido: ", motivo)
} else {

# ---- 1. Homicidios dolosos por municipio y anio ----------------------------------------
mu <- leer_csv_sesnsp(f_hom) |> limpiar_nombres()
col_anio <- c(intersect(c("ano", "anio"), names(mu)), grep("^a[a-z_]{0,3}o$", names(mu), value = TRUE))[1]
stopifnot(!is.na(col_anio), all(c("inegi", "modalidad", "tipo", MESES) %in% names(mu)))
H <- mu |>
  mutate(anio = as.integer(.data[[col_anio]]), mun = as.numeric(inegi),
         modalidad = normalizar_texto(modalidad), tipo = normalizar_texto(tipo),
         across(all_of(MESES), ~ suppressWarnings(as.numeric(gsub(",", "", .x))))) |>
  filter(modalidad == "HOMICIDIOS", tipo == "DOLOSOS", anio %in% ANIOS_MUN)
H$total <- rowSums(H[MESES], na.rm = TRUE)
H <- H |> group_by(mun, anio) |> summarise(hom = sum(total), .groups = "drop") |>
  complete(mun, anio = ANIOS_MUN, fill = list(hom = 0))
stopifnot(nrow(H) > 0)
tot <- H |> group_by(mun) |> summarise(hom_tot = sum(hom), .groups = "drop")
# Persistencia del ranking municipal (sustituto parcial de la prueba previa)
pers <- H |> group_by(mun) |>
  summarise(a = log1p(sum(hom[anio %in% 2011:2013])), b = log1p(sum(hom[anio %in% 2015:2017])),
            .groups = "drop") |> filter(a > 0 | b > 0)

# ---- 2. Poblacion municipal CONAPO, promedio 2011-2017 ---------------------------------
p <- read_csv(f_pob, progress = FALSE, col_types = cols(.default = col_character())) |> limpiar_nombres()
if (all(c("clave", "pob") %in% names(p))) {            # formato directo: CLAVE, pob
  pob <- p |> transmute(mun = as.numeric(clave), pob = as.numeric(pob))
} else {                                                # formato largo de CONAPO (2010-2030)
  c_anio <- c(intersect(c("ano", "anio"), names(p)), grep("^a[a-z_]{0,3}o$", names(p), value = TRUE))[1]
  stopifnot(all(c("cve_mpo", "mpo", "sexo", "valor") %in% names(p)), !is.na(c_anio))
  pob <- p |> filter(normalizar_texto(sexo) == "AMBOS", mpo != "Estado",
                     as.integer(.data[[c_anio]]) %in% ANIOS_MUN) |>
    group_by(mun = as.numeric(cve_mpo), anio = .data[[c_anio]]) |>
    summarise(pob = sum(as.numeric(valor)), .groups = "drop") |>
    group_by(mun) |> summarise(pob = mean(pob), .groups = "drop")
}
stopifnot(nrow(pob) > 2000)

# ---- 3. Muestra municipal ---------------------------------------------------------------
base <- readRDS(file.path(RUTAS$clean, "base_modelo.rds"))
M <- base |> filter(muestra_principal)
Mm <- M |> filter(!is.na(mun_14)) |>
  left_join(tot, by = c("mun_14" = "mun")) |> left_join(pob, by = c("mun_14" = "mun")) |>
  filter(!is.na(hom_tot), !is.na(pob), pob > 0) |>
  mutate(Im = log1p(hom_tot / length(ANIOS_MUN) / pob * 1e5),
         Im_conteo = log1p(hom_tot),
         Im = (Im - mean(Im)) / sd(Im), Im_conteo = (Im_conteo - mean(Im_conteo)) / sd(Im_conteo)) |>
  group_by(cve_14) |>
  mutate(Dm = Im - mean(Im), Dc = Im_conteo - mean(Im_conteo)) |> ungroup() |>
  mutate(dosis_m = Dm * frac, dosis_c = Dc * frac,
         grp = cut(nac, EVENTO_CORTES, labels = EVENTO_ETIQ),
         t2 = as.numeric(tam_14 == 2), t3 = as.numeric(tam_14 == 3),
         t4 = as.numeric(tam_14 == 4), t5 = as.numeric(tam_14 == 5))
registrar_n(SCRIPT, "Muestra municipal (municipio a los 14 valido, con homicidios y poblacion)",
            nrow(Mm), nrow(M))
TAM <- c("t2", "t3", "t4", "t5")
sd_m <- sd(Mm$Dm); sd_c <- sd(Mm$Dc)

# ---- 4. Tabla 5 ------------------------------------------------------------------------------
fila <- function(nombre, ...) estimar_ddd(Mm, ...)$fila |> mutate(especificacion = nombre, .before = 1)
t5 <- bind_rows(
  fila("(1) Dosis intraestatal con conteo", "dosis_c", cluster = "mun_14", escala = sd_c),
  fila("(2) Dosis intraestatal con tasa", "dosis_m", cluster = "mun_14", escala = sd_m),
  fila("(3) Tasa + tamano de localidad x rango", "dosis_m", cluster = "mun_14", escala = sd_m,
       controles = TAM),
  fila("(4) Tasa, errores agrupados por entidad", "dosis_m", cluster = "cve_14", escala = sd_m))
guardar_tabla(t5, "t11_tabla5_municipal")

# EF entidad x cohorte (absorbe choques comunes a una entidad en un momento): se intenta
ec <- tryCatch({
  d <- as.data.frame(Mm); d$D <- d$dosis_m
  m <- fixest::feols(r_des ~ D + D:r_ori | cve_14^grp + cve_14^grp[[r_ori]] + nac[r_ori], data = d,
                     weights = ~factor, cluster = ~mun_14, ssc = SSC_PROYECTO)
  k <- grep(":", names(coef(m)), value = TRUE)
  tibble(estado = "estimado", b3 = sd_m * unname(coef(m)[k]), ee = sd_m * unname(fixest::se(m)[k]),
         coeficientes_eliminados = length(m$collin.var),
         nota = "fixest absorbe las celdas entidad x grupo de nacimiento; el Entregable 3 lo declaraba no estimable (ver DECISIONES D23)")
}, error = function(e) tibble(estado = "no estimable", nota = conditionMessage(e)))
guardar_tabla(ec, "t11_ef_entidad_x_cohorte")

# ---- 5. Diagnosticos del denominador ----------------------------------------------------------
diag <- tibble(
  personas = nrow(Mm), municipios = n_distinct(Mm$mun_14), entidades = n_distinct(Mm$cve_14),
  de_total_Im = sd(Mm$Im), de_intraestatal_Dm = sd_m,
  cor_rural_conteo_niveles = cor(Mm$Im_conteo, Mm$origen_rural),
  cor_rural_tasa_niveles = cor(Mm$Im, Mm$origen_rural),
  cor_rural_conteo_intra = cor(Mm$Dc, Mm$origen_rural),
  cor_rural_tasa_intra = cor(Mm$Dm, Mm$origen_rural),
  persistencia_ranking_2011_13_vs_2015_17 = cor(pers$a, pers$b),
  municipios_persistencia = nrow(pers),
  personas_por_municipio_mediana = median(table(Mm$mun_14)),
  pendiente_base = t5$pendiente_base[2], mde_pct_pendiente_tasa = t5$mde_pct_pendiente[2])
guardar_tabla(diag, "t11_diagnosticos")

# ---- 6. Estudio de eventos municipal (Fig. 3b) --------------------------------------------------
otros <- setdiff(EVENTO_ETIQ, EVENTO_REF)
id <- setNames(paste0("g", match(otros, EVENTO_ETIQ)), otros)
for (g in otros) {
  Mm[[paste0("ev_", id[g])]] <- as.numeric(Mm$grp == g) * Mm$r_ori * Mm$Dm
  Mm[[paste0("nv_", id[g])]] <- as.numeric(Mm$grp == g) * Mm$Dm
}
evento_m <- function(variable, controles) {
  for (g in otros) {
    Mm[[paste0("ev_", id[g])]] <- as.numeric(Mm$grp == g) * Mm$r_ori * Mm[[variable]]
    Mm[[paste0("nv_", id[g])]] <- as.numeric(Mm$grp == g) * Mm[[variable]]
  }
  ctrl <- if (controles) paste("+", paste(c(TAM, paste0(TAM, ":r_ori")), collapse = " + ")) else ""
  f <- as.formula(paste("r_des ~", paste(c(paste0("ev_", id), paste0("nv_", id)), collapse = " + "),
                        ctrl, "| cve_14[r_ori] + nac[r_ori]"))
  m <- fixest::feols(f, data = Mm, weights = ~factor, cluster = ~mun_14, ssc = SSC_PROYECTO)
  ct <- fixest::coeftable(m); k <- sd(Mm[[variable]])
  w <- fixest::wald(m, keep = paste0("^ev_(", paste(id[EVENTO_PREVIOS], collapse = "|"), ")$"),
                    print = FALSE)
  tibble(grupo = otros, b = k * ct[paste0("ev_", id), 1], ee = k * ct[paste0("ev_", id), 2]) |>
    bind_rows(tibble(grupo = EVENTO_REF, b = 0, ee = 0)) |>
    mutate(grupo = factor(grupo, levels = EVENTO_ETIQ), t = ifelse(ee > 0, b / ee, NA),
           tipo = ifelse(grupo %in% EVENTO_EXPUESTOS, "Ventana con repunte (expuestos)",
                         "Ventana previa al repunte"),
           medida = ifelse(variable == "Dm", "tasa", "conteo"), controles_tamano = controles,
           F_previos = w$stat, p_previos = w$p) |> arrange(grupo)
}
ev <- bind_rows(evento_m("Dm", TRUE), evento_m("Dm", FALSE), evento_m("Dc", FALSE))
guardar_tabla(ev, "t11_estudio_eventos_municipal")
guardar_tabla(ev |> distinct(medida, controles_tamano, F_previos, p_previos),
              "t11_prueba_tendencias_previas_municipal")
e1 <- ev |> filter(medida == "tasa", controles_tamano)
g <- ggplot(e1, aes(grupo, b, colour = tipo)) +
  annotate("rect", xmin = 5.5, xmax = 8.5, ymin = -Inf, ymax = Inf, fill = "#f5e9e6") +
  geom_hline(yintercept = 0, colour = "grey50") +
  geom_errorbar(aes(ymin = b - Z_CRIT * ee, ymax = b + Z_CRIT * ee), width = 0.15) +
  geom_point(size = 2) +
  scale_colour_manual(values = c("Ventana previa al repunte" = COLORES[["bajo"]],
                                 "Ventana con repunte (expuestos)" = COLORES[["alto"]]), name = NULL) +
  labs(x = "Grupo de año de nacimiento (referencia: 1979-82)",
       y = "Cambio en la pendiente por DE\nde la desviación intraestatal",
       title = sprintf("(b) Estudio de eventos, diseno municipal (tasa, con tamano de localidad). Previos: p = %.2f",
                       e1$p_previos[1]),
       subtitle = "Barras: IC 95 % con errores agrupados por municipio.", caption = FUENTE_MUN) +
  tema_paper(9)
guardar_figura(g, "fig03b_estudio_eventos_municipal", 8, 4.5)

# ---- 7. DBSCAN sobre trayectorias municipales ---------------------------------------------------
W <- H |> pivot_wider(names_from = anio, values_from = hom) |> arrange(mun)
Xm <- as.matrix(W[, as.character(ANIOS_MUN)]); rownames(Xm) <- W$mun
Xm <- Xm[rowSums(Xm) >= MUN_MIN_HOM_DBSCAN, , drop = FALSE]
Xm <- log1p(Xm); s_ <- apply(Xm, 1, sd); Xm <- Xm[s_ > 0, , drop = FALSE]
Xm <- t(apply(Xm, 1, function(x) (x - mean(x)) / sd(x)))
dbm <- map_dfr(DBSCAN_MUN_EPS, function(e) {
  cl <- dbscan::dbscan(Xm, eps = e, minPts = DBSCAN_MUN_MINPTS)$cluster
  tibble(eps = e, minPts = DBSCAN_MUN_MINPTS, municipios = nrow(Xm),
         grupos = length(setdiff(unique(cl), 0)), pct_ruido = 100 * mean(cl == 0))
})
guardar_tabla(dbm, "t11_dbscan_municipal")
guardar_tabla(tibble(modulo = "Diseno municipal", estado = "ok",
                     motivo = sprintf("%s personas en %s municipios", nrow(Mm), n_distinct(Mm$mun_14))),
              "t11_estado")
p2 <- t5[2, ]
message(sprintf("11 terminado. Municipal (tasa): b3 = %+.4f (EE %.4f); EMD = %.1f %% de la pendiente",
                p2$b3, p2$ee, p2$mde_pct_pendiente))
}
