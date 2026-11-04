# =============================================================================
# 09_supuesto_y_placebo.R
# 1. Estudio de eventos (lamina "El supuesto, y su prueba"). Para cada grupo de
#    anio de nacimiento se estima la interaccion rango de origen x intensidad de
#    la entidad (en DE), con 1979-82 como referencia y con TODOS los efectos fijos
#    del modelo, incluida la pendiente propia de cada anio de nacimiento
#    (correccion del matiz 1: el modelo.py que circulo omitia C x r_ori y con ello
#    la prueba de tendencias previas falla). La version sin ese termino se
#    reporta solo como diagnostico.
#    Supuesto: los grupos cuya ventana 10-18 termino antes de 2007 deben dar cero.
# 2. Placebo con adultos jovenes (no estaba en la presentacion; la minuta lo pide
#    y es la prueba directa del "periodo formativo"): en los nacidos en 1988 o
#    antes (ventana 10-18 previa a 2007) se usa como "dosis" la intensidad x la
#    fraccion de los 19 a 27 anios vivida desde 2007. Debe dar b3 = 0.
# Insumo : data/clean/base_modelo.rds
# =============================================================================
source(here::here("scripts", "00_configuracion.R"))
SCRIPT <- "09_supuesto_y_placebo"
base <- readRDS(file.path(RUTAS$clean, "base_modelo.rds"))
M <- base |> filter(muestra_principal) |>
  mutate(grp = cut(nac, EVENTO_CORTES, labels = EVENTO_ETIQ))
stopifnot(!anyNA(M$grp))

# ---- 1. Estudio de eventos ----------------------------------------------------------------
otros <- setdiff(EVENTO_ETIQ, EVENTO_REF)
id <- setNames(paste0("g", match(otros, EVENTO_ETIQ)), otros)   # nombres sin simbolos
for (g in otros) {
  M[[paste0("ev_", id[g])]] <- as.numeric(M$grp == g) * M$r_ori * M$int_sd
  M[[paste0("nv_", id[g])]] <- as.numeric(M$grp == g) * M$int_sd
}
rhs <- paste(c(paste0("ev_", id), paste0("nv_", id)), collapse = " + ")
G <- length(unique(M$cve_14))
evento <- function(con_pendiente_nac) {
  fe <- if (con_pendiente_nac) "cve_14[r_ori] + nac[r_ori]" else "cve_14[r_ori] + nac"
  m <- fixest::feols(as.formula(paste("r_des ~", rhs, "|", fe)), data = M,
                     weights = ~factor, cluster = ~cve_14, ssc = SSC_PROYECTO)
  ct <- fixest::coeftable(m)
  w <- fixest::wald(m, keep = paste0("^ev_(", paste(id[EVENTO_PREVIOS], collapse = "|"), ")$"),
                    print = FALSE)
  ev <- tibble(grupo = otros, b = ct[paste0("ev_", id), 1], ee = ct[paste0("ev_", id), 2]) |>
    bind_rows(tibble(grupo = EVENTO_REF, b = 0, ee = 0)) |>
    mutate(grupo = factor(grupo, levels = EVENTO_ETIQ), t = ifelse(ee > 0, b / ee, NA),
           tc = Z_CRIT,
           tipo = ifelse(grupo %in% EVENTO_EXPUESTOS, "Ventana con repunte (expuestos)",
                         "Ventana previa al repunte"),
           version = ifelse(con_pendiente_nac, "Con EF nacimiento x origen (principal)",
                            "Sin EF nacimiento x origen (diagnostico, modelo.py)")) |>
    arrange(grupo)
  prueba <- tibble(version = ev$version[1], F = w$stat, p = w$p, gl1 = w$df1, gl2 = w$df2,
                   t_max_previos = max(abs(ev$t[ev$grupo %in% EVENTO_PREVIOS])),
                   t_max_expuestos = max(abs(ev$t[ev$grupo %in% EVENTO_EXPUESTOS])),
                   grupos_previos = paste(EVENTO_PREVIOS, collapse = ", "))
  list(ev = ev, prueba = prueba)
}
e1 <- evento(TRUE); e0 <- evento(FALSE)
guardar_tabla(bind_rows(e1$ev, e0$ev), "t09_estudio_eventos")
guardar_tabla(bind_rows(e1$prueba, e0$prueba), "t09_prueba_tendencias_previas")

g <- ggplot(e1$ev, aes(grupo, b, colour = tipo)) +
  annotate("rect", xmin = 5.5, xmax = 8.5, ymin = -Inf, ymax = Inf, fill = "#f5e9e6") +
  geom_hline(yintercept = 0, colour = "grey50") +
  geom_errorbar(aes(ymin = b - tc * ee, ymax = b + tc * ee), width = 0.15) +
  geom_point(size = 2) +
  scale_colour_manual(values = c("Ventana previa al repunte" = COLORES[["bajo"]],
                                 "Ventana con repunte (expuestos)" = COLORES[["alto"]]), name = NULL) +
  labs(x = "Grupo de año de nacimiento (referencia: 1979-82)",
       y = "Cambio en la pendiente origen-destino\npor DE de intensidad",
       title = sprintf("(a) Estudio de eventos, entidad. Prueba conjunta de los grupos previos: F = %.2f, p = %.2f",
                       e1$prueba$F, e1$prueba$p),
       subtitle = "Barras: IC 95 % (normal) con errores agrupados por entidad. Panel (a): diseno estatal.",
       caption = paste(FUENTE_EMOVI, FUENTE_SESNSP, sep = "\n")) + tema_paper(9)
guardar_figura(g, "fig10_estudio_eventos", 8, 4.5)

# ---- 2. Placebo: adultos jovenes ---------------------------------------------------------------
P <- M |> filter(nac <= NAC_PLACEBO_MAX)
stopifnot(all(P$dosis == 0))
pl <- estimar_ddd(P, "dosis_placebo")$fila |>
  mutate(especificacion = "Placebo: intensidad x fraccion de los 19-27 anios desde 2007, nacidos 1988 o antes",
         .before = 1)
guardar_tabla(pl, "t09_placebo_adultos_jovenes")
message(sprintf("09 terminado. Tendencias previas: F = %.2f, p = %.2f. Placebo b3 = %+.4f (EE %.4f)",
                e1$prueba$F, e1$prueba$p, pl$b3, pl$ee))
