# =============================================================================
# 07_modelo_principal.R
# Triple diferencia sobre la pendiente rango-rango (Entregable 3, seccion 4.2):
#
#   R_des = b1 R_ori + b2 D + b3 (R_ori x D) + EF_ent + EF_nac
#           + (EF_ent x R_ori) + (EF_nac x R_ori) + e
#
# D = intensidad 2007-2017 de la entidad (en DE) x fraccion de la ventana 10-18
# posterior a 2007. b3 = cambio en la pendiente por DE de intensidad.
# MCP con factor de expansion; errores agrupados por entidad.
# Inferencia principal (Entregable 3): normal, IC +/- 1.96 EE, EMD = 2.8 EE.
# Al lado, p e IC con t de G - 1 gl (correccion v4).
#
# Salidas:
#   t07_tabla2_principal      Tabla 2: (1) base, (2) sin corresidentes,
#                             (3) con migrantes, (4) solo homicidio,
#                             (5) agrupando por anio de nacimiento
#   t07_tabla3_capas          Tabla 3: lo que quita cada bloque de controles
#   t07_robustez              Construccion v4 y demas pruebas
#   t07_potencia              EMD en las dos escalas (5.7)
# Insumo : data/clean/base_modelo.rds
# =============================================================================
source(here::here("scripts", "00_configuracion.R"))
SCRIPT <- "07_modelo_principal"
base <- readRDS(file.path(RUTAS$clean, "base_modelo.rds"))
M <- base |> filter(muestra_principal)
de_tab <- read_csv(file.path(RUTAS$tablas, "t05_de_intensidad.csv"))
de_int <- de_tab$de_entre_entidades[1]
pend_base <- unname(coef(lm(r_des ~ r_ori, data = M, weights = factor))[2])
int_ent <- M |> distinct(cve_14, int_sd)
brecha_90_10 <- unname(diff(quantile(int_ent$int_sd, c(0.1, 0.9))))
con_migr <- base |> filter(en_pais, con_rangos)

fila <- function(nombre, d, dosis = "dosis", ...) {
  estimar_ddd(d, dosis, ...)$fila |> mutate(especificacion = nombre, dosis = dosis, .before = 1)
}

# ---- Tabla 2 --------------------------------------------------------------------------
t2 <- bind_rows(
  fila("(1) Base", M),
  fila("(2) Sin quienes viven con sus padres", M |> filter(corresidencia == 0)),
  fila("(3) Con migrantes interestatales", con_migr),
  fila("(4) Dosis solo con homicidio doloso", M, "dosis_hom"),
  fila("(5) Errores agrupados por anio de nacimiento", M, cluster = "nac")) |>
  mutate(pct_de_pendiente = 100 * b3 / pend_base, b3_crudo = b3 / de_int, .after = b3)
guardar_tabla(t2, "t07_tabla2_principal")

# ---- Tabla 3: el camino de especificacion ----------------------------------------------
t3 <- bind_rows(
  fila("(1) Sin controles", M, capas = "sin_controles"),
  fila("(2) + EF de entidad y de anio de nacimiento", M, capas = "ef"),
  fila("(3) + entidad x rango de origen", M, capas = "ef_ent_x_rango"),
  fila("(4) + anio de nacimiento x rango de origen (modelo completo)", M))
guardar_tabla(t3, "t07_tabla3_capas")

# ---- Robustez: construccion v4 y demas -------------------------------------------------
v4_completa <- M |> mutate(r_ori = r_ori_v4, r_des = r_des_v4)
rob <- bind_rows(
  fila("Construccion v4 completa (CONAPO, robo faltante, empates compartidos)", v4_completa, "dosis_v4"),
  fila("Poblacion CONAPO en vez de censal", M, "dosis_conapo"),
  fila("Robo en carreteras 1997-2001 como faltante", M, "dosis_faltante"),
  fila("Rangos con empates compartidos", v4_completa),
  fila("Indice = primer componente principal", M, "dosis_pc1"),
  fila("Calendario propio por conglomerado (particion principal)", M, "dosis_calendario"),
  fila("Calendario por conglomerado (particion v4)", M, "dosis_calendario_v4"),
  fila("Ventana de 12 a 18 anios", M, "dosis_ventana_alt"),
  fila("Anio de nacimiento = 2023 - edad - 1", M, "dosis_nac_menos1"),
  fila("Indice de recursos por ACM dentro de cohorte", M |> mutate(r_ori = r_ori_acm, r_des = r_des_acm)),
  fila("Destino medido por ingreso per capita (CEEY)", M |> mutate(r_des = r_des_ing))) |>
  mutate(pct_de_pendiente = 100 * b3 / pend_base, efecto_p90_vs_p10 = b3 * brecha_90_10,
         .after = b3)
guardar_tabla(rob, "t07_robustez")

# ---- Potencia (5.7) -----------------------------------------------------------------------
p1 <- t2[1, ]
pot <- tibble(escala = c("Por DE de intensidad (reportada)", "Por unidad de dosis (cruda)"),
              ee = c(p1$ee, p1$ee / de_int), mde_2.8 = MDE_NORMAL * ee,
              mde_pct_pendiente = 100 * mde_2.8 / pend_base,
              mde_t30 = c(p1$mde_t, p1$mde_t / de_int),
              mde_t30_pct_pendiente = 100 * mde_t30 / pend_base,
              pendiente_base = pend_base, de_intensidad = de_int,
              brecha_int_p90_p10_en_de = brecha_90_10)
guardar_tabla(pot, "t07_potencia")

g <- bind_rows(t2 |> mutate(grupo = "Tabla 2"), rob |> mutate(grupo = "Robustez")) |>
  mutate(especificacion = factor(especificacion, levels = rev(especificacion))) |>
  ggplot(aes(b3, especificacion, colour = grupo)) +
  geom_vline(xintercept = 0, colour = "grey50") +
  geom_errorbar(aes(xmin = ic_inf, xmax = ic_sup), width = 0.2) + geom_point(size = 2) +
  scale_colour_manual(values = c("Tabla 2" = COLORES[["alto"]], "Robustez" = COLORES[["bajo"]]),
                      name = NULL) +
  labs(x = "b3: cambio en la pendiente por DE de intensidad (IC 95 %)", y = NULL,
       title = "Resultado principal y robustez",
       subtitle = sprintf("Pendiente base = %.3f. EMD (80 %% de potencia) = %.3f = %.0f %% de la pendiente.",
                          pend_base, p1$mde, p1$mde_pct_pendiente),
       caption = paste(FUENTE_EMOVI, FUENTE_SESNSP, sep = "\n")) + tema_paper(9)
guardar_figura(g, "fig08_resultado_y_robustez", 9, 6)
message(sprintf("07 terminado. b3 = %+.4f (EE %.4f, p normal = %.2f), EMD = %.4f (%.1f %% de %.3f)",
                p1$b3, p1$ee, p1$p_normal, p1$mde, p1$mde_pct_pendiente, pend_base))
