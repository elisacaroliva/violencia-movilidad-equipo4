# =============================================================================
# 04_conglomerados.R
# Componente de ciencia de datos (Entregable 3, secciones 4.3 y 5.4). La dosis
# supone un calendario comun (repunte en 2007 en todo el pais). Se agrupan las
# TRAYECTORIAS 1997-2017 del indice, estandarizadas dentro de cada entidad (forma,
# no nivel), sin mirar la movilidad. Matriz de rasgos: 32 x 21.
#   - Ward y k-medias con silueta media para k = 2..8 (hiperparametro).
#   - Modelo de referencia: k = 1 (el calendario comun que la dosis ya supone).
#     Se reportan el R2 frente a k = 1, Calinski-Harabasz y Davies-Bouldin.
#   - Validacion: correlacion cofenetica, Rand ajustado Ward vs k-medias y
#     estabilidad en 500 submuestras del 80 % de las entidades.
#   - DBSCAN en una rejilla de radios: a nivel entidad no es informativo (el
#     contraste con municipios esta en 11_diseno_municipal.R).
#   - Invarianza: el grupo temprano y el tardio no cambian en k = 2, 3 y 4.
# Particion principal: indice del Entregable 3. Robustez: indice v4 (robo
# faltante, CONAPO), con el que la estructura se debilita (D13).
# Usos: heterogeneidad legitima (script 10) y dosis con calendario propio (05).
# Insumo : data/clean/panel_violencia.rds
# Salida : data/clean/conglomerados.rds
# =============================================================================
source(here::here("scripts", "00_configuracion.R"))
SCRIPT <- "04_conglomerados"
pv <- readRDS(file.path(RUTAS$clean, "panel_violencia.rds"))

trayectorias <- function(var) {
  w <- pv$panel |> select(cve_ent, anio, valor = all_of(var)) |>
    pivot_wider(names_from = anio, values_from = valor) |> arrange(cve_ent)
  m <- as.matrix(w[, -1]); rownames(m) <- w$cve_ent; m
}
cg    <- conglomerar_trayectorias(trayectorias("indice"))      # principal
cg_v4 <- conglomerar_trayectorias(trayectorias("indice_v4"))   # robustez

asig <- cg$asignacion |> left_join(ENTIDADES, by = "cve_ent")
asig_v4 <- cg_v4$asignacion |> left_join(ENTIDADES, by = "cve_ent")
guardar_tabla(asig, "t04_asignacion_conglomerados")
guardar_tabla(asig_v4, "t04_asignacion_conglomerados_v4")
tam <- asig |> count(nombre, anio_pico, anio_inicio)
guardar_tabla(bind_rows(
  cg$resumen |> mutate(indice = "Principal (Entregable 3)"),
  cg_v4$resumen |> mutate(indice = "v4: robo faltante antes de 2002, CONAPO")),
  "t04_resumen_conglomerados")
# Tabla 4, panel A: seleccion de k (Ward: silueta, R2 vs k = 1, CH, DB; k-medias: silueta)
guardar_tabla(bind_rows(cg$silueta |> mutate(indice = "Principal"),
                        cg_v4$silueta |> mutate(indice = "v4")), "t04_seleccion_k")
guardar_tabla(tibble(replica = seq_along(cg$estabilidad), ari = cg$estabilidad),
              "t04_estabilidad_submuestras")
guardar_tabla(bind_rows(cg$invarianza |> mutate(indice = "Principal"),
                        cg_v4$invarianza |> mutate(indice = "v4")), "t04_invarianza_k")
guardar_tabla(bind_rows(cg$dbscan |> mutate(indice = "Principal"),
                        cg_v4$dbscan |> mutate(indice = "v4")), "t04_dbscan_rejilla")
guardar_tabla(tam, "t04_calendario_por_conglomerado")
guardar_tabla(asig |> select(cve_ent, abrev, principal = nombre) |>
                left_join(asig_v4 |> select(cve_ent, v4 = nombre), by = "cve_ent") |>
                count(principal, v4), "t04_cruce_particiones")
registrar_decision(SCRIPT, "Inicio del repunte por conglomerado = primer anio desde el cual el perfil medio queda arriba de 0 hasta su pico",
                   "Grupo sin repunte: sin inicio (dosis de calendario = 0)", nrow(asig))

# ---- Figura 4 del Entregable 3: (a) dendrograma, (b) silueta, (c) perfiles ----------
png(file.path(RUTAS$figuras, "fig04a_dendrograma_ward.png"), width = 8, height = 4.5,
    units = "in", res = 300)
etq <- ENTIDADES$abrev[match(as.integer(cg$hc$labels), ENTIDADES$cve_ent)]
plot(cg$hc, labels = etq, main = sprintf("(a) Dendrograma de Ward. Correlacion cofenetica = %.3f",
                                          cg$resumen$correlacion_cofenetica),
     xlab = "", sub = "", ylab = "Distancia", cex = 0.7)
rect.hclust(cg$hc, k = K_FIJO, border = "grey40")
dev.off()

sil_larga <- cg$silueta |> select(k, Ward = silueta_ward, `k-medias` = silueta_kmedias) |>
  pivot_longer(-k, names_to = "algoritmo", values_to = "silueta")
g <- ggplot(sil_larga, aes(k, silueta, linetype = algoritmo, shape = algoritmo)) +
  geom_line() + geom_point(size = 2) +
  geom_vline(xintercept = K_FIJO, linetype = 3, colour = COLORES[["alto"]]) +
  labs(x = "Numero de conglomerados (k)", y = "Silueta media", linetype = NULL, shape = NULL,
       title = "(b) Seleccion de k por silueta",
       subtitle = sprintf("Rand ajustado Ward vs k-medias (k = %d): %.3f. Estabilidad (500 x 80 %%): media %.3f, p5 %.3f",
                          K_FIJO, cg$resumen$ari_ward_kmedias, cg$resumen$estabilidad_ari_media,
                          cg$resumen$estabilidad_ari_p5),
       caption = FUENTE_SESNSP) + tema_paper(9)
guardar_figura(g, "fig04b_silueta", 7, 4)

g <- ggplot(cg$perfil, aes(anio, media, colour = nombre, fill = nombre)) +
  geom_ribbon(aes(ymin = media - de, ymax = media + de), alpha = 0.15, colour = NA) +
  geom_line(linewidth = 0.8) + geom_vline(xintercept = ANIO_REPUNTE, linetype = 3) +
  facet_wrap(~ nombre) +
  scale_colour_manual(values = COLORES_CONG, guide = "none") +
  scale_fill_manual(values = COLORES_CONG, guide = "none") +
  labs(x = NULL, y = "Indice estandarizado dentro de la entidad",
       title = "(c) Tres calendarios de la violencia: trayectoria media por conglomerado (Ward)",
       subtitle = "Banda: +/- una desviacion estandar entre entidades del grupo. Linea punteada: 2007.",
       caption = FUENTE_SESNSP) + tema_paper(9)
guardar_figura(g, "fig04c_perfiles_conglomerados", 9, 4)

saveRDS(list(asignacion = asig, resumen = cg$resumen, perfil = cg$perfil,
             seleccion = cg$silueta, invarianza = cg$invarianza, dbscan = cg$dbscan,
             asignacion_v4 = asig_v4, resumen_v4 = cg_v4$resumen),
        file.path(RUTAS$clean, "conglomerados.rds"))
message("04 terminado. ", paste(capture.output(print(tam)), collapse = "\n"))
