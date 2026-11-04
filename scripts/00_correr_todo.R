# =============================================================================
# 00_correr_todo.R
# Ejecuta el pipeline completo en orden. Abrir antes Equipo_4_v5.Rproj (fija la
# raiz del proyecto para here::here()). Version 5. Prueba de reproducibilidad: se pueden
# borrar data/clean/ y output/ y este script los reconstruye sin intervencion.
# =============================================================================
source(here::here("scripts", "00_configuracion.R"))
unlink(file.path(RUTAS$logs, c("log_n.csv", "log_decisiones_codigo.csv")))

pasos <- c(
  "scripts/limpieza/01_limpieza_emovi.R",
  "scripts/limpieza/02_indices_recursos.R",
  "scripts/limpieza/03_panel_violencia.R",
  "scripts/limpieza/04_conglomerados.R",
  "scripts/limpieza/05_dosis_y_muestra.R",
  "scripts/resultados/06_descriptivos.R",
  "scripts/resultados/07_modelo_principal.R",
  "scripts/resultados/08_inferencia.R",
  "scripts/resultados/09_supuesto_y_placebo.R",
  "scripts/resultados/10_heterogeneidad.R",
  "scripts/resultados/11_diseno_municipal.R",        # OMITIBLE (EJECUTAR_MUNICIPAL)
  "scripts/resultados/12_emovi_2017_ciclo_de_vida.R", # OMITIBLE (EJECUTAR_POOL_2017)
  "scripts/resultados/13_verificacion_entregable3.R"
)

tiempos <- tibble(script = character(), segundos = numeric(), estado = character())
for (p in pasos) {
  message("\n==================== ", p, " ====================")
  t0 <- Sys.time()
  estado <- tryCatch({ source(here::here(p), local = new.env()); "ok" },
                     error = function(e) { message("ERROR en ", p, ": ", conditionMessage(e))
                       paste("error:", conditionMessage(e)) })
  tiempos <- add_row(tiempos, script = p, estado = estado,
                     segundos = round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1))
  if (estado != "ok") break      # no se sigue con insumos incompletos
}
write_csv(tiempos, file.path(RUTAS$logs, "tiempos_y_estado.csv"))
writeLines(capture.output(sessioninfo::session_info()),
           file.path(RUTAS$logs, "session_info.txt"))
print(tiempos)
