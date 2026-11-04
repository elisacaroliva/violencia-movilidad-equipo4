# =============================================================================
# 08_inferencia.R
# Lamina "La inferencia": con 31 entidades el error agrupado de formula no basta.
#   1. Permutacion: se reasigna al azar, entre entidades, la intensidad (y, en la
#      dosis de calendario, tambien el anio de inicio del repunte), se reconstruye
#      la dosis conservando su componente de cohorte y se recalcula b3. 2,000 veces.
#   2. Bootstrap salvaje por conglomerados, restringido, pesos de Webb
#      (Cameron, Gelbach y Miller, 2008), 999 replicas.
# Correccion del matiz 2: la distribucion nula y el intervalo analitico se
# reportan en la MISMA unidad (por DE de intensidad y, al lado, en unidades crudas).
# Insumo : data/clean/base_modelo.rds
# =============================================================================
source(here::here("scripts", "00_configuracion.R"))
SCRIPT <- "08_inferencia"
base <- readRDS(file.path(RUTAS$clean, "base_modelo.rds"))
M <- base |> filter(muestra_principal) |> as.data.frame()
ents <- sort(unique(M$cve_14))
de_int <- read_csv(file.path(RUTAS$tablas, "t05_de_intensidad.csv"))$de_entre_entidades[1]
E <- M |> distinct(cve_14, int_sd, int_hom_sd, anio_inicio, anio_inicio_v4) |> arrange(cve_14)
stopifnot(nrow(E) == length(ents), identical(as.numeric(E$cve_14), as.numeric(ents)))
pos <- match(M$cve_14, ents)

dosis_perm <- list(
  dosis = function(perm) E$int_sd[match(perm, ents)][pos] * M$frac,
  dosis_hom = function(perm) E$int_hom_sd[match(perm, ents)][pos] * M$frac,
  dosis_calendario = function(perm) {
    j <- match(perm, ents)[pos]
    ini <- E$anio_inicio[j]
    ifelse(is.na(ini), 0, E$int_sd[j] * fraccion_ventana(M$nac, ini))
  },
  dosis_calendario_v4 = function(perm) {
    j <- match(perm, ents)[pos]
    ini <- E$anio_inicio_v4[j]
    ifelse(is.na(ini), 0, E$int_sd[j] * fraccion_ventana(M$nac, ini))
  })
etiquetas <- c(dosis = "Principal", dosis_hom = "Solo homicidio doloso",
               dosis_calendario = "Calendario propio por conglomerado",
               dosis_calendario_v4 = "Calendario por conglomerado (particion v4)")

resultados <- map(names(dosis_perm), function(v) {
  stopifnot(isTRUE(all.equal(dosis_perm[[v]](ents), M[[v]])))   # identidad = dosis observada
  pm <- permutacion_ddd(M, M[[v]], dosis_perm[[v]], ents)
  wb <- wild_cluster_boot(M, M[[v]])
  fc <- estimar_ddd(M, v)$fila
  list(tabla = tibble(especificacion = etiquetas[[v]], dosis = v, n = nrow(M),
                      entidades = length(ents), unidad = "por DE de intensidad",
                      b3 = fc$b3, ee_agrupado = fc$ee, p_normal = fc$p_normal,
                      p_agrupado_t30 = fc$p_t,
                      ic_analitico_inf = fc$ic_inf, ic_analitico_sup = fc$ic_sup,
                      p_permutacion = pm$p, nulo_p2.5 = pm$nulo_p2.5, nulo_p97.5 = pm$nulo_p97.5,
                      p_wild = wb$p_wild, B_perm = B_PERMUTACIONES, B_wild = B_WILD),
       nulos = pm$nulos)
})
tab <- map_dfr(resultados, "tabla")
# Misma informacion en unidades crudas del indice (solo dosis con la intensidad principal)
crudo <- tab |> filter(dosis %in% c("dosis", "dosis_calendario", "dosis_calendario_v4")) |>
  mutate(unidad = "por unidad cruda del indice",
         across(c(b3, ee_agrupado, ic_analitico_inf, ic_analitico_sup, nulo_p2.5, nulo_p97.5),
                ~ .x / de_int))
guardar_tabla(bind_rows(tab, crudo), "t08_inferencia")

r1 <- resultados[[1]]
g <- ggplot(tibble(b = r1$nulos), aes(b)) +
  geom_histogram(bins = 50, fill = "grey75", colour = "white") +
  geom_vline(xintercept = r1$tabla$b3, colour = COLORES[["alto"]], linewidth = 0.8) +
  geom_vline(xintercept = c(r1$tabla$ic_analitico_inf, r1$tabla$ic_analitico_sup),
             linetype = 2, colour = COLORES[["bajo"]]) +
  labs(x = "b3 (por DE de intensidad) bajo reasignaciones al azar de la intensidad entre entidades",
       y = "Permutaciones",
       title = sprintf("Inferencia por permutacion: p = %.2f; bootstrap salvaje: p = %.2f",
                       r1$tabla$p_permutacion, r1$tabla$p_wild),
       subtitle = sprintf("Rojo: b3 observado. Punteadas: IC 95 %% analitico (normal) [%.3f, %.3f]. Nulo 2.5-97.5 %%: [%.3f, %.3f].",
                          r1$tabla$ic_analitico_inf, r1$tabla$ic_analitico_sup,
                          r1$tabla$nulo_p2.5, r1$tabla$nulo_p97.5),
       caption = paste(FUENTE_EMOVI, FUENTE_SESNSP, sep = "\n")) + tema_paper(9)
guardar_figura(g, "fig09_permutacion", 8, 4.2)
message("08 terminado. p permutacion = ", round(r1$tabla$p_permutacion, 3),
        "; p wild = ", round(r1$tabla$p_wild, 3))
