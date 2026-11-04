# =============================================================================
# 13_verificacion_entregable3.R
# Compara, cifra por cifra, lo que obtiene la v5 con lo que reporta el
# Entregable 3 (config/objetivos_entregable3.csv). Cada fila dice:
#   coincide    : dentro de la tolerancia
#   difiere     : fuera de la tolerancia (se explica en DECISIONES, seccion F)
#   omitido     : el modulo opcional no se corrio (municipal o EMOVI 2017)
# Tolerancias: redondeo de la cifra publicada; para permutacion y submuestreo,
# 3 errores Monte Carlo (la semilla de Python no se reproduce en R).
# Salida: output/tablas/t13_verificacion_entregable3.csv
# =============================================================================
source(here::here("scripts", "00_configuracion.R"))
SCRIPT <- "13_verificacion_entregable3"
T_ <- function(x) { f <- file.path(RUTAS$tablas, paste0(x, ".csv"))
  if (file.exists(f)) read_csv(f) else NULL }
val <- function(d, col, cond = TRUE) { if (is.null(d)) return(NA_real_)
  x <- d[[col]][cond]; if (length(x) == 0) NA_real_ else as.numeric(x[1]) }

t2 <- T_("t07_tabla2_principal"); t3 <- T_("t07_tabla3_capas"); pot <- T_("t07_potencia")
t1 <- T_("t06_tabla1_descriptivos"); ex <- T_("t06_expuestos"); ce <- T_("t06_correlacion_entidades")
pr <- T_("t06_pendiente_rango_rango"); val2 <- T_("t02_validacion_indices")
de <- T_("t05_de_intensidad"); inf <- T_("t08_inferencia"); tp <- T_("t09_prueba_tendencias_previas")
rc <- T_("t04_resumen_conglomerados"); sk <- T_("t04_seleccion_k"); cal <- T_("t04_calendario_por_conglomerado")
cp <- T_("t10_conglomerados_permutacion"); t6 <- T_("t10_tabla6_heterogeneidad")
tc <- T_("t10_tipo_de_cambio_transiciones")
m5 <- T_("t11_tabla5_municipal"); md <- T_("t11_diagnosticos"); mp <- T_("t11_prueba_tendencias_previas_municipal")
mdb <- T_("t11_dbscan_municipal"); pc <- T_("t12_triple_diferencia_combinada"); cv <- T_("t12_ciclo_de_vida")

r1 <- !is.null(rc) & rc$indice == "Principal (Entregable 3)"
skp <- if (!is.null(sk)) sk$indice == "Principal" else NULL
cpp <- function(g, col) val(cp, col, cp$particion == "Principal (Entregable 3)" & cp$conglomerado == g)
b6 <- function(g, col = "b3") val(t6, col, t6$grupo == g)
banda <- function(g) val(pr, "pendiente", pr$muestra == "Analitica, bandas del Entregable 3 (4.1)" & pr$grupo == g)
t1v <- function(v, col = "media") val(t1, col, t1$variable == v)

obtenido <- c(
  n_principal = val(t2, "n", 1), n_sin_corresidentes = val(t2, "n", 2), n_con_migrantes = val(t2, "n", 3),
  entidades = val(de, "entidades", 1), expuestos = val(ex, "expuestos"), pct_expuestos = val(ex, "pct_expuestos"),
  var_pc1_origen_pct = val(val2, "valor", val2$indicador == "Varianza del primer componente, origen (%)"),
  var_pc1_destino_pct = val(val2, "valor", val2$indicador == "Varianza del primer componente, destino (%)"),
  de_intensidad = val(de, "de_entre_entidades", 1), pendiente_base = val(pot, "pendiente_base", 1),
  t1_media_r_ori = t1v("r_ori"), t1_media_ed_yo = t1v("ed_yo"), t1_media_ed_pad = t1v("ed_pad"),
  t1_n_ed_pad = t1v("ed_pad", "n"), t1_media_intensidad = t1v("int_crudo"), t1_media_dosis = t1v("dosis_cruda"),
  t1_media_rural = t1v("origen_rural"), t1_media_corresidencia = t1v("corresidencia"),
  corr_entidades = val(ce, "r", 1), corr_entidades_ic_inf = val(ce, "ic_inf", 1),
  corr_entidades_ic_sup = val(ce, "ic_sup", 1), corr_entidades_sin_chica = val(ce, "r", 2),
  pend_1959_1966 = banda("Nacidos 1959-1966"), pend_1967_1974 = banda("Nacidos 1967-1974"),
  pend_1975_1984 = banda("Nacidos 1975-1984"), pend_1985_1991 = banda("Nacidos 1985-1991"),
  pend_1992_1998 = banda("Nacidos 1992-1998"),
  b3_base = val(t2, "b3", 1), ee_base = val(t2, "ee", 1),
  b3_sin_corresidentes = val(t2, "b3", 2), ee_sin_corresidentes = val(t2, "ee", 2),
  b3_con_migrantes = val(t2, "b3", 3), ee_con_migrantes = val(t2, "ee", 3),
  b3_solo_homicidio = val(t2, "b3", 4), ee_solo_homicidio = val(t2, "ee", 4),
  ee_cluster_nacimiento = val(t2, "ee", 5),
  p_permutacion = val(inf, "p_permutacion", 1), nulo_p2.5 = val(inf, "nulo_p2.5", 1),
  nulo_p97.5 = val(inf, "nulo_p97.5", 1),
  capa1_b3 = val(t3, "b3", 1), capa2_b3 = val(t3, "b3", 2), capa3_b3 = val(t3, "b3", 3), capa3_t = val(t3, "t", 3),
  evento_F_previos = val(tp, "F", 1), evento_p_previos = val(tp, "p", 1),
  evento_p_sin_pendiente_nac = val(tp, "p", 2),
  mde_por_de = val(pot, "mde_2.8", 1), mde_pct_pendiente = val(pot, "mde_pct_pendiente", 1),
  cofenetica = val(rc, "correlacion_cofenetica", r1), silueta_ward_k3 = val(sk, "silueta_ward", skp & sk$k == 3),
  silueta_kmedias_k3 = val(sk, "silueta_kmedias", skp & sk$k == 3),
  silueta_kmedias_k4 = val(sk, "silueta_kmedias", skp & sk$k == 4),
  ari_ward_kmedias = val(rc, "ari_ward_kmedias", r1), estabilidad_ari_media = val(rc, "estabilidad_ari_media", r1),
  estabilidad_ari_p5 = val(rc, "estabilidad_ari_p5", r1), r2_vs_k1_k3 = val(sk, "r2_vs_k1", skp & sk$k == 3),
  calinski_k2 = val(sk, "calinski_harabasz", skp & sk$k == 2), calinski_k3 = val(sk, "calinski_harabasz", skp & sk$k == 3),
  davies_bouldin_k3 = val(sk, "davies_bouldin", skp & sk$k == 3), amplitud_picos = val(rc, "amplitud_picos", r1),
  conglomerado_temprano_entidades = val(cal, "n", cal$nombre == "Repunte temprano"),
  conglomerado_tardio_entidades = val(cal, "n", cal$nombre == "Repunte tardio"),
  conglomerado_sin_repunte_entidades = val(cal, "n", cal$nombre == "Sin repunte"),
  dbscan_grupos_max = val(rc, "dbscan_max_grupos", r1), dbscan_ruido_min = val(rc, "dbscan_ruido_min", r1),
  dbscan_ruido_max = val(rc, "dbscan_ruido_max", r1),
  b3_temprano = cpp("Repunte temprano", "b3"), t_temprano = cpp("Repunte temprano", "t"),
  p_perm_temprano = cpp("Repunte temprano", "p_permutacion"),
  b3_tardio = cpp("Repunte tardio", "b3"), t_tardio = cpp("Repunte tardio", "t"),
  p_perm_tardio = cpp("Repunte tardio", "p_permutacion"),
  b3_sin_repunte = cpp("Sin repunte", "b3"), p_perm_sin_repunte = cpp("Sin repunte", "p_permutacion"),
  het_mujeres = b6("Mujeres"), het_hombres = b6("Hombres"), het_rural = b6("Origen rural (< 2,500 hab.)"),
  het_urbano = b6("Origen urbano"), het_q12 = b6("Origen en Q1 y Q2"), het_q45 = b6("Origen en Q4 y Q5"),
  rango_escolaridad_b3 = b6("Rango de anios de escolaridad (empates por orden, Entregable 3)"),
  q1_sigue_q1 = b6("Origen Q1 -> sigue en Q1"), q1_llega_q45 = b6("Origen Q1 -> llega a Q4 o Q5"),
  n_q1 = b6("Origen Q1 -> sigue en Q1", "n"),
  primera_escolaridad = b6("Anios de escolaridad"), primera_escolaridad_cota = b6("Anios de escolaridad", "cota_sup_95"),
  primera_activos = b6("Indice de activos de destino (nivel)"), primera_migracion = b6("Migracion interestatal"),
  transiciones_mde_pct = val(tc, "mde_pct_pendiente"),
  mun_n = val(m5, "n", 2), mun_municipios = val(md, "municipios"),
  mun_b3_conteo = val(m5, "b3", 1), mun_ee_conteo = val(m5, "ee", 1),
  mun_b3_tasa = val(m5, "b3", 2), mun_ee_tasa = val(m5, "ee", 2), mun_b3_tasa_tamano = val(m5, "b3", 3),
  mun_ee_tasa_entidad = val(m5, "ee", 4),
  mun_cor_rural_conteo_intra = val(md, "cor_rural_conteo_intra"), mun_cor_rural_tasa_intra = val(md, "cor_rural_tasa_intra"),
  mun_cor_rural_conteo_niveles = val(md, "cor_rural_conteo_niveles"),
  mun_cor_rural_tasa_niveles = val(md, "cor_rural_tasa_niveles"), mun_de_intra = val(md, "de_intraestatal_Dm"),
  mun_p_evento_con_tamano = val(mp, "p_previos", mp$medida == "tasa" & mp$controles_tamano),
  mun_p_evento_sin_tamano = val(mp, "p_previos", mp$medida == "tasa" & !mp$controles_tamano),
  mun_mde_pct = val(md, "mde_pct_pendiente_tasa"), mun_dbscan_municipios = val(mdb, "municipios", 1),
  pool_mde_pct = val(pc, "mde_pct_pendiente"), pool_pct_expuestos_2017 = val(pc, "pct_expuestos_2017"),
  pool_frac_max_2017 = val(pc, "frac_max_2017"),
  ciclo_pend_2017 = val(cv, "pendiente_2017", 1), ciclo_pend_2023 = val(cv, "pendiente_2023", 1))

obj <- read_csv(file.path(RUTAS$config, "objetivos_entregable3.csv"))
stopifnot(setequal(obj$indicador, names(obtenido)))
verif <- obj |>
  mutate(valor_v5 = unname(obtenido[indicador]), diferencia = valor_v5 - valor_entregable3,
         estado = case_when(is.na(valor_v5) ~ "omitido",
                            abs(diferencia) <= tolerancia + 1e-9 ~ "coincide",
                            TRUE ~ "difiere"))
guardar_tabla(verif, "t13_verificacion_entregable3")
res <- verif |> count(modulo, estado)
print(res)
print(verif |> filter(estado == "difiere") |> select(indicador, valor_entregable3, valor_v5, diferencia))
message(sprintf("13 terminado. Coinciden %d de %d cifras del Entregable 3 (%d omitidas por modulos opcionales).",
                sum(verif$estado == "coincide"), nrow(verif), sum(verif$estado == "omitido")))
