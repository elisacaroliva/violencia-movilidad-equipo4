# =============================================================================
# funciones_utiles.R  (version 5)
# Funciones reutilizables. Validan sus insumos (stopifnot) para que un error de
# datos detenga el pipeline en lugar de propagarse en silencio.
# =============================================================================

# ---- Bitacoras ------------------------------------------------------------------
registrar_n <- function(script, paso, n, n_previo = NA_integer_, nota = "") {
  archivo <- file.path(RUTAS$logs, "log_n.csv")
  fila <- tibble(script = script, paso = paso, n = as.integer(n),
                 n_previo = as.integer(n_previo),
                 perdidos = ifelse(is.na(n_previo), NA_integer_,
                                   as.integer(n_previo) - as.integer(n)),
                 nota = nota)
  write_csv(fila, archivo, append = file.exists(archivo))
  message(sprintf("[N] %s | %s: %s", script, paso, format(n, big.mark = ",")))
  invisible(fila)
}

registrar_decision <- function(script, decision, criterio, casos = NA_integer_) {
  archivo <- file.path(RUTAS$logs, "log_decisiones_codigo.csv")
  fila <- tibble(script = script, decision = decision, criterio = criterio,
                 casos_afectados = as.integer(casos))
  write_csv(fila, archivo, append = file.exists(archivo))
  invisible(fila)
}

# ---- Texto y lectura -------------------------------------------------------------
normalizar_texto <- function(x) {
  x <- stringi::stri_trans_general(enc2utf8(as.character(x)), "Latin-ASCII")
  str_squish(toupper(x))
}

limpiar_nombres <- function(df) {
  nm <- stringi::stri_trans_general(enc2utf8(names(df)), "Latin-ASCII")
  nm <- gsub("[^a-z0-9]+", "_", tolower(nm))
  names(df) <- gsub("^_|_$", "", nm)
  df
}

a_na <- function(x, codigos) {
  x <- as.numeric(haven::zap_labels(x))
  x[x %in% codigos] <- NA
  x
}

buscar_archivo <- function(carpeta, patron, obligatorio = TRUE, descripcion = "") {
  f <- sort(list.files(carpeta, pattern = patron, full.names = TRUE, ignore.case = TRUE),
            decreasing = TRUE)   # si hay varios cortes, el mas reciente por nombre
  if (length(f) == 0) {
    if (obligatorio) stop("No se encontro ", descripcion, " en ", carpeta,
                          " (patron: ", patron, "). Ver README.md", call. = FALSE)
    return(NA_character_)
  }
  if (length(f) > 1) message("Varios archivos coinciden; se usa: ", basename(f[1]))
  f[1]
}

# Lee un CSV del SESNSP probando UTF-8 y Latin-1 (los cortes cambian de codificacion)
leer_csv_sesnsp <- function(ruta) {
  muestra <- readLines(ruta, n = 5000, warn = FALSE, encoding = "bytes")
  enc <- if (all(validUTF8(muestra))) "UTF-8" else "latin1"
  read_csv(ruta, locale = locale(encoding = enc),
           col_types = cols(.default = col_character()), progress = FALSE)
}

# ---- Estandarizacion -----------------------------------------------------------------
estandarizar <- function(x) {
  s <- sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(0, length(x)))
  (x - mean(x, na.rm = TRUE)) / s
}

# ---- Indice de recursos por componentes principales ----------------------------------
# Igual que la Entrega 2: dicotomicos 1 = si, 2 = no (otros codigos = faltante);
# automoviles como conteo (98/99 = faltante); imputacion por la media;
# estandarizacion; primer componente, orientado para que mas = mas recursos.
indice_pca <- function(df, binarios, numericas) {
  X <- sapply(binarios, function(v) {
    x <- as.numeric(haven::zap_labels(df[[v]]))
    ifelse(x == 1, 1, ifelse(x == 2, 0, NA_real_))
  })
  for (v in numericas) {
    x <- as.numeric(haven::zap_labels(df[[v]])); x[x >= 98] <- NA
    X <- cbind(X, x); colnames(X)[ncol(X)] <- v
  }
  n_faltantes <- sum(is.na(X))
  X <- apply(X, 2, function(v) { v[is.na(v)] <- mean(v, na.rm = TRUE); v })
  Z <- scale(X); Z[is.nan(Z)] <- 0
  sv <- svd(Z)
  s <- as.numeric(Z %*% sv$v[, 1])
  if (cor(s, rowMeans(Z)) < 0) s <- -s
  list(puntaje = s, var_pc1 = sv$d[1]^2 / sum(sv$d^2), k = ncol(Z),
       celdas_imputadas = n_faltantes)
}

# ---- Rangos percentiles ponderados ---------------------------------------------------
# empates = "promedio": observaciones con el mismo valor comparten rango (principal).
# empates = "orden"   : rango por orden de fila, como modelo.py (solo para el puente).
rango_ponderado <- function(x, w, empates = c("promedio", "orden")) {
  empates <- match.arg(empates)
  ok <- !is.na(x) & !is.na(w) & w > 0
  out <- rep(NA_real_, length(x))
  if (!any(ok)) return(out)
  if (empates == "orden") {
    o <- order(x[ok], method = "radix")          # orden estable
    ws <- w[ok][o]
    r <- (cumsum(ws) - ws / 2) / sum(ws)
    out[which(ok)[o]] <- r
    return(out)
  }
  agg <- tibble(x = x[ok], w = w[ok]) |>
    group_by(x) |> summarise(w = sum(w), .groups = "drop") |>
    arrange(x) |> mutate(r = (cumsum(w) - w / 2) / sum(w))
  out[ok] <- agg$r[match(x[ok], agg$x)]
  out
}

quintil_de_rango <- function(r) pmin(pmax(ceiling(r * 5), 1L), 5L)

# ---- SESNSP (metodologia anterior) -----------------------------------------------------
MESES <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio",
           "agosto", "septiembre", "octubre", "noviembre", "diciembre")

# Devuelve: anio, cve_ent, nivel1 (MODALIDAD), nivel2 (TIPO), nivel3 (SUBTIPO), total
leer_sesnsp_anterior <- function(ruta) {
  s <- leer_csv_sesnsp(ruta) |> limpiar_nombres()
  col_anio <- c(intersect(c("ano", "anio", "year"), names(s)),
                grep("^a[a-z_]{0,3}o$", names(s), value = TRUE))[1]
  col_ent <- intersect(c("inegi", "clave_ent", "cve_ent", "cve_entidad"), names(s))[1]
  niveles <- c("modalidad", "tipo", "subtipo")
  if (is.na(col_anio) || is.na(col_ent) || !all(niveles %in% names(s)) ||
      !all(MESES %in% names(s))) {
    stop("Estructura inesperada en ", basename(ruta), ". Columnas: ",
         paste(names(s), collapse = ", "), call. = FALSE)
  }
  s <- s |>
    transmute(anio = as.integer(.data[[col_anio]]),
              cve_ent = as.integer(.data[[col_ent]]),
              nivel1 = normalizar_texto(coalesce(modalidad, "")),
              nivel2 = normalizar_texto(coalesce(tipo, "")),
              nivel3 = normalizar_texto(coalesce(subtipo, "")),
              across(all_of(MESES), ~ suppressWarnings(as.numeric(gsub(",", "", .x)))))
  s$total <- rowSums(s[MESES], na.rm = TRUE)
  s |> filter(cve_ent %in% 1:32) |> select(anio, cve_ent, nivel1, nivel2, nivel3, total)
}

# Asigna cada categoria del SESNSP a un delito del mapa (config/mapa_delitos.csv).
# Se detiene si un delito no encuentra categoria o si una categoria cae en dos.
asignar_delitos <- function(s, mapa) {
  cat_ <- s |> distinct(nivel1, nivel2, nivel3)
  asign <- map_dfr(seq_len(nrow(mapa)), function(i) {
    cat_ |> filter(str_detect(nivel1, mapa$patron_nivel1[i]),
                   str_detect(nivel2, mapa$patron_nivel2[i]),
                   str_detect(nivel3, mapa$patron_nivel3[i])) |>
      mutate(delito_id = mapa$delito_id[i], etiqueta = mapa$etiqueta[i])
  })
  sin <- setdiff(unique(mapa$delito_id), asign$delito_id)
  if (length(sin) > 0) stop("Sin categoria SESNSP para ", paste(sin, collapse = ", "),
                            ". Revisa t03_catalogo_sesnsp.csv y config/mapa_delitos.csv.",
                            call. = FALSE)
  dobles <- asign |> count(nivel1, nivel2, nivel3) |> filter(n > 1)
  if (nrow(dobles) > 0) stop("Categorias asignadas a mas de un delito.")
  asign
}

# Poblacion CONAPO a mitad de anio por entidad (suma edades y sexos)
poblacion_conapo <- function(ruta, anios) {
  p <- read_csv(ruta, locale = locale(encoding = "latin1"), progress = FALSE) |>
    limpiar_nombres()
  c_anio <- c(intersect(c("anio", "ano", "year"), names(p)),
              grep("^a[a-z_]{0,3}o$", names(p), value = TRUE))[1]
  c_ent <- intersect(c("cve_geo", "cve_ent", "clave_ent"), names(p))[1]
  c_pob <- intersect(c("poblacion", "pob", "pob_mit"), names(p))[1]
  stopifnot(!is.na(c_anio), !is.na(c_ent), !is.na(c_pob))
  pob <- p |>
    transmute(anio = as.integer(.data[[c_anio]]), cve_ent = as.integer(.data[[c_ent]]),
              pob = as.numeric(.data[[c_pob]])) |>
    filter(cve_ent %in% 1:32, anio %in% anios) |>
    group_by(cve_ent, anio) |> summarise(pob = sum(pob), .groups = "drop")
  stopifnot(nrow(pob) == 32 * length(anios))
  pob
}

# Poblacion interpolada log-lineal entre los censos 2010 y 2020 (como la
# presentacion). Para 1997-2009 es una EXTRAPOLACION hacia atras: solo robustez y puente.
poblacion_censal_interpolada <- function(anios) {
  pob2010 <- c(1184996, 3155070, 637026, 822441, 2748391, 650555, 4796580, 3406465,
               8851080, 1632934, 5486372, 3388768, 2665018, 7350682, 15175862, 4351037,
               1777227, 1084979, 4653458, 3801962, 5779829, 1827937, 1325578, 2585518,
               2767761, 2662480, 2238603, 3268554, 1169936, 7643194, 1955577, 1490668)
  pob2020 <- c(1425607, 3769020, 798447, 928363, 3146771, 731391, 5543828, 3741869,
               9209944, 1832650, 6166934, 3540685, 3082841, 8348151, 16992418, 4748846,
               1971520, 1235456, 5784442, 4132148, 6583278, 2368467, 1857985, 2822255,
               3026943, 2944840, 2402598, 3527735, 1342977, 8062579, 2320898, 1622138)
  tasa <- (log(pob2020) - log(pob2010)) / 10
  expand_grid(cve_ent = 1:32, anio = anios) |>
    mutate(pob = pob2010[cve_ent] * exp(tasa[cve_ent] * (anio - 2010)))
}

# ---- Indice de violencia por componentes principales ---------------------------------
# Z: matriz (filas = entidad-anio, columnas = delitos) de tasas estandarizadas,
# con NA donde el delito no tiene registro. Las cargas salen de la matriz de
# correlacion de las filas completas. En filas con faltantes, el puntaje usa los
# delitos disponibles y reescala las cargas para que sumen lo mismo que todas.
indice_pc1_violencia <- function(Z) {
  completas <- stats::complete.cases(Z)
  e <- eigen(stats::cor(Z[completas, , drop = FALSE]), symmetric = TRUE)
  v <- e$vectors[, 1]; if (sum(v) < 0) v <- -v
  names(v) <- colnames(Z)
  puntaje <- apply(Z, 1, function(z) {
    ok <- !is.na(z)
    sum(v[ok] * z[ok]) * sum(v) / sum(v[ok])
  })
  list(puntaje = puntaje, cargas = v, var_pc1 = e$values[1] / sum(e$values),
       filas_completas = sum(completas))
}

# ---- Dosis de la presentacion ---------------------------------------------------------
# Fraccion de la ventana de edades [edades] que cae en o despues de `anio_inicio`.
fraccion_ventana <- function(nac, anio_inicio, edades = EDAD_VENTANA) {
  largo <- edades[2] - edades[1] + 1
  ini <- nac + edades[1]; fin <- nac + edades[2]
  pmin(pmax(fin - pmax(ini, anio_inicio) + 1, 0), largo) / largo
}

# ---- Triple diferencia ----------------------------------------------------------------
# r_des = b1 r_ori + b2 D + b3 (r_ori x D) + EF_ent + EF_nac + EF_ent x r_ori
#         + EF_nac x r_ori + e
# fixest: `cve_14[r_ori]` agrega el efecto fijo de entidad y su pendiente propia;
# lo mismo para el anio de nacimiento. MCP con factor de expansion, errores
# agrupados por entidad, correccion de muestra pequena como statsmodels (K completo).
# Con G conglomerados, los valores p, los intervalos y el efecto minimo detectable
# usan la t de Student con G - 1 grados de libertad (no la normal).
SSC_PROYECTO <- fixest::ssc(adj = TRUE, fixef.K = "full", cluster.adj = TRUE)

mde_factor <- function(G) qt(1 - ALFA / 2, G - 1) + qt(POTENCIA, G - 1)

# capas: "completo" (principal) o una de las capas de la Tabla 3 del Entregable 3:
#   "sin_controles", "ef", "ef_ent_x_rango". y: desenlace (r_des por defecto).
# escala: multiplica b3 y EE (para pasar a "por DE" cuando la dosis esta cruda).
FE_CAPAS <- c(sin_controles = "", ef = "| cve_14 + nac", ef_ent_x_rango = "| cve_14[r_ori] + nac",
              completo = "| cve_14[r_ori] + nac[r_ori]")
estimar_ddd <- function(d, dosis = "dosis", cluster = "cve_14", y = "r_des",
                        capas = "completo", escala = 1, controles = character(0)) {
  d <- as.data.frame(d)
  d$D <- d[[dosis]]; d$Y <- d[[y]]
  d <- d[stats::complete.cases(d[, c("Y", "r_ori", "D", "factor", "cve_14", "nac", cluster,
                                       controles)]), ]
  ctrl <- if (length(controles)) paste0(" + ", paste0(controles, collapse = " + "), " + ",
                                        paste0(controles, ":r_ori", collapse = " + ")) else ""
  # r_ori entra explicito salvo cuando la pendiente por entidad (cve_14[r_ori]) lo absorbe
  base_rhs <- if (capas %in% c("sin_controles", "ef")) "Y ~ r_ori + D + D:r_ori" else "Y ~ D + D:r_ori"
  f <- stats::as.formula(paste(base_rhs, ctrl, FE_CAPAS[[capas]]))
  m <- fixest::feols(f, data = d, weights = ~factor,
                     cluster = stats::as.formula(paste0("~", cluster)),
                     ssc = SSC_PROYECTO)
  b <- coef(m); se <- fixest::se(m)
  k <- if ("D:r_ori" %in% names(b)) "D:r_ori" else "r_ori:D"
  G <- length(unique(d[[cluster]]))
  tc <- qt(1 - ALFA / 2, G - 1)
  pend <- unname(coef(lm(Y ~ r_ori, data = d, weights = factor))[2])
  list(modelo = m,
       fila = tibble(n = nobs(m), conglomerados = G,
                     b3 = escala * unname(b[k]), ee = escala * unname(se[k]), t = b3 / ee,
                     # inferencia principal del Entregable 3 (normal)
                     p_normal = 2 * pnorm(-abs(t)),
                     ic_inf = b3 - Z_CRIT * ee, ic_sup = b3 + Z_CRIT * ee,
                     mde = MDE_NORMAL * ee, mde_pct_pendiente = 100 * mde / pend,
                     # correccion v4 (t con G - 1 gl), al lado
                     p_t = 2 * pt(-abs(t), G - 1),
                     ic_inf_t = b3 - tc * ee, ic_sup_t = b3 + tc * ee,
                     mde_t = mde_factor(G) * ee,
                     pendiente_base = pend,
                     media_dosis = weighted.mean(d$D, d$factor),
                     pct_con_dosis = 100 * weighted.mean(d$D != 0, d$factor)))
}

# Regresion en niveles de un desenlace sobre la dosis con EF de entidad y de anio de
# nacimiento (sin interacciones). Primera etapa y transiciones (Entregable 3, Tabla 6).
estimar_nivel <- function(d, y, dosis = "dosis", escala = 1, con_rango = FALSE) {
  d <- as.data.frame(d); d$Y <- as.numeric(d[[y]]); d$D <- d[[dosis]]
  d <- d[stats::complete.cases(d[, c("Y", "D", "factor", "cve_14", "nac")]), ]
  f <- if (con_rango) Y ~ D + r_ori | cve_14 + nac else Y ~ D | cve_14 + nac
  m <- fixest::feols(f, data = d, weights = ~factor, cluster = ~cve_14, ssc = SSC_PROYECTO)
  b <- escala * unname(coef(m)["D"]); se <- escala * unname(fixest::se(m)["D"])
  tibble(n = nobs(m), b = b, ee = se, t = b / se, p_normal = 2 * pnorm(-abs(b / se)),
         cota_sup_95 = b + Z_CRIT * se, mde = MDE_NORMAL * se,
         tasa_base = weighted.mean(d$Y, d$factor))
}

# Matriz de efectos fijos explicita (constante, r_ori, EF entidad, EF nacimiento y
# sus interacciones con r_ori). La usan la permutacion y el bootstrap salvaje.
matriz_ef <- function(d) {
  E <- model.matrix(~ factor(cve_14), d)[, -1, drop = FALSE]
  C <- model.matrix(~ factor(nac), d)[, -1, drop = FALSE]
  cbind(1, d$r_ori, E, C, E * d$r_ori, C * d$r_ori)
}

# ---- Inferencia por permutacion (Frisch-Waugh) ---------------------------------------
# Reasigna al azar, entre entidades, la intensidad de violencia (quien era "Morelos"
# recibe la de otra entidad), reconstruye la dosis y recalcula b3. El componente
# de cohorte de la dosis (fraccion de la ventana) se conserva.
# dosis_de: funcion(perm) -> vector de dosis; perm[j] es la entidad cuya intensidad
# recibe la entidad j (en el orden de `entidades`).
permutacion_ddd <- function(d, dosis_obs, dosis_de, entidades, B = B_PERMUTACIONES,
                            semilla = SEMILLA) {
  set.seed(semilla)
  FE <- matriz_ef(d)
  sw <- sqrt(d$factor)
  Q <- qr.Q(qr(FE * sw))
  resid <- function(v) { vw <- v * sw; vw - Q %*% crossprod(Q, vw) }
  ry <- as.numeric(resid(d$r_des)); ro <- d$r_ori
  # b3 de varias dosis a la vez (una por columna): regresion de ry sobre los
  # residuos de D y de r_ori x D, resuelta en forma cerrada (2 x 2).
  b3 <- function(Dm) {
    Dm <- as.matrix(Dm)
    R1 <- resid(Dm); R2 <- resid(ro * Dm)
    a11 <- colSums(R1^2); a12 <- colSums(R1 * R2); a22 <- colSums(R2^2)
    c1 <- colSums(R1 * ry); c2 <- colSums(R2 * ry)
    (a11 * c2 - a12 * c1) / (a11 * a22 - a12^2)
  }
  obs <- b3(dosis_obs)
  perms <- lapply(seq_len(B), function(r) sample(entidades))
  lotes <- split(seq_len(B), ceiling(seq_len(B) / 250))
  nulos <- unlist(lapply(lotes, function(ix) b3(sapply(perms[ix], dosis_de))), use.names = FALSE)
  # tolerancia numerica: con pocas entidades hay permutaciones que reproducen
  # exactamente el b3 observado y deben contar como "al menos tan grandes"
  list(b3 = unname(obs), p = mean(abs(nulos) >= abs(obs) - 1e-10),
       nulo_p2.5 = unname(quantile(nulos, 0.025)),
       nulo_p97.5 = unname(quantile(nulos, 0.975)), nulos = nulos)
}

# ---- Bootstrap salvaje por conglomerados (Webb, restringido) --------------------------
# Cameron, Gelbach y Miller (2008); pesos de Webb para pocos conglomerados.
# Se impone H0: b3 = 0 y se compara el t observado con la distribucion bootstrap.
wild_cluster_boot <- function(d, dosis_obs, B = B_WILD, semilla = SEMILLA) {
  set.seed(semilla)
  X <- cbind(matriz_ef(d), dosis_obs, d$r_ori * dosis_obs)
  q <- qr(X); X <- X[, sort(q$pivot[seq_len(q$rank)]), drop = FALSE]   # quita colineales
  j <- ncol(X)                                  # la ultima columna es r_ori x D
  stopifnot(identical(unname(X[, j]), unname(d$r_ori * dosis_obs)))
  w <- d$factor / mean(d$factor); y <- d$r_des
  N <- nrow(X); K <- ncol(X)
  gi <- match(d$cve_14, unique(d$cve_14)); G <- max(gi)
  XtWX_inv <- solve(crossprod(X, X * w))
  h <- as.numeric(X %*% XtWX_inv[j, ]) * w
  c_adj <- (G / (G - 1)) * ((N - 1) / (N - K))
  t_de <- function(yv) {
    beta <- XtWX_inv %*% crossprod(X, w * yv)
    u <- yv - as.numeric(X %*% beta)
    s <- rowsum(h * u, gi)
    c(beta[j], sqrt(c_adj * sum(s^2)))
  }
  obs <- t_de(y)
  fit_r <- lm.wfit(X[, -j, drop = FALSE], y, w)
  u_r <- as.numeric(fit_r$residuals); yhat_r <- y - u_r
  webb <- c(-sqrt(1.5), -1, -sqrt(0.5), sqrt(0.5), 1, sqrt(1.5))
  t_boot <- vapply(seq_len(B), function(r) {
    v <- sample(webb, G, replace = TRUE)
    res <- t_de(yhat_r + u_r * v[gi]); res[1] / res[2]
  }, numeric(1))
  tibble(b3 = obs[1], ee_cluster = obs[2], t = obs[1] / obs[2],
         p_wild = mean(abs(t_boot) >= abs(obs[1] / obs[2])),
         conglomerados = G, B = B)
}

# ---- Conglomerados de trayectorias de violencia ----------------------------------------
indice_rand_ajustado <- function(a, b) {
  tab <- table(a, b); n <- sum(tab)
  c2 <- function(x) sum(choose(x, 2))
  s_ij <- c2(tab); s_a <- c2(rowSums(tab)); s_b <- c2(colSums(tab))
  esp <- s_a * s_b / choose(n, 2)
  (s_ij - esp) / ((s_a + s_b) / 2 - esp)
}

# Criterios internos para una particion `lab` de las filas de X (distancia euclidiana)
criterios_particion <- function(X, lab) {
  n <- nrow(X); k <- length(unique(lab))
  centro <- colMeans(X)
  TSS <- sum(sweep(X, 2, centro)^2)
  cent <- t(sapply(sort(unique(lab)), function(g) colMeans(X[lab == g, , drop = FALSE])))
  WSS <- sum(sapply(sort(unique(lab)), function(g) {
    Xi <- X[lab == g, , drop = FALSE]; sum(sweep(Xi, 2, colMeans(Xi))^2) }))
  ch <- ((TSS - WSS) / (k - 1)) / (WSS / (n - k))
  # Davies-Bouldin: dispersion = distancia media al centroide
  disp <- sapply(seq_len(k), function(i) {
    g <- sort(unique(lab))[i]; Xi <- X[lab == g, , drop = FALSE]
    mean(sqrt(rowSums(sweep(Xi, 2, cent[i, ])^2))) })
  dc <- as.matrix(dist(cent))
  db <- mean(sapply(seq_len(k), function(i) max((disp[i] + disp[-i]) / dc[i, -i])))
  tibble(k = k, wss = WSS, tss = TSS, r2_vs_k1 = 1 - WSS / TSS,
         calinski_harabasz = ch, davies_bouldin = db)
}

# T: matriz entidad x anio del indice. Se estandariza DENTRO de cada entidad (forma
# de la trayectoria, no nivel). Entregable 3, seccion 4.3:
#   - Ward (dendrograma; fidelidad = correlacion cofenetica) y k-medias, ambos con
#     silueta media para k = 2..8 (hiperparametro);
#   - R2 frente al modelo de referencia k = 1 (el calendario comun de la dosis),
#     Calinski-Harabasz y Davies-Bouldin (no tienen por que coincidir);
#   - acuerdo entre algoritmos: Rand ajustado Ward vs k-medias en k = 3;
#   - estabilidad: 500 submuestras del 80 % de las entidades, Ward en cada una y
#     Rand ajustado contra la particion completa restringida a las entidades sorteadas;
#   - DBSCAN en una rejilla de radios (con 32 entidades no es informativo).
conglomerar_trayectorias <- function(T, semilla = SEMILLA) {
  X <- t(apply(T, 1, function(x) (x - mean(x)) / sd(x)))
  D <- dist(X)
  hc <- hclust(D, method = "ward.D2")
  cofen <- cor(D, cophenetic(hc))
  set.seed(semilla)
  km <- lapply(K_RANGO, function(k) kmeans(X, k, nstart = K_MEDIAS_NSTART, iter.max = 100))
  sil_k <- vapply(km, function(m) mean(cluster::silhouette(m$cluster, D)[, 3]), numeric(1))
  sil_w <- vapply(K_RANGO, function(k) mean(cluster::silhouette(cutree(hc, k), D)[, 3]),
                  numeric(1))
  crit <- bind_rows(lapply(K_RANGO, function(k) criterios_particion(X, cutree(hc, k)))) |>
    mutate(silueta_ward = sil_w, silueta_kmedias = sil_k, .after = k)
  k_opt_w <- K_RANGO[which.max(sil_w)]; k_opt_k <- K_RANGO[which.max(sil_k)]
  ward <- cutree(hc, K_FIJO)
  kmed <- km[[which(K_RANGO == K_FIJO)]]$cluster
  ari <- indice_rand_ajustado(ward, kmed)
  # Estabilidad por submuestreo
  set.seed(semilla)
  n <- nrow(X); m <- floor(n * FRAC_ESTABILIDAD)
  aris <- vapply(seq_len(B_ESTABILIDAD), function(r) {
    sel <- sort(sample.int(n, m))
    hs <- hclust(dist(X[sel, , drop = FALSE]), method = "ward.D2")
    indice_rand_ajustado(ward[sel], cutree(hs, K_FIJO))
  }, numeric(1))
  # DBSCAN en la rejilla fija de radios
  db <- map_dfr(DBSCAN_EPS, function(e) {
    cl <- dbscan::dbscan(X, eps = e, minPts = DBSCAN_MINPTS)$cluster
    tibble(eps = e, minPts = DBSCAN_MINPTS, grupos = length(setdiff(unique(cl), 0)),
           ruido = sum(cl == 0))
  })
  anios <- as.integer(colnames(T))
  pico_de <- function(lab) sapply(sort(unique(lab)), function(g)
    anios[which.max(colMeans(X[lab == g, , drop = FALSE]))])
  perfil <- map_dfr(sort(unique(ward)), function(g) {
    tibble(grupo = g, anio = anios, media = colMeans(X[ward == g, , drop = FALSE]),
           de = apply(X[ward == g, , drop = FALSE], 2, sd))
  })
  picos <- perfil |> group_by(grupo) |> slice_max(media, n = 1, with_ties = FALSE) |>
    ungroup() |> arrange(anio) |> select(grupo, anio_pico = anio)
  stopifnot(nrow(picos) == 3)          # el etiquetado supone k = 3
  picos$nombre <- ifelse(picos$anio_pico <= 2003, "Sin repunte", NA)
  resto <- which(is.na(picos$nombre))
  stopifnot(length(resto) == 2)
  picos$nombre[resto] <- c("Repunte temprano", "Repunte tardio")
  # Inicio del repunte: primer anio desde el cual el perfil medio del grupo queda
  # por encima del promedio de la entidad (0) sin volver a bajar hasta su pico.
  inicio <- perfil |> left_join(picos, by = "grupo") |>
    filter(nombre != "Sin repunte", anio <= anio_pico) |>
    group_by(grupo) |> arrange(desc(anio), .by_group = TRUE) |>
    mutate(arriba = cumprod(media > 0)) |>
    filter(arriba == 1) |> summarise(anio_inicio = min(anio), .groups = "drop")
  picos <- picos |> left_join(inicio, by = "grupo")
  # Invarianza del hallazgo en k = 2..4: el grupo temprano y el tardio de k = 3,
  # ?siguen juntos (y sin mezclarse con otros) en las particiones de k = 2 y 4?
  g_temp <- picos$grupo[picos$nombre == "Repunte temprano"]
  g_tard <- picos$grupo[picos$nombre == "Repunte tardio"]
  invarianza <- map_dfr(K_INVARIANZA, function(k) {
    lab <- cutree(hc, k)
    juntos <- function(g) length(unique(lab[ward == g])) == 1
    identico <- function(g) juntos(g) && all(ward[lab == lab[ward == g][1]] == g)
    tibble(k = k, temprano_juntos = juntos(g_temp), temprano_identico = identico(g_temp),
           tardio_juntos = juntos(g_tard), tardio_identico = identico(g_tard))
  })
  pk <- pico_de(ward)
  list(asignacion = tibble(cve_ent = as.integer(rownames(T)), grupo = ward,
                           grupo_kmedias = kmed) |>
         left_join(picos, by = "grupo"),
       perfil = perfil |> left_join(picos |> select(grupo, nombre), by = "grupo"),
       resumen = tibble(correlacion_cofenetica = cofen, k_usado = K_FIJO,
                        k_silueta = k_opt_k, k_silueta_ward = k_opt_w,
                        silueta_max = max(sil_k),
                        silueta_k_usado = sil_k[K_RANGO == K_FIJO],
                        silueta_ward_k_usado = sil_w[K_RANGO == K_FIJO],
                        ari_ward_kmedias = ari,
                        estabilidad_ari_media = mean(aris),
                        estabilidad_ari_p5 = unname(quantile(aris, 0.05)),
                        r2_vs_k1_k_usado = crit$r2_vs_k1[crit$k == K_FIJO],
                        k_calinski = crit$k[which.max(crit$calinski_harabasz)],
                        k_davies_bouldin = crit$k[which.min(crit$davies_bouldin)],
                        amplitud_picos = max(pk) - min(pk),
                        pico_referencia_k1 = anios[which.max(colMeans(X))],
                        dbscan_max_grupos = max(db$grupos),
                        dbscan_ruido_min = min(db$ruido), dbscan_ruido_max = max(db$ruido)),
       silueta = crit, estabilidad = aris, invarianza = invarianza,
       dbscan = db, hc = hc, X = X)
}

# ---- Guardado ------------------------------------------------------------------------
guardar_tabla <- function(df, nombre) {
  ruta <- file.path(RUTAS$tablas, paste0(nombre, ".csv"))
  write_csv(df, ruta); message("Tabla: ", basename(ruta)); invisible(ruta)
}
guardar_figura <- function(p, nombre, ancho = 7, alto = 4.5) {
  ruta <- file.path(RUTAS$figuras, paste0(nombre, ".png"))
  # Formato de revista: el titulo, las notas y la fuente van en el manuscrito,
  # no dentro de la imagen.
  if (isTRUE(FIGURAS_SIN_TITULO)) p <- p + labs(title = NULL, subtitle = NULL, caption = NULL)
  ggsave(ruta, p, width = ancho, height = alto, dpi = 300, bg = "white")
  message("Figura: ", basename(ruta)); invisible(ruta)
}
