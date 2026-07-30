library(duckdb)
library(DBI)

# ==============================================================================
# Anonimización k-anonimidad + l-diversidad, 100% en DuckDB
#
# Misma lógica que `anonimizar()` en anonimizador.R, pero los datos NUNCA se
# cargan en memoria R: se leen y escriben como Parquet (o CSV) y todas las
# transformaciones ocurren dentro de DuckDB. Pensado para datasets que no caben
# en RAM.
#
# Este archivo es autocontenido a propósito, para poder copiarlo solo. Si tocas
# la lógica de anonimización, revisa que anonimizador.R siga en línea.
# ==============================================================================

# Construye las etiquetas de los tramos de edad a partir de los cortes
# inferiores. El último corte queda siempre abierto.
#   c(0, 5, 10)              -> c("0-4", "5-9", "10+")
#   c(0, 15, 30, 45)         -> c("0-14", "15-29", "30-44", "45+")
etiquetas_edad <- function(rangos) {
  n <- length(rangos)
  if (n < 1) {
    stop("'edad_rangos' debe contener al menos un corte.")
  }
  if (n > 1 && is.unsorted(rangos, strictly = TRUE)) {
    stop("'edad_rangos' debe ser un vector estrictamente creciente.")
  }
  c(
    if (n > 1) paste0(rangos[-n], "-", rangos[-1] - 1),
    paste0(rangos[n], "+")
  )
}

anonimizar_duckdb <- function(
  archivo_entrada, # Ruta al .parquet o .csv de entrada
  archivo_salida, # Ruta al .parquet o .csv de salida
  id_vars = NULL, # Variables a eliminar por ser identificadores directos
  pseudo_id_vars = NULL, # Variables a reemplazar por un hash con salt
  quasi_id_vars, # Variables cuasi-identificadoras
  sensitive_var, # Variable sensible
  geo_vars = NULL, # Variables geográficas a anonimizar (opcional)
  k = 2, # Valor de k-anonimidad
  l = 2, # Valor de l-diversidad
  edad_rangos = seq(0, 80, by = 5), # Cortes inferiores de los tramos de edad
  salt = NULL, # Salt del hash; si es NULL se genera uno aleatorio
  eliminar_temporales = TRUE # Si se deben eliminar las columnas temporales
) {
  if (!is.numeric(k) || k < 1 || !is.numeric(l) || l < 1) {
    stop("Los valores de k y l deben ser enteros positivos (>= 1).")
  }

  etiquetas <- etiquetas_edad(edad_rangos)
  geo_vars <- if (is.null(geo_vars)) character(0) else geo_vars

  con <- dbConnect(duckdb())
  on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # --- Helpers internos -------------------------------------------------------

  # Quotear identificadores SQL
  qi <- function(x) {
    sapply(
      x,
      function(v) as.character(dbQuoteIdentifier(con, v)),
      USE.NAMES = FALSE
    )
  }

  # Quotear literales de texto (rutas de archivo, salt, etiquetas)
  qs <- function(x) as.character(dbQuoteString(con, as.character(x)))

  # Obtener columnas actuales de la tabla 'datos'
  get_cols <- function() {
    dbGetQuery(
      con,
      "SELECT column_name FROM information_schema.columns
       WHERE table_name = 'datos' ORDER BY ordinal_position"
    )$column_name
  }

  # Tipo de dato de una columna de 'datos'
  get_tipo <- function(col) {
    dbGetQuery(
      con,
      paste0(
        "SELECT data_type FROM information_schema.columns ",
        "WHERE table_name = 'datos' AND column_name = ",
        qs(col)
      )
    )$data_type
  }

  # Reconstruir tabla 'datos' desde un SELECT (evita UPDATEs lentos)
  rebuild <- function(select_sql) {
    dbExecute(con, "DROP TABLE IF EXISTS _datos_new")
    dbExecute(con, paste0("CREATE TABLE _datos_new AS ", select_sql))
    dbExecute(con, "DROP TABLE datos")
    dbExecute(con, "ALTER TABLE _datos_new RENAME TO datos")
  }

  # Clave de agrupación: concatena las columnas con separadores de control
  # (chr(31) = unit separator, chr(30) = centinela de NULL) que no aparecen en
  # datos reales, para que dos filas distintas no colisionen.
  build_gk <- function(cols) {
    parts <- sapply(cols, function(c) {
      paste0("COALESCE(CAST(", qi(c), " AS VARCHAR), chr(30))")
    })
    paste(parts, collapse = " || chr(31) || ")
  }

  # Calcular K y L via GROUP BY + JOIN (todo en DuckDB)
  calcular_kl <- function(group_vars, sens_var, k_col, l_col) {
    gk_expr <- build_gk(group_vars)

    # Columnas a mantener (sin K/L previas si existían)
    cols <- setdiff(get_cols(), c(k_col, l_col))
    cols_select <- paste(paste0("d.", qi(cols)), collapse = ", ")

    dbExecute(
      con,
      paste0(
        "CREATE OR REPLACE TEMP TABLE _stats AS ",
        "SELECT ", gk_expr, " AS _gk, ",
        "COUNT(*) AS ", qi(k_col), ", ",
        "COUNT(DISTINCT ", qi(sens_var), ") AS ", qi(l_col), " ",
        "FROM datos GROUP BY _gk"
      )
    )

    rebuild(paste0(
      "SELECT ", cols_select, ", s.", qi(k_col), ", s.", qi(l_col), " ",
      "FROM (SELECT *, ", gk_expr, " AS _gk FROM datos) d ",
      "JOIN _stats s ON d._gk = s._gk"
    ))

    dbExecute(con, "DROP TABLE IF EXISTS _stats")
  }

  # --- Leer datos (cero memoria R) -------------------------------------------

  lector <- if (grepl("\\.csv$", archivo_entrada, ignore.case = TRUE)) {
    "read_csv_auto"
  } else {
    "read_parquet"
  }

  dbExecute(
    con,
    paste0(
      "CREATE TABLE datos AS SELECT * FROM ",
      lector, "(", qs(archivo_entrada), ")"
    )
  )

  n_registros <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM datos")$n

  resumen <- list(
    n_registros = n_registros,
    id_vars_eliminados = NULL,
    pseudo_id_vars = NULL,
    edad_vars_agrupadas = NULL,
    vars_geo_anonimizadas = NULL,
    nivel_anonimizacion = list(),
    otras_vars_anonimizadas = NULL
  )

  # --- Validaciones ----------------------------------------------------------

  cols_actuales <- get_cols()

  if (!sensitive_var %in% cols_actuales) {
    stop("La variable sensible '", sensitive_var, "' no existe en el dataset.")
  }

  missing_vars <- quasi_id_vars[!quasi_id_vars %in% cols_actuales]
  if (length(missing_vars) > 0) {
    stop(
      "Variables cuasi-identificadoras no encontradas: ",
      paste(missing_vars, collapse = ", ")
    )
  }

  missing_geo <- geo_vars[!geo_vars %in% cols_actuales]
  if (length(missing_geo) > 0) {
    stop(
      "Variables geográficas no encontradas: ",
      paste(missing_geo, collapse = ", ")
    )
  }

  # --- 1. Eliminar identificadores directos ----------------------------------

  if (!is.null(id_vars)) {
    cols_to_drop <- intersect(id_vars, get_cols())
    if (length(cols_to_drop) > 0) {
      keep <- setdiff(get_cols(), cols_to_drop)
      rebuild(paste0("SELECT ", paste(qi(keep), collapse = ", "), " FROM datos"))
      resumen$id_vars_eliminados <- cols_to_drop
    }
  }

  # --- 2. Pseudonimización (hash md5 truncado, con salt) ---------------------

  if (!is.null(pseudo_id_vars)) {
    pseudo_presentes <- intersect(pseudo_id_vars, get_cols())
    if (length(pseudo_presentes) > 0) {
      if (is.null(salt)) {
        salt <- paste(sample(c(letters, LETTERS, 0:9), 16), collapse = "")
      }

      select_parts <- sapply(get_cols(), function(c) {
        if (c %in% pseudo_presentes) {
          paste0(
            "CASE WHEN ", qi(c), " IS NULL THEN NULL ",
            "ELSE SUBSTR(md5(", qs(salt), " || CAST(", qi(c),
            " AS VARCHAR)), 1, 12) END AS ", qi(c)
          )
        } else {
          qi(c)
        }
      })

      rebuild(paste0(
        "SELECT ", paste(select_parts, collapse = ", "), " FROM datos"
      ))
      resumen$pseudo_id_vars <- pseudo_presentes
    }
  }

  # --- 3. Agrupar variables de edad en tramos --------------------------------

  vars_a_eliminar <- character(0)
  vars_edad_agrupadas <- character(0)

  for (v in quasi_id_vars) {
    if (grepl("edad", v, ignore.case = TRUE) && v %in% get_cols()) {
      tipo <- get_tipo(v)

      if (grepl(
        "INT|FLOAT|DOUBLE|DECIMAL|NUMERIC|BIGINT", tipo,
        ignore.case = TRUE
      )) {
        vars_a_eliminar <- c(vars_a_eliminar, v)
        grupo_var <- paste0(v, "_grupo")

        q <- qi(v)
        partes <- sapply(seq_along(edad_rangos), function(i) {
          desde <- edad_rangos[i]
          if (i < length(edad_rangos)) {
            paste0(
              "WHEN ", q, " >= ", desde, " AND ", q, " < ", edad_rangos[i + 1],
              " THEN ", qs(etiquetas[i]), " "
            )
          } else {
            paste0("WHEN ", q, " >= ", desde, " THEN ", qs(etiquetas[i]), " ")
          }
        })

        rebuild(paste0(
          "SELECT *, CASE ", paste(partes, collapse = ""),
          "ELSE NULL END AS ", qi(grupo_var), " FROM datos"
        ))

        quasi_id_vars <- c(setdiff(quasi_id_vars, v), grupo_var)
        vars_edad_agrupadas <- c(vars_edad_agrupadas, grupo_var)
        resumen$edad_vars_agrupadas <- c(resumen$edad_vars_agrupadas, v)
      }
    }
  }

  # Conjunto de cuasi-identificadores sobre el que se evalúan k y l.
  # Incluye las geográficas: son cuasi-identificadoras por definición, y
  # dejarlas fuera haría que k y l se midieran sobre menos información de la
  # que efectivamente se publica.
  qid_evaluacion <- unique(c(quasi_id_vars, geo_vars))

  # --- 4-5. Anonimización geográfica por niveles progresivos -----------------

  for (v in geo_vars) {
    q <- qi(v)
    n1 <- qi(paste0(v, "_nivel1"))
    n2 <- qi(paste0(v, "_nivel2"))
    n3 <- qi(paste0(v, "_nivel3"))

    # Nivel 1: código completo. Nivel 2: 3 primeros dígitos.
    # Nivel 3: 2 primeros dígitos.
    nivel_expr <- function(n) {
      paste0(
        "CASE WHEN ", q, " IS NULL THEN NULL ",
        "     WHEN LENGTH(CAST(", q, " AS VARCHAR)) >= ", n, " ",
        "     THEN SUBSTR(CAST(", q, " AS VARCHAR), 1, ", n, ") || ",
        "          REPEAT('*', LENGTH(CAST(", q, " AS VARCHAR)) - ", n, ") ",
        "     ELSE CAST(", q, " AS VARCHAR) END"
      )
    }

    rebuild(paste0(
      "SELECT * REPLACE (CAST(", q, " AS VARCHAR) AS ", q, "), ",
      "CAST(", q, " AS VARCHAR) AS ", n1, ", ",
      nivel_expr(3), " AS ", n2, ", ",
      nivel_expr(2), " AS ", n3,
      " FROM datos"
    ))

    # Calcular K y L para cada nivel, agrupando por el resto de los
    # cuasi-identificadores (incluidas las otras geográficas ya procesadas)
    for (nivel in c("nivel1", "nivel2", "nivel3")) {
      nivel_var <- paste0(v, "_", nivel)
      calcular_kl(
        c(setdiff(qid_evaluacion, v), nivel_var),
        sensitive_var,
        paste0("K_", nivel_var),
        paste0("L_", nivel_var)
      )
    }

    k1 <- qi(paste0("K_", v, "_nivel1"))
    l1 <- qi(paste0("L_", v, "_nivel1"))
    k2 <- qi(paste0("K_", v, "_nivel2"))
    l2 <- qi(paste0("L_", v, "_nivel2"))
    k3 <- qi(paste0("K_", v, "_nivel3"))
    l3 <- qi(paste0("L_", v, "_nivel3"))

    cumple1 <- paste0(k1, " >= ", k, " AND ", l1, " >= ", l)
    cumple2 <- paste0(k2, " >= ", k, " AND ", l2, " >= ", l)
    cumple3 <- paste0(k3, " >= ", k, " AND ", l3, " >= ", l)

    # Nivel óptimo: el menos anonimizado que cumpla k y l
    final_expr <- paste0(
      "CASE ",
      "WHEN ", cumple1, " THEN ", n1, " ",
      "WHEN ", cumple2, " THEN ", n2, " ",
      "WHEN ", cumple3, " THEN ", n3, " ",
      "WHEN ", q, " IS NOT NULL THEN REPEAT('*', LENGTH(", q, ")) ",
      "ELSE NULL END"
    )

    # Conteo por nivel: excluyente y por prioridad
    nivel_counts <- dbGetQuery(
      con,
      paste0(
        "SELECT ",
        "SUM(CASE WHEN ", cumple1, " THEN 1 ELSE 0 END) AS nivel1, ",
        "SUM(CASE WHEN NOT(", cumple1, ") AND ", cumple2,
        " THEN 1 ELSE 0 END) AS nivel2, ",
        "SUM(CASE WHEN NOT(", cumple1, ") AND NOT(", cumple2, ") AND ", cumple3,
        " THEN 1 ELSE 0 END) AS nivel3 ",
        "FROM datos"
      )
    )
    max_anon <- n_registros -
      nivel_counts$nivel1 -
      nivel_counts$nivel2 -
      nivel_counts$nivel3

    resumen$vars_geo_anonimizadas <- c(resumen$vars_geo_anonimizadas, v)
    resumen$nivel_anonimizacion[[v]] <- c(
      nivel1 = nivel_counts$nivel1,
      nivel2 = nivel_counts$nivel2,
      nivel3 = nivel_counts$nivel3,
      max_anon = max_anon
    )

    # Reemplazar la columna original con el nivel óptimo y soltar temporales
    cols_temp_geo <- c(
      paste0(v, "_nivel1"), paste0(v, "_nivel2"), paste0(v, "_nivel3"),
      paste0("K_", v, "_nivel1"), paste0("L_", v, "_nivel1"),
      paste0("K_", v, "_nivel2"), paste0("L_", v, "_nivel2"),
      paste0("K_", v, "_nivel3"), paste0("L_", v, "_nivel3")
    )
    keep_cols <- if (eliminar_temporales) {
      setdiff(get_cols(), c(cols_temp_geo, v))
    } else {
      setdiff(get_cols(), v)
    }

    # Con eliminar_temporales = FALSE se conserva también <var>_final, para
    # poder inspeccionar el nivel elegido junto a los K/L que lo justifican.
    extra_final <- if (eliminar_temporales) {
      ""
    } else {
      paste0(", ", final_expr, " AS ", qi(paste0(v, "_final")))
    }

    rebuild(paste0(
      "SELECT ", paste(qi(keep_cols), collapse = ", "), ", ",
      final_expr, " AS ", q, extra_final, " FROM datos"
    ))
  }

  # --- 6. Calcular K y L sobre todos los cuasi-identificadores ---------------

  calcular_kl(qid_evaluacion, sensitive_var, "k_valor", "l_valor")

  # --- 7. Supresión iterativa -------------------------------------------------

  vars_anonimizadas <- character(0)
  candidatas <- setdiff(quasi_id_vars, c(geo_vars, vars_edad_agrupadas))

  repeat {
    hay_no_cumplen <- dbGetQuery(
      con,
      paste0(
        "SELECT COUNT(*) AS n FROM datos WHERE k_valor < ", k,
        " OR l_valor < ", l
      )
    )$n

    if (hay_no_cumplen == 0) break

    if (length(candidatas) == 0) {
      warning(
        "No quedan variables para suprimir y aún hay ", hay_no_cumplen,
        " registros que no cumplen k = ", k, " o l = ", l,
        ". Revisar el resultado antes de publicar."
      )
      break
    }

    var_a_anon <- candidatas[1]
    tipo <- get_tipo(var_a_anon)

    if (grepl("VARCHAR|TEXT|CHAR", tipo, ignore.case = TRUE)) {
      anon_expr <- paste0(
        "CASE WHEN k_valor < ", k, " OR l_valor < ", l,
        " THEN '***' ELSE ", qi(var_a_anon), " END AS ", qi(var_a_anon)
      )
    } else {
      anon_expr <- paste0(
        "CASE WHEN k_valor < ", k, " OR l_valor < ", l,
        " THEN NULL ELSE ", qi(var_a_anon), " END AS ", qi(var_a_anon)
      )
    }

    # Rebuild sin k_valor/l_valor y con la variable ya suprimida
    other_cols <- setdiff(get_cols(), c(var_a_anon, "k_valor", "l_valor"))

    rebuild(paste0(
      "SELECT ", paste(qi(other_cols), collapse = ", "), ", ",
      anon_expr, " FROM datos"
    ))

    vars_anonimizadas <- c(vars_anonimizadas, var_a_anon)
    candidatas <- setdiff(candidatas, var_a_anon)

    calcular_kl(qid_evaluacion, sensitive_var, "k_valor", "l_valor")
  }

  if (length(vars_anonimizadas) > 0) {
    resumen$otras_vars_anonimizadas <- vars_anonimizadas
  }

  # --- 8. Limpiar columnas temporales ----------------------------------------

  if (eliminar_temporales) {
    cols_finales <- get_cols()
    temp_patterns <- c(
      "_nivel\\d+$", "^K_.*_nivel\\d+$", "^L_.*_nivel\\d+$",
      "_final$", "^k_valor$", "^l_valor$"
    )
    vars_temp <- c()
    for (p in temp_patterns) {
      vars_temp <- c(vars_temp, grep(p, cols_finales, value = TRUE))
    }
    vars_temp <- unique(c(vars_temp, intersect(vars_a_eliminar, cols_finales)))

    if (length(vars_temp) > 0) {
      keep <- setdiff(cols_finales, vars_temp)
      rebuild(paste0(
        "SELECT ", paste(qi(keep), collapse = ", "), " FROM datos"
      ))
    }
  }

  # --- 9. Escribir resultado --------------------------------------------------

  n_final <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM datos")$n

  formato <- if (grepl("\\.csv$", archivo_salida, ignore.case = TRUE)) {
    "(FORMAT CSV, HEADER)"
  } else {
    "(FORMAT PARQUET)"
  }
  dbExecute(con, paste0("COPY datos TO ", qs(archivo_salida), " ", formato))

  # --- 10. Resumen ------------------------------------------------------------

  imprimir_resumen_duckdb(resumen, k, l, n_final)

  invisible(resumen)
}

# Resumen en consola. Idéntico al de anonimizador.R; se duplica para que este
# archivo se pueda copiar solo.
imprimir_resumen_duckdb <- function(resumen, k, l, n_final) {
  cat("\n=== RESUMEN DE ANONIMIZACIÓN ===\n")
  cat("Dataset original:", resumen$n_registros, "registros\n")

  if (length(resumen$id_vars_eliminados) > 0) {
    cat(
      "Identificadores eliminados:",
      paste(resumen$id_vars_eliminados, collapse = ", "),
      "\n"
    )
  }

  if (length(resumen$pseudo_id_vars) > 0) {
    cat(
      "Variables pseudonimizadas (hash):",
      paste(resumen$pseudo_id_vars, collapse = ", "),
      "\n"
    )
  }

  if (length(resumen$edad_vars_agrupadas) > 0) {
    cat(
      "Variables de edad agrupadas:",
      paste(resumen$edad_vars_agrupadas, collapse = ", "),
      "\n"
    )
  }

  if (length(resumen$vars_geo_anonimizadas) > 0) {
    cat("Variables geográficas anonimizadas:\n")
    for (var in resumen$vars_geo_anonimizadas) {
      nivel_info <- resumen$nivel_anonimizacion[[var]]
      total <- sum(nivel_info)
      cat("  -", var, ":\n")
      cat(
        "     Sin anonimización: ",
        nivel_info["nivel1"],
        " registros (",
        round(nivel_info["nivel1"] / total * 100, 1),
        "%)\n",
        sep = ""
      )
      cat(
        "     Nivel 2 (3 primeros dígitos): ",
        nivel_info["nivel2"],
        " registros (",
        round(nivel_info["nivel2"] / total * 100, 1),
        "%)\n",
        sep = ""
      )
      cat(
        "     Nivel 3 (2 primeros dígitos): ",
        nivel_info["nivel3"],
        " registros (",
        round(nivel_info["nivel3"] / total * 100, 1),
        "%)\n",
        sep = ""
      )
      cat(
        "     Máxima anonimización: ",
        nivel_info["max_anon"],
        " registros (",
        round(nivel_info["max_anon"] / total * 100, 1),
        "%)\n",
        sep = ""
      )
    }
  }

  if (length(resumen$otras_vars_anonimizadas) > 0) {
    cat(
      "Otras variables anonimizadas:",
      paste(resumen$otras_vars_anonimizadas, collapse = ", "),
      "\n"
    )
  }

  cat("Parámetros utilizados: k =", k, ", l =", l, "\n")
  cat("Registros en dataset anonimizado:", n_final, "\n")
  cat("===================================\n\n")

  invisible(resumen)
}
