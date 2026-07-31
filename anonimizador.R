library(dplyr)
library(stringr)
# Nota: readr y janitor son usados en ejemplo.R para cargar/limpiar datos,
# pero no directamente por la función anonimizar.

# ==============================================================================
# Anonimización k-anonimidad + l-diversidad (implementación en memoria, dplyr)
#
# Sigue la metodología de la "Norma técnica de anonimización para la publicación
# de bases de datos como datos abiertos" (DEIS, MINSAL Chile), incluida en este
# repositorio.
#
# Para volúmenes que no caben en RAM existe `anonimizador_duckdb.R`, que aplica
# exactamente la misma lógica sobre archivos Parquet/CSV sin cargar los datos
# en R.
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

# Función para anonimizar datasets según documento MINSAL
anonimizar <- function(
  data, # El dataset a anonimizar
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
  # Validación de parámetros
  if (!is.data.frame(data)) {
    stop("El parámetro 'data' debe ser un data frame.")
  }

  if (!is.numeric(k) || k < 1 || !is.numeric(l) || l < 1) {
    stop("Los valores de k y l deben ser enteros positivos (>= 1).")
  }

  if (!sensitive_var %in% names(data)) {
    stop(paste(
      "La variable sensible",
      sensitive_var,
      "no existe en el dataset."
    ))
  }

  # Verificar quasi-identificadores
  missing_vars <- quasi_id_vars[!quasi_id_vars %in% names(data)]
  if (length(missing_vars) > 0) {
    stop(paste(
      "Las siguientes variables cuasi-identificadoras no existen en el dataset:",
      paste(missing_vars, collapse = ", ")
    ))
  }

  # Verificar que las variables geo existen
  if (!is.null(geo_vars) && length(geo_vars) > 0) {
    missing_geo_vars <- geo_vars[!geo_vars %in% names(data)]
    if (length(missing_geo_vars) > 0) {
      stop(paste(
        "Las siguientes variables geográficas especificadas no existen en el dataset:",
        paste(missing_geo_vars, collapse = ", ")
      ))
    }
  }

  etiquetas <- etiquetas_edad(edad_rangos)

  # Crear variables para el resumen
  resumen <- list(
    n_registros = nrow(data),
    id_vars_eliminados = NULL,
    pseudo_id_vars = NULL,
    edad_vars_agrupadas = NULL,
    vars_geo_anonimizadas = NULL,
    nivel_anonimizacion = list(),
    otras_vars_anonimizadas = NULL
  )

  # Copia del dataset para no modificar el original
  df <- data

  # Variables originales que serán reemplazadas
  vars_a_eliminar <- character(0)

  # 1. Eliminar identificadores explícitos
  if (!is.null(id_vars)) {
    id_vars_presentes <- intersect(id_vars, names(df))
    if (length(id_vars_presentes) > 0) {
      df <- df |> select(-all_of(id_vars_presentes))
      resumen$id_vars_eliminados <- id_vars_presentes
    }
  }

  # 2. Pseudonimización (hash md5 truncado, con salt)
  # A diferencia de id_vars, conserva la columna para poder seguir a un mismo
  # sujeto dentro del dataset sin publicar su identificador.
  if (!is.null(pseudo_id_vars)) {
    pseudo_presentes <- intersect(pseudo_id_vars, names(df))
    if (length(pseudo_presentes) > 0) {
      if (!requireNamespace("digest", quietly = TRUE)) {
        stop("La pseudonimización requiere el paquete 'digest'.")
      }
      if (is.null(salt)) {
        salt <- paste(sample(c(letters, LETTERS, 0:9), 16), collapse = "")
      }
      hasher <- digest::getVDigest(algo = "md5")

      for (var in pseudo_presentes) {
        valores <- as.character(df[[var]])
        hashed <- rep(NA_character_, length(valores))
        no_na <- !is.na(valores)
        if (any(no_na)) {
          hashed[no_na] <- substr(
            hasher(paste0(salt, valores[no_na]), serialize = FALSE),
            1,
            12
          )
        }
        df[[var]] <- hashed
      }

      resumen$pseudo_id_vars <- pseudo_presentes
    }
  }

  # 3. Agrupar variables de edad en tramos
  # Solo las cuasi-identificadoras numéricas cuyo nombre contenga "edad".
  vars_edad_agrupadas <- character(0)
  vars_numericas <- quasi_id_vars[sapply(df[quasi_id_vars], is.numeric)]

  for (var in vars_numericas) {
    if (grepl("edad", var, ignore.case = TRUE)) {
      # La variable original se elimina al final
      vars_a_eliminar <- c(vars_a_eliminar, var)

      nombre_grupo <- paste0(var, "_grupo")
      df[[nombre_grupo]] <- as.character(cut(
        df[[var]],
        breaks = c(edad_rangos, Inf),
        labels = etiquetas,
        right = FALSE
      ))

      quasi_id_vars <- c(setdiff(quasi_id_vars, var), nombre_grupo)
      vars_edad_agrupadas <- c(vars_edad_agrupadas, nombre_grupo)
      resumen$edad_vars_agrupadas <- c(resumen$edad_vars_agrupadas, var)
    }
  }

  # Conjunto de cuasi-identificadores sobre el que se evalúan k y l.
  # Incluye las geográficas: son cuasi-identificadoras por definición, y
  # dejarlas fuera haría que k y l se midieran sobre menos información de la
  # que efectivamente se publica.
  geo_vars <- if (is.null(geo_vars)) character(0) else geo_vars
  qid_evaluacion <- unique(c(quasi_id_vars, geo_vars))

  # --- Anonimización geográfica y supresión progresiva ------------------------
  # Sigue el orden de la norma técnica: la geografía se degrada por niveles,
  # y si eso no basta se sacrifican primero las otras cuasi-identificadoras
  # (sexo, previsión, ...), después el tramo etario, y solo al final se enmascara
  # el código territorial por completo.
  #
  # Cada supresión obliga a recalcular los niveles desde cero, porque colapsar
  # una categoría cambia el tamaño de los grupos y puede devolver la geografía
  # a un nivel menos anonimizado. Por eso se conserva el código original aparte.

  for (v in geo_vars) {
    df[[paste0(v, "_orig")]] <- as.character(df[[v]])
    df[[v]] <- df[[paste0(v, "_orig")]]
  }

  # Orden de sacrificio: cuasi-identificadoras comunes primero, tramo etario
  # después. La geografía no entra: tiene su propia escalera de niveles.
  candidatas <- c(
    setdiff(quasi_id_vars, c(geo_vars, vars_edad_agrupadas)),
    vars_edad_agrupadas
  )

  # Calcula los tres niveles de una variable geográfica y marca, por registro,
  # el menos anonimizado que cumple k y l. Devuelve el df con las columnas
  # <var>_nivel1/2/3, K_*, L_*, <var>_final y <var>_cumple.
  calcular_niveles_geo <- function(df_in, var_geo) {
    orig <- paste0(var_geo, "_orig")

    df_in <- df_in |>
      mutate(
        !!paste0(var_geo, "_nivel1") := .data[[orig]],
        !!paste0(var_geo, "_nivel2") := case_when(
          is.na(.data[[orig]]) ~ NA_character_,
          nchar(.data[[orig]]) >= 3 ~
            paste0(
              str_sub(.data[[orig]], 1, 3),
              str_dup("*", nchar(.data[[orig]]) - 3)
            ),
          TRUE ~ .data[[orig]]
        ),
        !!paste0(var_geo, "_nivel3") := case_when(
          is.na(.data[[orig]]) ~ NA_character_,
          nchar(.data[[orig]]) >= 2 ~
            paste0(
              str_sub(.data[[orig]], 1, 2),
              str_dup("*", nchar(.data[[orig]]) - 2)
            ),
          TRUE ~ .data[[orig]]
        )
      )

    # Agrupar por el resto de los cuasi-identificadores (incluidas las otras
    # geográficas, que ya llevan su valor vigente) más el nivel evaluado
    resto <- setdiff(qid_evaluacion, var_geo)
    for (nivel in c("nivel1", "nivel2", "nivel3")) {
      nivel_var <- paste0(var_geo, "_", nivel)
      df_in <- df_in |>
        group_by(across(all_of(c(resto, nivel_var)))) |>
        mutate(
          !!paste0("K_", nivel_var) := n(),
          !!paste0("L_", nivel_var) := n_distinct(.data[[sensitive_var]])
        ) |>
        ungroup()
    }

    cumple1 <- df_in[[paste0("K_", var_geo, "_nivel1")]] >= k &
      df_in[[paste0("L_", var_geo, "_nivel1")]] >= l
    cumple2 <- df_in[[paste0("K_", var_geo, "_nivel2")]] >= k &
      df_in[[paste0("L_", var_geo, "_nivel2")]] >= l
    cumple3 <- df_in[[paste0("K_", var_geo, "_nivel3")]] >= k &
      df_in[[paste0("L_", var_geo, "_nivel3")]] >= l

    df_in[[paste0(var_geo, "_final")]] <- case_when(
      cumple1 ~ df_in[[paste0(var_geo, "_nivel1")]],
      cumple2 ~ df_in[[paste0(var_geo, "_nivel2")]],
      cumple3 ~ df_in[[paste0(var_geo, "_nivel3")]],
      TRUE ~ NA_character_
    )
    df_in[[paste0(var_geo, "_cumple")]] <- cumple1 | cumple2 | cumple3

    # Valor vigente para las pasadas siguientes y para agrupar las otras geo
    df_in[[var_geo]] <- df_in[[paste0(var_geo, "_final")]]

    attr(df_in, "cumples") <- list(c1 = cumple1, c2 = cumple2, c3 = cumple3)
    df_in
  }

  calcular_kl_global <- function(df_in) {
    df_in |>
      group_by(across(all_of(qid_evaluacion))) |>
      mutate(
        k_valor = n(),
        l_valor = n_distinct(.data[[sensitive_var]])
      ) |>
      ungroup()
  }

  vars_anonimizadas <- character(0)
  cumples_finales <- list()

  repeat {
    if (length(geo_vars) > 0) {
      falla <- rep(FALSE, nrow(df))
      for (v in geo_vars) {
        df <- calcular_niveles_geo(df, v)
        cumples_finales[[v]] <- attr(df, "cumples")
        falla <- falla | !df[[paste0(v, "_cumple")]]
      }
    } else {
      df <- calcular_kl_global(df)
      falla <- df$k_valor < k | df$l_valor < l
    }

    if (!any(falla)) break
    if (length(candidatas) == 0) break

    var_a_suprimir <- candidatas[1]
    if (is.numeric(df[[var_a_suprimir]])) {
      df[[var_a_suprimir]] <- ifelse(falla, NA_real_, df[[var_a_suprimir]])
    } else {
      df[[var_a_suprimir]] <- ifelse(
        falla,
        "***",
        as.character(df[[var_a_suprimir]])
      )
    }
    vars_anonimizadas <- c(vars_anonimizadas, var_a_suprimir)
    candidatas <- candidatas[-1]
  }

  # Máxima anonimización: lo que no se resolvió con las supresiones anteriores
  for (v in geo_vars) {
    orig <- df[[paste0(v, "_orig")]]
    final <- df[[paste0(v, "_final")]]
    df[[v]] <- if_else(
      is.na(final) & !is.na(orig),
      str_dup("*", nchar(orig)),
      final
    )

    cc <- cumples_finales[[v]]
    nivel1_count <- sum(cc$c1, na.rm = TRUE)
    nivel2_count <- sum(!cc$c1 & cc$c2, na.rm = TRUE)
    nivel3_count <- sum(!cc$c1 & !cc$c2 & cc$c3, na.rm = TRUE)

    resumen$vars_geo_anonimizadas <- c(resumen$vars_geo_anonimizadas, v)
    resumen$nivel_anonimizacion[[v]] <- c(
      nivel1 = nivel1_count,
      nivel2 = nivel2_count,
      nivel3 = nivel3_count,
      max_anon = nrow(df) - nivel1_count - nivel2_count - nivel3_count
    )
  }

  if (length(vars_anonimizadas) > 0) {
    resumen$otras_vars_anonimizadas <- vars_anonimizadas
  }

  # Verificación final sobre el conjunto completo de cuasi-identificadores
  df <- calcular_kl_global(df)
  incumplen <- sum(df$k_valor < k | df$l_valor < l)
  if (incumplen > 0) {
    warning(
      "Quedan ", incumplen, " registros que no cumplen k = ", k, " o l = ", l,
      " tras agotar las variables disponibles. Revisar antes de publicar."
    )
  }

  # Soltar las copias del código territorial original
  cols_orig <- paste0(geo_vars, "_orig")
  cols_orig <- intersect(cols_orig, names(df))
  if (length(cols_orig) > 0) {
    df <- df |> select(-all_of(cols_orig))
  }
  # 8. Limpiar variables temporales si se solicita
  if (eliminar_temporales) {
    # Eliminar variables temporales creadas en el proceso
    temp_patterns <- c(
      "_nivel\\d+$",
      "^K_.*_nivel\\d+$",
      "^L_.*_nivel\\d+$",
      "_final$",
      "_cumple$",
      "^k_valor$",
      "^l_valor$"
    )

    vars_temp <- c()
    for (pattern in temp_patterns) {
      vars_temp <- c(vars_temp, grep(pattern, names(df), value = TRUE))
    }

    # Eliminar también las variables originales que fueron transformadas
    vars_temp <- unique(c(vars_temp, intersect(vars_a_eliminar, names(df))))

    if (length(vars_temp) > 0) {
      df <- df |> select(-all_of(vars_temp))
    }
  }

  # 9. Mostrar resumen de la anonimización
  imprimir_resumen(resumen, k, l, nrow(df))

  # 10. Devolver el dataset anonimizado
  return(df)
}

# Resumen en consola, compartido por ambas implementaciones.
imprimir_resumen <- function(resumen, k, l, n_final) {
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
