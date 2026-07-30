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

  # --- Helper Function for Geographic Anonymization ---
  # Esta función encapsula la lógica para anonimizar una variable geográfica específica.
  anonimizar_variable_geo <- function(
    df_in,
    var_geo,
    qid_vars,
    sens_var,
    k_val,
    l_val
  ) {
    # Copia local para evitar modificar el df original directamente en la función helper
    df_local <- df_in

    # Convertir a carácter si es necesario
    df_local[[var_geo]] <- as.character(df_local[[var_geo]])

    # Determinar longitud del código para valores no NA
    chars_no_na <- nchar(df_local[[var_geo]][!is.na(df_local[[var_geo]])])
    if (length(chars_no_na) == 0) {
      # Si todos son NA o la variable no existe, devolver sin cambios
      return(list(df = df_local, resumen_geo = NULL))
    }

    # Crear los tres niveles de anonimización
    df_local <- df_local |>
      mutate(
        !!paste0(var_geo, "_nivel1") := .data[[var_geo]],
        !!paste0(var_geo, "_nivel2") := case_when(
          is.na(.data[[var_geo]]) ~ NA_character_,
          nchar(.data[[var_geo]]) >= 3 ~
            paste0(
              str_sub(.data[[var_geo]], 1, 3),
              str_dup("*", nchar(.data[[var_geo]]) - 3)
            ),
          TRUE ~ .data[[var_geo]]
        ),
        !!paste0(var_geo, "_nivel3") := case_when(
          is.na(.data[[var_geo]]) ~ NA_character_,
          nchar(.data[[var_geo]]) >= 2 ~
            paste0(
              str_sub(.data[[var_geo]], 1, 2),
              str_dup("*", nchar(.data[[var_geo]]) - 2)
            ),
          TRUE ~ .data[[var_geo]]
        )
      )

    # Calcular K y L para cada nivel, agrupando por el resto de los
    # cuasi-identificadores (incluidas las otras geográficas ya procesadas)
    quasi_ids_sin_geo_local <- setdiff(qid_vars, var_geo)
    df_local <- df_local |>
      group_by(across(all_of(c(
        quasi_ids_sin_geo_local,
        paste0(var_geo, "_nivel1")
      )))) |>
      mutate(
        !!paste0("K_", var_geo, "_nivel1") := n(),
        !!paste0("L_", var_geo, "_nivel1") := n_distinct(.data[[sens_var]])
      ) |>
      ungroup() |>
      group_by(across(all_of(c(
        quasi_ids_sin_geo_local,
        paste0(var_geo, "_nivel2")
      )))) |>
      mutate(
        !!paste0("K_", var_geo, "_nivel2") := n(),
        !!paste0("L_", var_geo, "_nivel2") := n_distinct(.data[[sens_var]])
      ) |>
      ungroup() |>
      group_by(across(all_of(c(
        quasi_ids_sin_geo_local,
        paste0(var_geo, "_nivel3")
      )))) |>
      mutate(
        !!paste0("K_", var_geo, "_nivel3") := n(),
        !!paste0("L_", var_geo, "_nivel3") := n_distinct(.data[[sens_var]])
      ) |>
      ungroup()

    # Condiciones de cumplimiento por nivel
    cumple1 <- df_local[[paste0("K_", var_geo, "_nivel1")]] >= k_val &
      df_local[[paste0("L_", var_geo, "_nivel1")]] >= l_val
    cumple2 <- df_local[[paste0("K_", var_geo, "_nivel2")]] >= k_val &
      df_local[[paste0("L_", var_geo, "_nivel2")]] >= l_val
    cumple3 <- df_local[[paste0("K_", var_geo, "_nivel3")]] >= k_val &
      df_local[[paste0("L_", var_geo, "_nivel3")]] >= l_val

    # Crear la variable final (_final) con el menor nivel que cumpla k y l
    df_local[[paste0(var_geo, "_final")]] <- case_when(
      cumple1 ~ df_local[[paste0(var_geo, "_nivel1")]],
      cumple2 ~ df_local[[paste0(var_geo, "_nivel2")]],
      cumple3 ~ df_local[[paste0(var_geo, "_nivel3")]],
      !is.na(df_local[[var_geo]]) ~ str_dup("*", nchar(df_local[[var_geo]])),
      TRUE ~ NA_character_
    )

    # Contar registros por nivel para el resumen.
    # Se cuenta por prioridad excluyente sobre las condiciones K/L, no
    # comparando strings: dos niveles pueden coincidir (por ejemplo, en códigos
    # de 3 dígitos el nivel 2 no enmascara nada) y el registro se contaría dos
    # veces, dejando max_anon negativo.
    nivel1_count <- sum(cumple1, na.rm = TRUE)
    nivel2_count <- sum(!cumple1 & cumple2, na.rm = TRUE)
    nivel3_count <- sum(!cumple1 & !cumple2 & cumple3, na.rm = TRUE)
    max_anon_count <- nrow(df_local) -
      nivel1_count -
      nivel2_count -
      nivel3_count

    resumen_geo_local <- list(
      var = var_geo,
      niveles = c(
        nivel1 = nivel1_count,
        nivel2 = nivel2_count,
        nivel3 = nivel3_count,
        max_anon = max_anon_count
      )
    )

    # Reemplazar la variable original con la final en el df local
    df_local[[var_geo]] <- df_local[[paste0(var_geo, "_final")]]

    # Devolver el df modificado y el resumen para esta variable
    return(list(df = df_local, resumen_geo = resumen_geo_local))
  }
  # --- Fin Helper Function ---

  # 4. Procesar variables geográficas (si se especificaron) usando la función helper
  resumen$vars_geo_anonimizadas <- character(0) # Inicializar en resumen
  if (length(geo_vars) > 0) {
    for (var_g in geo_vars) {
      resultado_geo <- anonimizar_variable_geo(
        df_in = df,
        var_geo = var_g, # Usar la variable de geo_vars
        qid_vars = qid_evaluacion, # Todos los cuasi-identificadores
        sens_var = sensitive_var,
        k_val = k,
        l_val = l
      )

      # Actualizar el dataframe principal
      df <- resultado_geo$df

      # Actualizar el resumen general si hubo cambios
      if (!is.null(resultado_geo$resumen_geo)) {
        resumen$vars_geo_anonimizadas <- c(
          resumen$vars_geo_anonimizadas,
          resultado_geo$resumen_geo$var
        )
        resumen$nivel_anonimizacion[[
          resultado_geo$resumen_geo$var
        ]] <- resultado_geo$resumen_geo$niveles
      }

      # Nota: La variable geográfica (var_g) se modifica *en el dataframe df*
      # dentro de anonimizar_variable_geo, conservando su nombre. Las
      # operaciones posteriores operan sobre la columna ya modificada.
    }
  }

  # 5. Calcular k y l iniciales sobre todos los cuasi-identificadores
  # (incluyendo edad agrupada y geo anonimizada)
  df <- df |>
    group_by(across(all_of(qid_evaluacion))) |>
    mutate(
      k_valor = n(),
      l_valor = n_distinct(.data[[sensitive_var]])
    ) |>
    ungroup()

  # 7. Supresión iterativa para cumplir k-anonimidad y l-diversidad
  # Si después de las transformaciones iniciales (edad, geo), aún hay grupos
  # que no cumplen con k o l, se entra en un ciclo para suprimir valores
  # en otras variables cuasi-identificadoras hasta que se cumplan los criterios
  # o no queden más variables por suprimir.
  vars_anonimizadas <- character(0)
  candidatas <- setdiff(quasi_id_vars, c(geo_vars, vars_edad_agrupadas))

  repeat {
    # Verificar si todavía hay registros/grupos que no cumplen k o l
    no_cumplen <- any(df$k_valor < k | df$l_valor < l)

    if (!no_cumplen) {
      # Si todos los grupos cumplen, salir del ciclo
      break
    }

    if (length(candidatas) == 0) {
      # Si no quedan más variables para suprimir y aún no se cumplen los
      # criterios k/l, se detiene el proceso de supresión.
      warning(
        "No quedan variables para suprimir y aún hay grupos que no cumplen ",
        "k = ", k, " o l = ", l, ". Revisar el resultado antes de publicar."
      )
      break
    }

    var_a_anonimizar <- candidatas[1]

    # Identificar registros que no cumplen
    registros_no_cumplen <- df$k_valor < k | df$l_valor < l

    # Anonimizar la variable
    if (is.numeric(df[[var_a_anonimizar]])) {
      df[[var_a_anonimizar]] <- ifelse(
        registros_no_cumplen,
        NA_real_,
        df[[var_a_anonimizar]]
      )
    } else {
      df[[var_a_anonimizar]] <- ifelse(
        registros_no_cumplen,
        "***",
        as.character(df[[var_a_anonimizar]])
      )
    }

    # Actualizar resumen
    vars_anonimizadas <- c(vars_anonimizadas, var_a_anonimizar)

    # Recalcular k y l siempre sobre el mismo conjunto de cuasi-identificadores
    # (ya transformados). Suprimir un valor colapsa categorías, así que el
    # recuento cambia aunque el conjunto de columnas sea fijo.
    df <- df |>
      group_by(across(all_of(qid_evaluacion))) |>
      mutate(
        k_valor = n(),
        l_valor = n_distinct(.data[[sensitive_var]])
      ) |>
      ungroup()

    # No volver a elegir esta variable en la siguiente iteración
    candidatas <- setdiff(candidatas, var_a_anonimizar)
  }

  # Actualizar resumen con variables categóricas anonimizadas
  if (length(vars_anonimizadas) > 0) {
    resumen$otras_vars_anonimizadas <- vars_anonimizadas
  }

  # 8. Limpiar variables temporales si se solicita
  if (eliminar_temporales) {
    # Eliminar variables temporales creadas en el proceso
    temp_patterns <- c(
      "_nivel\\d+$",
      "^K_.*_nivel\\d+$",
      "^L_.*_nivel\\d+$",
      "_final$",
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
