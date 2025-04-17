library(dplyr)
library(stringr)
library(readr)
library(janitor)

# Función para anonimizar datasets según documento MINSAL
anonimizar <- function(
  data, # El dataset a anonimizar
  id_vars = NULL, # Variables a eliminar por ser identificadores directos
  quasi_id_vars, # Variables cuasi-identificadoras
  sensitive_var, # Variable sensible
  k = 2, # Valor de k-anonimidad
  l = 2, # Valor de l-diversidad
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

  # Crear variables para el resumen
  resumen <- list(
    n_registros = nrow(data),
    id_vars_eliminados = NULL,
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
    id_vars_presentes <- id_vars[id_vars %in% names(df)]
    if (length(id_vars_presentes) > 0) {
      df <- df |> select(-all_of(id_vars_presentes))
      resumen$id_vars_eliminados <- id_vars_presentes
    }
  }

  # 2. Detectar variables numéricas entre los quasi-identificadores
  vars_numericas <- quasi_id_vars[sapply(df[quasi_id_vars], is.numeric)]

  # 3. Agrupar variables de edad
  for (var in vars_numericas) {
    # Solo agrupar si es variable de edad
    if (grepl("edad", var, ignore.case = TRUE)) {
      # Añadir a la lista de variables a eliminar
      vars_a_eliminar <- c(vars_a_eliminar, var)

      # Rangos para edades
      rangos <- c(0, 15, 30, 45, 60, 75, 90, Inf)
      etiquetas <- c("0-14", "15-29", "30-44", "45-59", "60-74", "75-89", "90+")

      # Crear variable agrupada
      nombre_grupo <- paste0(var, "_grupo")
      df[[nombre_grupo]] <- cut(
        df[[var]],
        breaks = rangos,
        labels = etiquetas,
        include.lowest = TRUE
      )

      # Actualizar quasi_id_vars
      quasi_id_vars <- c(setdiff(quasi_id_vars, var), nombre_grupo)

      # Actualizar resumen
      resumen$edad_vars_agrupadas <- c(resumen$edad_vars_agrupadas, var)
    }
  }

  # 4. Detectar variables geográficas (códigos de región, comuna, etc.)
  vars_geo <- character(0)
  for (var in quasi_id_vars) {
    if (var %in% names(df) && grepl("cod.*com", var, ignore.case = TRUE)) {
      vars_geo <- c(vars_geo, var)
    }
  }

  # 5. Procesar variables geográficas según la lógica del documento
  for (var in vars_geo) {
    # Convertir a carácter si es necesario
    df[[var]] <- as.character(df[[var]])

    # Determinar longitud del código para valores no NA
    chars_no_na <- nchar(df[[var]][!is.na(df[[var]])])
    if (length(chars_no_na) == 0) next

    # Crear los tres niveles de anonimización del código geográfico
    df[[paste0(var, "_nivel1")]] <- df[[var]]

    df[[paste0(var, "_nivel2")]] <- sapply(df[[var]], function(x) {
      if (is.na(x)) return(NA_character_)
      len <- nchar(x)
      if (len >= 3) {
        return(paste0(substr(x, 1, 3), paste(rep("*", len - 3), collapse = "")))
      } else {
        return(x)
      }
    })

    df[[paste0(var, "_nivel3")]] <- sapply(df[[var]], function(x) {
      if (is.na(x)) return(NA_character_)
      len <- nchar(x)
      if (len >= 2) {
        return(paste0(substr(x, 1, 2), paste(rep("*", len - 2), collapse = "")))
      } else {
        return(x)
      }
    })

    # Calcular K y L para cada nivel
    for (nivel in c("nivel1", "nivel2", "nivel3")) {
      nivel_var <- paste0(var, "_", nivel)

      # Variables para agrupar (quasi-ids sin la variable geográfica original + nivel actual)
      vars_agrupar <- c(setdiff(quasi_id_vars, var), nivel_var)

      # Calcular K y L
      df <- df |>
        group_by(across(all_of(vars_agrupar))) |>
        mutate(
          !!paste0("K_", nivel_var) := n(),
          !!paste0("L_", nivel_var) := n_distinct(.data[[sensitive_var]])
        ) |>
        ungroup()
    }

    # Crear la variable final con el nivel óptimo de anonimización
    df[[paste0(var, "_final")]] <- case_when(
      df[[paste0("K_", var, "_nivel1")]] >= k &
        df[[paste0("L_", var, "_nivel1")]] >= l ~
        df[[paste0(var, "_nivel1")]],

      df[[paste0("K_", var, "_nivel2")]] >= k &
        df[[paste0("L_", var, "_nivel2")]] >= l ~
        df[[paste0(var, "_nivel2")]],

      df[[paste0("K_", var, "_nivel3")]] >= k &
        df[[paste0("L_", var, "_nivel3")]] >= l ~
        df[[paste0(var, "_nivel3")]],

      TRUE ~ NA_character_
    )

    # Manejo de máxima anonimización para los NA
    df[[paste0(var, "_final")]] <- sapply(seq_along(df[[var]]), function(i) {
      val_final <- df[[paste0(var, "_final")]][i]
      if (is.na(val_final) && !is.na(df[[var]][i])) {
        return(paste(rep("*", nchar(df[[var]][i])), collapse = ""))
      } else {
        return(val_final)
      }
    })

    # Contar registros por nivel de anonimización para el resumen
    nivel1_count <- sum(
      df[[paste0(var, "_final")]] == df[[paste0(var, "_nivel1")]],
      na.rm = TRUE
    )
    nivel2_count <- sum(
      df[[paste0(var, "_final")]] == df[[paste0(var, "_nivel2")]],
      na.rm = TRUE
    )
    nivel3_count <- sum(
      df[[paste0(var, "_final")]] == df[[paste0(var, "_nivel3")]],
      na.rm = TRUE
    )
    max_anon_count <- nrow(df) - nivel1_count - nivel2_count - nivel3_count

    # Actualizar resumen
    resumen$vars_geo_anonimizadas <- c(resumen$vars_geo_anonimizadas, var)
    resumen$nivel_anonimizacion[[var]] <- c(
      nivel1 = nivel1_count,
      nivel2 = nivel2_count,
      nivel3 = nivel3_count,
      max_anon = max_anon_count
    )

    # Reemplazar la variable original con la versión anonimizada
    df[[var]] <- df[[paste0(var, "_final")]]
  }

  # 6. Calcular k y l para los quasi-identificadores actualizados
  df <- df |>
    group_by(across(all_of(quasi_id_vars))) |>
    mutate(
      k_valor = n(),
      l_valor = n_distinct(.data[[sensitive_var]])
    ) |>
    ungroup()

  # 7. Anonimizar variables que no cumplen criterios
  vars_anonimizadas <- character(0)

  repeat {
    # Verificar si hay registros que no cumplen
    no_cumplen <- any(df$k_valor < k | df$l_valor < l)

    if (!no_cumplen) break

    # Si hay registros que no cumplen, anonimizar una variable
    # Empezar con variable categórica (como sexo)
    vars_categoricas <- setdiff(
      quasi_id_vars,
      c(vars_numericas, vars_geo, grep("_grupo$", quasi_id_vars, value = TRUE))
    )

    if (length(vars_categoricas) > 0) {
      var_a_anonimizar <- vars_categoricas[1]

      # Identificar registros que no cumplen
      registros_no_cumplen <- df$k_valor < k | df$l_valor < l

      # Anonimizar la variable
      if (
        is.character(df[[var_a_anonimizar]]) ||
          is.factor(df[[var_a_anonimizar]])
      ) {
        df[[var_a_anonimizar]] <- ifelse(
          registros_no_cumplen,
          "***",
          as.character(df[[var_a_anonimizar]])
        )
      } else if (is.numeric(df[[var_a_anonimizar]])) {
        df[[var_a_anonimizar]] <- ifelse(
          registros_no_cumplen,
          NA_real_,
          df[[var_a_anonimizar]]
        )
      }

      # Actualizar resumen
      vars_anonimizadas <- c(vars_anonimizadas, var_a_anonimizar)

      # Recalcular k y l
      df <- df |>
        group_by(across(all_of(quasi_id_vars))) |>
        mutate(
          k_valor = n(),
          l_valor = n_distinct(.data[[sensitive_var]])
        ) |>
        ungroup()

      # Eliminar la variable anonimizada de la lista para no volver a procesarla
      quasi_id_vars <- setdiff(quasi_id_vars, var_a_anonimizar)
    } else {
      # Si no hay más variables para anonimizar, romper el ciclo
      break
    }
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
    vars_temp <- c(vars_temp, vars_a_eliminar)

    if (length(vars_temp) > 0) {
      df <- df |> select(-all_of(vars_temp))
    }
  }

  # 9. Mostrar resumen de la anonimización
  cat("\n=== RESUMEN DE ANONIMIZACIÓN ===\n")
  cat("Dataset original:", resumen$n_registros, "registros\n")

  if (!is.null(resumen$id_vars_eliminados)) {
    cat(
      "Identificadores eliminados:",
      paste(resumen$id_vars_eliminados, collapse = ", "),
      "\n"
    )
  }

  if (!is.null(resumen$edad_vars_agrupadas)) {
    cat(
      "Variables de edad agrupadas:",
      paste(resumen$edad_vars_agrupadas, collapse = ", "),
      "\n"
    )
  }

  if (!is.null(resumen$vars_geo_anonimizadas)) {
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

  if (!is.null(resumen$otras_vars_anonimizadas)) {
    cat(
      "Otras variables anonimizadas:",
      paste(resumen$otras_vars_anonimizadas, collapse = ", "),
      "\n"
    )
  }

  cat("Parámetros utilizados: k =", k, ", l =", l, "\n")
  cat("Registros en dataset anonimizado:", nrow(df), "\n")
  cat("===================================\n\n")

  # 10. Devolver el dataset anonimizado
  return(df)
}
