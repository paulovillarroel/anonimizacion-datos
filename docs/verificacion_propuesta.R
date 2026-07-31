# =============================================================================
# Verificación empírica de la propuesta de ajuste a la Norma Técnica N° 241
#
# Aísla el paso de asignación de niveles del código de comuna y compara:
#   (A) el procedimiento vigente de la norma
#   (B) el ajuste propuesto (iteración a punto fijo)
#
# Reproduce las cifras del documento docs/propuesta-ajuste-norma-241.md
# Ejecutar desde la raíz del repositorio:  Rscript docs/verificacion_propuesta.R
# =============================================================================

suppressMessages(library(dplyr))

set.seed(2024)

# --- Datos sintéticos con heterogeneidad de tamaño comunal -------------------
# Es la condición que hace visible el fenómeno: comunas grandes y chicas
# compartiendo el mismo prefijo de provincia.
comunas <- c(
  # Región 13, provincia 131: una comuna grande y varias chicas
  "13101" = 4000, "13102" = 120, "13103" = 90, "13104" = 60,
  # Región 05, provincia 051: tamaños intermedios
  "05101" = 800, "05102" = 300, "05103" = 150,
  # Región 11, provincia 111: todas chicas
  "11101" = 70, "11102" = 40, "11103" = 25
)

base <- tibble(
  cod_comuna = rep(names(comunas), times = comunas),
  sexo = sample(c("Hombre", "Mujer"), sum(comunas), replace = TRUE),
  grupo_edad = sample(
    c("0-14", "15-29", "30-44", "45-59", "60-74", "75+"),
    sum(comunas), replace = TRUE
  ),
  ENO = sample(sprintf("E%02d", 1:15), sum(comunas), replace = TRUE)
)

RESTO <- c("sexo", "grupo_edad")
SENS <- "ENO"

# --- Niveles de anonimización del código territorial -------------------------
nivel_valor <- function(x, n) {
  ifelse(is.na(x), NA_character_,
    ifelse(nchar(x) >= n,
      paste0(substr(x, 1, n), strrep("*", nchar(x) - n)),
      x
    )
  )
}
niveles_de <- function(x) {
  list(x, nivel_valor(x, 3), nivel_valor(x, 2), strrep("*", nchar(x)))
}

# K y L de un vector de valores territoriales, dado el resto de cuasi-ids
kl_de <- function(df, valor, sens) {
  df |>
    mutate(.v = valor) |>
    group_by(across(all_of(c(RESTO, ".v")))) |>
    mutate(.K = n(), .L = n_distinct(.data[[sens]])) |>
    ungroup() |>
    select(.K, .L)
}

# --- (A) Procedimiento vigente de la norma -----------------------------------
# K y L se calculan para cada nivel de forma independiente y se elige el
# primero que cumpla. La máscara total es el descarte final.
asignar_norma <- function(df, k, l) {
  nv <- niveles_de(df$cod_comuna)
  kl <- lapply(nv[1:3], function(v) kl_de(df, v, SENS))
  cumple <- lapply(kl, function(x) x$.K >= k & x$.L >= l)

  idx <- rep(4L, nrow(df)) # por defecto, máscara total
  idx[cumple[[3]]] <- 3L
  idx[cumple[[2]]] <- 2L
  idx[cumple[[1]]] <- 1L
  idx
}

# --- (B) Ajuste propuesto: iteración a punto fijo ----------------------------
# Se asigna un nivel, se mide el K y L del agrupamiento que realmente resulta,
# y se degrada solo a los registros que no cumplen. Se repite hasta que ningún
# registro cambia de nivel.
#
# Igual que la norma, la iteración se detiene en el nivel 3: los registros que
# siguen incumpliendo quedan marcados como no resueltos (índice 4), que es lo
# que en la norma corresponde a cod_comuna_final = NA y dispara la supresión de
# sexo y grupo_edad. La máscara total es el descarte final en ambos casos.
asignar_punto_fijo <- function(df, k, l) {
  nv <- niveles_de(df$cod_comuna)
  idx <- rep(1L, nrow(df))
  iteraciones <- 0L

  repeat {
    iteraciones <- iteraciones + 1L
    valor <- nv[[1]]
    for (i in 2:3) valor[idx == i] <- nv[[i]][idx == i]
    valor[idx == 4L] <- nv[[4]][idx == 4L]

    kl <- kl_de(df, valor, SENS)
    incumple <- kl$.K < k | kl$.L < l
    subir <- incumple & idx < 4L
    if (!any(subir)) break
    idx[subir] <- idx[subir] + 1L
  }
  attr(idx, "iteraciones") <- iteraciones
  idx
}

# --- Evaluación --------------------------------------------------------------
valor_de <- function(df, idx) {
  nv <- niveles_de(df$cod_comuna)
  v <- nv[[1]]
  for (i in 2:4) v[idx == i] <- nv[[i]][idx == i]
  v
}

evaluar <- function(df, idx, k, l) {
  v <- valor_de(df, idx)
  con_kl <- kl_de(df, v, SENS)
  incumple <- con_kl$.K < k | con_kl$.L < l

  g <- df |>
    mutate(.v = v) |>
    group_by(across(all_of(c(RESTO, ".v")))) |>
    summarise(K = n(), L = n_distinct(.data[[SENS]]), .groups = "drop")

  list(
    grupos = nrow(g),
    grupos_incumplen = sum(g$K < k | g$L < l),
    registros_incumplen = sum(incumple),
    # Lo que importa: registros bajo el umbral que el procedimiento NO marca
    # como no resueltos, es decir, que se publican creyendo que cumplen.
    incumplen_no_detectados = sum(incumple & idx < 4L),
    sin_resolver = sum(idx == 4L),
    k_min = min(g$K),
    niveles = as.integer(table(factor(idx, levels = 1:4)))
  )
}

cat("Registros:", nrow(base), "| comunas:", length(comunas), "\n")
cat(strrep("=", 78), "\n")
cat("Registros bajo el umbral que el procedimiento NO detecta\n")
cat("(se publican con la apariencia de cumplir)\n\n")
cat(sprintf("%-4s | %-22s | %-22s\n", "k", "NORMA VIGENTE", "AJUSTE PROPUESTO"))
cat(sprintf("%-4s | %-10s %-11s | %-10s %-11s\n", "",
            "no detect.", "sin resolver", "no detect.", "sin resolver"))
cat(strrep("-", 78), "\n")

resultados <- list()
for (k in c(2, 3, 5, 10, 25)) {
  ia <- asignar_norma(base, k, 2)
  ib <- asignar_punto_fijo(base, k, 2)
  ra <- evaluar(base, ia, k, 2)
  rb <- evaluar(base, ib, k, 2)
  resultados[[as.character(k)]] <- list(norma = ra, propuesta = rb, ib = ib)
  cat(sprintf(
    "%-4d | %-10d %-11d | %-10d %-11d\n",
    k, ra$incumplen_no_detectados, ra$sin_resolver,
    rb$incumplen_no_detectados, rb$sin_resolver
  ))
}

cat(strrep("=", 78), "\n\n")
cat("Distribución de registros por nivel (1=código completo ... 4=máscara total)\n\n")
cat(sprintf("%-4s | %-24s | %-24s\n", "k", "NORMA VIGENTE", "AJUSTE PROPUESTO"))
cat(sprintf("%-4s | %-5s %-5s %-5s %-6s | %-5s %-5s %-5s %-6s\n", "",
            "n1", "n2", "n3", "n4", "n1", "n2", "n3", "n4"))
cat(strrep("-", 78), "\n")
for (k in c(2, 3, 5, 10, 25)) {
  r <- resultados[[as.character(k)]]
  cat(sprintf(
    "%-4d | %-5d %-5d %-5d %-6d | %-5d %-5d %-5d %-6d   (%d iteraciones)\n",
    k, r$norma$niveles[1], r$norma$niveles[2], r$norma$niveles[3], r$norma$niveles[4],
    r$propuesta$niveles[1], r$propuesta$niveles[2], r$propuesta$niveles[3],
    r$propuesta$niveles[4], attr(r$ib, "iteraciones")
  ))
}

cat("\n", strrep("=", 78), "\n")
cat("Caso mínimo reproducible (k = 10, l = 2)\n\n")
minimo <- bind_rows(
  tibble(sexo = "Hombre", grupo_edad = "30-44", cod_comuna = "13101",
         ENO = rep(c("E01", "E02"), each = 8)),
  tibble(sexo = "Hombre", grupo_edad = "30-44", cod_comuna = "13102",
         ENO = c("E01", "E02", "E01", "E02"))
)
im <- asignar_norma(minimo, 10, 2)
vm <- valor_de(minimo, im)
print(minimo |> mutate(cod_comuna_final = vm) |>
        count(cod_comuna, cod_comuna_final, name = "registros"))
cat("\nK del grupo publicado '131**':",
    sum(vm == "131**"), "  (k solicitado: 10)\n")
