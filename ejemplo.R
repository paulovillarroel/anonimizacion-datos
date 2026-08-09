# =============================================================================
# Ejemplo reproducible del anonimizador
#
# Se genera una base sintética en el propio script, de modo que corre siempre y
# en cualquier máquina, sin depender de descargas externas. Los datos imitan la
# forma de un registro de egresos: comunas de tamaños muy dispares, que es la
# condición que hace trabajar a la escalera de anonimización territorial.
#
# Para usarlo con datos propios, reemplaza la sección 1 por la lectura de tu
# archivo y ajusta los nombres de variables en las llamadas.
#
# USO:  Rscript ejemplo.R
# =============================================================================

library(dplyr)
library(stringr)

source("anonimizador.R")

# --- 1. Base sintética -------------------------------------------------------
# Códigos únicos territoriales reales (2 dígitos de región, 1 de provincia,
# 2 de comuna). Los tamaños son deliberadamente heterogéneos: comunas grandes
# junto a otras pequeñas dentro de la misma provincia.
set.seed(2024)

# Cada provincia agrupa varias comunas: es lo que permite que generalizar
# 05506 a 055** sirva de algo. Una comuna sola en su región no tiene con quién
# fusionarse, y la escalera no puede ayudarla.
comunas <- c(
  # Región 13, provincia 131
  "13101" = 4000, # Santiago
  "13123" = 2500, # Las Condes
  "13110" = 1500, # La Florida
  # Región 05, provincia 051
  "05101" = 1200, # Valparaíso
  "05109" = 900,  # Viña del Mar
  # Región 05, provincia 055
  "05502" = 400,  # Quillota
  "05506" = 150,  # La Cruz
  # Región 08, provincia 081
  "08101" = 800,  # Concepción
  "08103" = 200,  # Hualpén
  # Región 09, provincia 091
  "09101" = 600,  # Temuco
  "09115" = 120,  # Vilcún
  # Región 11, provincia 111
  "11101" = 150,  # Coyhaique
  "11102" = 60    # Lago Verde
)

n <- sum(comunas)

egresos <- tibble(
  # Identificadores directos: se eliminan
  rut = sprintf("%08d-%d", sample(1e6:2.5e7, n, replace = TRUE),
                sample(0:9, n, replace = TRUE)),
  nombre_paciente = sprintf("Paciente %05d", seq_len(n)),

  # Identificador a pseudonimizar: permite seguir a la persona sin publicarla
  id_paciente = sprintf("PAC%06d", sample(1:(n * 0.8), n, replace = TRUE)),

  # Cuasi-identificadores
  cod_comuna = rep(names(comunas), times = comunas),
  sexo_nombre = sample(c("Hombre", "Mujer"), n, replace = TRUE),
  edad_cant = pmin(round(rgamma(n, shape = 6, scale = 11)), 104),
  prevision = sample(c("FONASA", "ISAPRE", "Otra"), n, replace = TRUE,
                     prob = c(0.75, 0.20, 0.05)),

  # Variable sensible
  diagnostico = sample(sprintf("CIE-%03d", 1:25), n, replace = TRUE)
) |>
  slice_sample(prop = 1)

cat("Base sintética:", nrow(egresos), "registros,", length(comunas), "comunas\n")

# Nota: la base NO incluye el nombre de la comuna, a propósito. Publicar el
# nombre junto al código anula la anonimización territorial: de nada sirve
# enmascarar 13101 como 131** si en la columna de al lado dice "Santiago".
# Si necesitas nombres en el resultado, agrégalos DESPUÉS de anonimizar,
# derivándolos del código ya enmascarado.

# --- 2. Anonimización en memoria ---------------------------------------------
# cod_comuna va en geo_vars porque es un código jerárquico y admite la escalera
# de niveles. Un nombre de comuna, en cambio, iría en quasi_id_vars: truncarlo
# por la derecha no produce una jerarquía útil.
egresos_anonimizados <- anonimizar(
  data = egresos,
  id_vars = c("rut", "nombre_paciente"),
  pseudo_id_vars = c("id_paciente"),
  quasi_id_vars = c("sexo_nombre", "edad_cant", "prevision"),
  geo_vars = c("cod_comuna"),
  sensitive_var = "diagnostico",
  k = 3,
  l = 2,
  # El salt fijo hace que el ejemplo sea reproducible. En produccion NUNCA en el
  # codigo: leelo del entorno, p. ej. salt = Sys.getenv("ANON_SALT"), y no lo
  # subas al repositorio.
  salt = "ejemplo-reproducible"
)

cat("\nPrimeras filas del resultado:\n")
print(head(egresos_anonimizados, 4))

# --- 3. Verificación ---------------------------------------------------------
# El resumen dice qué se hizo, no que el resultado cumpla. Hay que comprobarlo
# sobre la base final, agrupando por TODOS los cuasi-identificadores.
QID <- c("sexo_nombre", "edad_cant_grupo", "prevision", "cod_comuna")

verificar <- function(df) {
  df |>
    group_by(across(all_of(QID))) |>
    summarise(K = n(), L = n_distinct(diagnostico), .groups = "drop")
}

v <- verificar(egresos_anonimizados)
cat("\nVerificacion sobre el resultado:\n")
cat("  grupos:", nrow(v), "| K minimo:", min(v$K), "| L minimo:", min(v$L),
    "| grupos bajo el umbral:", sum(v$K < 3 | v$L < 2), "\n")

# --- 4. Qué hacer con los grupos que quedaron bajo el umbral -----------------
# Es esperable que queden algunos: el procedimiento de la norma no garantiza
# por sí solo que todos los grupos publicados alcancen k (ver la sección
# "Limitación conocida" del README). Descartarlos es supresión de registros,
# una técnica estándar. Como los grupos se definen por los valores de los
# cuasi-identificadores, quitar un grupo entero no afecta a los demás.
conformes <- v |> filter(K >= 3, L >= 2) |> select(all_of(QID))

egresos_publicables <- egresos_anonimizados |> semi_join(conformes, by = QID)

descartados <- nrow(egresos_anonimizados) - nrow(egresos_publicables)
vp <- verificar(egresos_publicables)

cat("\nTras descartar los grupos bajo el umbral:\n")
cat("  registros:", nrow(egresos_publicables), "de", nrow(egresos_anonimizados),
    sprintf("(se descarta %.2f%%)\n", 100 * descartados / nrow(egresos_anonimizados)))
cat("  K minimo:", min(vp$K), "| L minimo:", min(vp$L),
    "| grupos bajo el umbral:", sum(vp$K < 3 | vp$L < 2), "\n")

# --- 5. Tramos de edad a medida ----------------------------------------------
# Por defecto son quinquenios. La norma no prescribe tramos concretos, asi que
# se pasan los cortes que correspondan al caso de uso.
invisible(anonimizar(
  data = egresos,
  id_vars = c("rut", "nombre_paciente"),
  quasi_id_vars = c("sexo_nombre", "edad_cant", "prevision"),
  geo_vars = c("cod_comuna"),
  sensitive_var = "diagnostico",
  k = 3,
  l = 2,
  edad_rangos = c(0, 15, 30, 45, 60, 75, 90) # -> 0-14, 15-29, ... 90+
))

# --- 6. La misma anonimización sobre archivo, sin cargar en RAM --------------
# Misma lógica y mismo resultado, para bases que no caben en memoria.
if (requireNamespace("duckdb", quietly = TRUE)) {
  library(duckdb)
  library(DBI)
  source("anonimizador_duckdb.R")

  con <- dbConnect(duckdb())
  duckdb_register(con, "egresos", egresos)
  dbExecute(con, "COPY egresos TO 'egresos.parquet' (FORMAT PARQUET)")
  dbDisconnect(con, shutdown = TRUE)

  anonimizar_duckdb(
    archivo_entrada = "egresos.parquet",
    archivo_salida = "egresos_anonimizados.parquet",
    id_vars = c("rut", "nombre_paciente"),
    pseudo_id_vars = c("id_paciente"),
    quasi_id_vars = c("sexo_nombre", "edad_cant", "prevision"),
    geo_vars = c("cod_comuna"),
    sensitive_var = "diagnostico",
    k = 3,
    l = 2,
    salt = "ejemplo-reproducible" # ver nota de arriba
  )

  unlink(c("egresos.parquet", "egresos_anonimizados.parquet"))
}
