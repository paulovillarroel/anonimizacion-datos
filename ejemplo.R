library(tidyverse)

# ==============================================================================
# Ejemplo con datos abiertos reales: defunciones COVID-19 2020-2024 (DEIS)
# ==============================================================================

source("anonimizador.R")

# Cargar los datos
defunciones_covid <- read_csv2(
  "https://datos.gob.cl/dataset/8982a05a-91f7-422d-97bc-3eee08fde784/resource/8e5539b7-10b2-409b-ae5a-36dae4faf817/download/defunciones_covid19_2020_2024.csv"
) |>
  janitor::clean_names()

# --- Opción A: en memoria (dplyr) ---------------------------------------------

# Nota sobre geo_vars: la escalera de tres niveles trunca el código por la
# derecha (13101 -> 131** -> 13***), así que solo tiene sentido para códigos
# jerárquicos como el CUT de 5 dígitos. Los nombres de comuna y región van como
# cuasi-identificadores normales: si no cumplen k o l, se suprimen a "***".
defunciones_anonimizadas <- anonimizar(
  data = defunciones_covid,
  quasi_id_vars = c("sexo_nombre", "edad_cant", "comuna", "nombre_region"),
  sensitive_var = "codigo_subcategoria_diag1",
  geo_vars = c("cod_comuna"), # Código jerárquico: se anonimiza por niveles
  k = 3,
  l = 2
)

# Ver resultados
head(defunciones_anonimizadas)

# Tramos de edad a medida: por defecto son quinquenios (0-4, 5-9, ... 80+).
# La norma no prescribe tramos concretos, así que se pasan los cortes que
# correspondan al caso de uso.
defunciones_tramos_anchos <- anonimizar(
  data = defunciones_covid,
  quasi_id_vars = c("sexo_nombre", "edad_cant", "comuna", "nombre_region"),
  sensitive_var = "codigo_subcategoria_diag1",
  geo_vars = c("cod_comuna"),
  k = 3,
  l = 2,
  edad_rangos = c(0, 15, 30, 45, 60, 75, 90) # -> 0-14, 15-29, ... 90+
)

# --- Opción B: sobre archivo, sin cargar en RAM (DuckDB) ----------------------
# Misma lógica y mismo resultado, pero los datos nunca entran a memoria R.
# Sirve para datasets de millones de filas.

library(duckdb)
library(DBI)
source("anonimizador_duckdb.R")

# Dejar los datos en disco (aquí desde el data frame ya cargado; en un caso real
# apuntarías directamente a tu .parquet o .csv de origen)
con <- dbConnect(duckdb())
duckdb_register(con, "defunciones", defunciones_covid)
dbExecute(con, "COPY defunciones TO 'defunciones.parquet' (FORMAT PARQUET)")
dbDisconnect(con, shutdown = TRUE)

anonimizar_duckdb(
  archivo_entrada = "defunciones.parquet",
  archivo_salida = "defunciones_anonimizadas.parquet",
  quasi_id_vars = c("sexo_nombre", "edad_cant", "comuna", "nombre_region"),
  sensitive_var = "codigo_subcategoria_diag1",
  geo_vars = c("cod_comuna"),
  k = 3,
  l = 2
)

# --- Pseudonimización de un identificador -------------------------------------
# Si necesitas conservar la trazabilidad de una persona dentro del dataset sin
# publicar su identificador, usa pseudo_id_vars en lugar de id_vars.
#
# El salt es lo que hace irreversible el hash. Si no lo pasas, se genera uno
# aleatorio distinto en cada ejecución, así que dos corridas NO son cruzables
# entre sí. Si necesitas que sí lo sean, fija el salt y guárdalo en secreto —
# nunca en el repositorio.
#
# anonimizar(
#   data = mis_datos,
#   id_vars = c("rut", "nombre", "direccion"),  # se eliminan
#   pseudo_id_vars = c("id_paciente"),          # se reemplazan por hash
#   quasi_id_vars = c("sexo", "edad"),
#   geo_vars = c("cod_comuna"),
#   sensitive_var = "diagnostico",
#   k = 3,
#   l = 2,
#   salt = Sys.getenv("ANON_SALT")
# )
