# Cargar los datos
defunciones_covid <- read_csv2(
  "https://datos.gob.cl/dataset/8982a05a-91f7-422d-97bc-3eee08fde784/resource/8e5539b7-10b2-409b-ae5a-36dae4faf817/download/defunciones_covid19_2020_2024.csv"
) |>
  janitor::clean_names()

# Aplicar anonimización
defunciones_anonimizadas <- anonimizar(
  data = defunciones_covid,
  quasi_id_vars = c("sexo_nombre", "edad_cant", "cod_comuna", "comuna"),
  sensitive_var = "codigo_subcategoria_diag1",
  k = 3,
  l = 2
)

# Ver resultados
head(defunciones_anonimizadas)
