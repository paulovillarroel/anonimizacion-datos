# Anonimizador de Datos

## Descripción

Esta herramienta implementa técnicas de k-anonimidad y l-diversidad para anonimizar conjuntos de datos, siguiendo la metodología establecida en la norma técnica de anonimización para la publicación de bases de datos como datos abiertos del Ministerio de Salud de Chile (MINSAL).

Viene en dos implementaciones que aplican **exactamente la misma lógica** y producen el mismo resultado:

| Archivo | Función | Cuándo usarla |
|---|---|---|
| `anonimizador.R` | `anonimizar()` | Trabaja con un data frame en memoria. Es la opción simple para datasets que caben en RAM. |
| `anonimizador_duckdb.R` | `anonimizar_duckdb()` | Trabaja sobre archivos Parquet o CSV íntegramente dentro de DuckDB; los datos nunca se cargan en R. Para datasets grandes (millones de filas). |

Cada archivo es autocontenido: puedes copiar solo el que necesites a tu proyecto.

## Instalación

```r
# Para la versión en memoria
install.packages(c("dplyr", "stringr", "readr", "janitor"))

# Para pseudonimizar (opcional, solo si usas pseudo_id_vars)
install.packages("digest")

# Para la versión sobre archivos
install.packages(c("duckdb", "DBI"))
```

## Uso

```r
source("anonimizador.R")

datos_anonimizados <- anonimizar(
  data = datos,
  id_vars = c("rut", "nombre", "email"),  # Identificadores directos: se eliminan
  pseudo_id_vars = c("id_paciente"),      # Identificadores a reemplazar por hash
  quasi_id_vars = c("sexo", "edad"),      # Cuasi-identificadores
  geo_vars = c("cod_comuna"),             # Códigos territoriales jerárquicos
  sensitive_var = "diagnostico",
  k = 3,
  l = 2
)
```

La versión DuckDB toma las mismas opciones, cambiando el data frame por rutas de archivo:

```r
source("anonimizador_duckdb.R")

anonimizar_duckdb(
  archivo_entrada = "datos.parquet",   # .parquet o .csv
  archivo_salida  = "datos_anon.parquet",
  id_vars = c("rut", "nombre", "email"),
  pseudo_id_vars = c("id_paciente"),
  quasi_id_vars = c("sexo", "edad"),
  geo_vars = c("cod_comuna"),
  sensitive_var = "diagnostico",
  k = 3,
  l = 2
)
```

Ver `ejemplo.R` para un caso completo con datos abiertos reales del DEIS.

## Parámetros

| Parámetro | Por defecto | Descripción |
|---|---|---|
| `data` / `archivo_entrada` + `archivo_salida` | — | Data frame, o rutas de entrada y salida en la versión DuckDB |
| `id_vars` | `NULL` | Identificadores directos. Se **eliminan** del resultado |
| `pseudo_id_vars` | `NULL` | Identificadores a reemplazar por un hash md5 con salt, truncado a 12 caracteres |
| `quasi_id_vars` | — | Variables cuasi-identificadoras que podrían permitir re-identificación |
| `geo_vars` | `NULL` | Códigos territoriales jerárquicos, anonimizados por niveles progresivos |
| `sensitive_var` | — | Variable sensible cuya confidencialidad se quiere proteger |
| `k` | `2` | Valor mínimo para k-anonimidad |
| `l` | `2` | Valor mínimo para l-diversidad |
| `edad_rangos` | `seq(0, 80, by = 5)` | Cortes inferiores de los tramos de edad. El último queda abierto |
| `salt` | `NULL` | Salt del hash. Si es `NULL` se genera uno aleatorio por ejecución |
| `eliminar_temporales` | `TRUE` | Si se eliminan las columnas de trabajo (`K_*`, `L_*`, `*_nivel*`, `k_valor`, `l_valor`) |

k y l se evalúan siempre sobre **todas** las cuasi-identificadoras, incluidas las de `geo_vars`.

### Sobre `geo_vars`

La escalera de tres niveles trunca el código por la derecha, así que solo tiene sentido para **códigos jerárquicos**. El caso previsto es el código único territorial (CUT) de 5 dígitos: 2 de región, 1 de provincia y 2 de comuna. Para `13101`:

| Nivel | Valor | Qué oculta |
|---|---|---|
| 1 | `13101` | Nada |
| 2 | `131**` | La comuna |
| 3 | `13***` | La comuna y la provincia |
| Máxima | `*****` | Todo |

Se aplica, por registro, el nivel menos restrictivo que cumpla k y l.

Los **nombres** de comuna o región no van aquí: truncarlos por la izquierda no produce una jerarquía útil. Pásalos en `quasi_id_vars` y se suprimirán a `"***"` cuando no cumplan k o l.

### Sobre `edad_rangos`

Se pasan los **cortes inferiores** de cada tramo; el último queda siempre abierto. El default son quinquenios:

```r
seq(0, 80, by = 5)           # -> 0-4, 5-9, 10-14, ... 75-79, 80+
c(0, 15, 30, 45, 60, 75, 90) # -> 0-14, 15-29, 30-44, 45-59, 60-74, 75-89, 90+
```

La norma técnica no prescribe tramos concretos: habla de generalización a "rangos etarios" y su único ejemplo concreto usa décadas. La elección queda en manos de quien publica. Tramos más anchos protegen más pero reducen la utilidad analítica.

### Sobre `pseudo_id_vars` y el salt

`id_vars` elimina la columna; `pseudo_id_vars` la conserva como hash, lo que permite seguir a un mismo sujeto **dentro** del dataset sin publicar su identificador.

El salt es lo que hace irreversible el hash. Si no lo pasas, se genera uno aleatorio distinto en cada ejecución, así que dos corridas —o dos subconjuntos procesados por separado— **no son cruzables entre sí**. Si necesitas que sí lo sean, fija el salt explícitamente y **guárdalo en secreto**: nunca en el repositorio, idealmente en una variable de entorno.

Esto es pseudonimización, no anonimización: con el salt en mano el mapeo se puede reconstruir por diccionario.

## Proceso de Anonimización

1. **Eliminación de identificadores directos**: remueve las variables de `id_vars` (RUT, nombre, dirección, etc.).
2. **Pseudonimización**: reemplaza las variables de `pseudo_id_vars` por un hash md5 con salt.
3. **Agrupación de variables de edad**: toda cuasi-identificadora numérica cuyo nombre contenga "edad" se convierte en tramos según `edad_rangos`.
4. **Anonimización de códigos geográficos**: para cada variable de `geo_vars`, aplica el nivel menos restrictivo que cumpla k y l.
5. **Supresión progresiva**: mientras queden registros que no cumplan k o l, suprime una cuasi-identificadora a la vez (`"***"` si es texto, `NA` si es numérica), recalculando k y l tras cada paso. No toca las de `geo_vars` ni el tramo etario. Si se acaban las variables disponibles y aún quedan registros incumpliendo, emite un `warning`.
6. **Limpieza** de las columnas de trabajo.

## Resultado

`anonimizar()` devuelve el data frame anonimizado. `anonimizar_duckdb()` escribe el archivo de salida y devuelve el resumen de forma invisible. Ambas imprimen en consola un resumen:

```
=== RESUMEN DE ANONIMIZACIÓN ===
Dataset original: 58714 registros
Identificadores eliminados: rut, nombre, email
Variables pseudonimizadas (hash): id_paciente
Variables de edad agrupadas: edad_cant
Variables geográficas anonimizadas:
  - cod_comuna:
     Sin anonimización: 52500 registros (89.4%)
     Nivel 2 (3 primeros dígitos): 3200 registros (5.5%)
     Nivel 3 (2 primeros dígitos): 1800 registros (3.1%)
     Máxima anonimización: 1214 registros (2.0%)
Otras variables anonimizadas: sexo_nombre, comuna
Parámetros utilizados: k = 3, l = 2
Registros en dataset anonimizado: 58714
===================================
```

## Consideraciones

- El nivel de protección aumenta con valores mayores de k y l, pero valores muy altos reducen la utilidad de los datos. Evalúa el balance según el caso de uso.
- La detección de variables de edad es **por nombre de columna** (debe contener "edad") y solo aplica a columnas numéricas.
- Si aparece el `warning` de que se acabaron las variables por suprimir, el resultado **no** cumple k o l en todos los grupos. Revísalo antes de publicar.
- Ninguna técnica de este tipo elimina por completo el riesgo de re-identificación. La k-anonimidad no protege frente a atacantes con conocimiento externo del sujeto, y la l-diversidad no cubre todos los ataques de inferencia sobre la variable sensible.

## Referencia

Esta implementación se basa en la *Norma técnica de anonimización para la publicación bases de datos como datos abiertos* del Departamento de Estadísticas e Información de Salud, Ministerio de Salud, Chile (incluida en este repositorio).

## Licencia

[MIT]
