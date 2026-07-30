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
4. **Degradación progresiva**, en el orden que fija la norma:
   1. Para cada variable de `geo_vars`, se aplica el nivel menos restrictivo que cumpla k y l.
   2. Si quedan registros sin resolver, se suprime la primera cuasi-identificadora común (`"***"` si es texto, `NA` si es numérica) **solo en esos registros**, y se recalculan los niveles geográficos desde cero: colapsar una categoría agranda los grupos y puede devolver la geografía a un nivel menos anonimizado.
   3. Se repite con las siguientes cuasi-identificadoras, dejando el **tramo etario para el final**.
   4. Solo cuando ya no quedan variables por sacrificar, los códigos territoriales que siguen sin resolverse pasan a máxima anonimización (`*****`).

   El orden importa: la norma sacrifica sexo y tramo etario **antes** que la geografía completa, de modo que el territorio se preserva todo lo posible.
5. **Verificación final**: se recalculan k y l sobre el conjunto completo de cuasi-identificadoras. Si quedan registros incumpliendo, emite un `warning`.
6. **Limpieza** de las columnas de trabajo.

### Limitación conocida

El K de cada nivel geográfico se cuenta sobre **todos** los registros que comparten ese nivel, pero después solo algunos terminan usándolo, así que el grupo publicado puede ser más chico que el contado. Ejemplo con k=10:

| Comuna | K en nivel 1 | Nivel elegido |
|---|---|---|
| 13101 | 16 | nivel 1 → `13101` |
| 13102 | 4 | nivel 2 → `131**` |

`K_nivel2` es 20 (los 16 más los 4), así que 13102 "cumple" y baja a `131**`. Pero los 16 de 13101 se quedaron en nivel 1, de modo que el grupo real de `131**` tiene K=4, por debajo de k.

Es un comportamiento heredado del procedimiento de la norma. La **verificación final** lo detecta y avisa: si aparece el `warning`, revisa la salida antes de publicar. Con k=2 o k=3 —los valores de la norma— es poco frecuente; se vuelve visible con k altos.

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
