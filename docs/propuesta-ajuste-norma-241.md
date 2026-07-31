# Propuesta de ajuste al procedimiento de anonimización territorial de la Norma Técnica N° 241

**Dirigido a:** Departamento de Estadísticas e Información de Salud (DEIS), Ministerio de Salud de Chile
**Asunto:** El K que justifica el nivel de anonimización del código de comuna no es el K que finalmente se publica
**Documento de referencia:** *Norma técnica de anonimización para la publicación de bases de datos como datos abiertos* (N° 241), anexo con la implementación en R

> **Aporte externo e independiente.** Este documento no proviene del Ministerio de Salud ni del DEIS, ni cuenta con su patrocinio o aval. Es una observación hecha a título personal por quien implementó el procedimiento de la norma en un proyecto propio, y se comparte por si resulta de utilidad para una eventual actualización del anexo. La decisión sobre su pertinencia corresponde enteramente al Departamento.

---

## Resumen

El anexo en R de la norma asigna a cada registro el menor nivel de anonimización del código de comuna que cumpla con K y L mínimos, y a continuación afirma:

> "Notemos que si se cumple con que `cod_comuna_final` no tenga elementos vacíos, la anonimización estará lista ya que cada grupo tendrá un K y un L mayor o igual a 2."

**Esta afirmación no se sostiene en general.** El K de cada nivel se calcula sobre todos los registros que *comparten* ese nivel, pero solo algunos de ellos *terminan publicándose* en él: los demás se quedaron en un nivel menos anonimizado que también cumplía. El grupo que finalmente aparece en la base publicada puede entonces ser más pequeño que el K que justificó su elección, sin que el procedimiento lo advierta.

Se propone un ajuste acotado: **medir el K sobre el agrupamiento que realmente se va a publicar, e iterar hasta que ningún registro necesite moverse.**

En la verificación realizada, el ajuste lleva a **cero** los registros publicados bajo el umbral sin detección, en todos los valores de k evaluados. No descarta más información de la que corresponde: reclasifica los casos problemáticos hacia la vía que la norma ya tiene prevista para ellos.

---

## 1. Alcance

La observación aplica **solo al paso de asignación de niveles del código de comuna** del anexo en R.

No afecta a la definición de k-anonimidad ni de l-diversidad, ni a los umbrales, ni al resto del procedimiento: eliminación de identificadores directos, generalización de la edad, supresión progresiva de `sexo` y `grupo_edad`, y máscara total como descarte final. Todo eso se mantiene igual.

## 2. Qué hace hoy el procedimiento

El anexo construye tres niveles de anonimización del código único territorial de 5 dígitos:

| Nivel | Valor para `13101` | Información que conserva |
|:---:|---|---|
| 1 | `13101` | Región, provincia y comuna |
| 2 | `131**` | Región y provincia |
| 3 | `13***` | Solo región |

Para cada nivel calcula K y L por separado, agrupando por las demás cuasi-identificadoras más el valor de ese nivel:

```r
base = base %>%
  group_by(sexo, grupo_edad, cod_comuna_primer_nivel) %>%
  mutate(K_primer_nivel = n(),
         L_primer_nivel = n_distinct(ENO)) %>%
  ungroup() %>%
  # ídem para el segundo y el tercer nivel
```

Y elige, por registro, el primer nivel que cumpla ambos umbrales:

```r
base = base %>%
  mutate(cod_comuna_final = case_when(
    K_primer_nivel  >= 2 & L_primer_nivel  >= 2 ~ cod_comuna_primer_nivel,
    K_segundo_nivel >= 2 & L_segundo_nivel >= 2 ~ cod_comuna_segundo_nivel,
    K_tercer_nivel  >= 2 & L_tercer_nivel  >= 2 ~ cod_comuna_tercer_nivel,
    TRUE ~ NA_character_
  ))
```

Los registros que quedan en `NA` son los "no resueltos", y son los que activan los pasos siguientes: suprimir `sexo`, reevaluar, suprimir `grupo_edad`, reevaluar, y finalmente aplicar `*****`.

## 3. El problema, en una frase

> Los tres valores de K se calculan **como si todos los registros de cada nivel fueran a publicarse en ese nivel**, pero cada registro elige su nivel por separado, así que el grupo publicado es solo un subconjunto del que se contó.

`K_segundo_nivel` cuenta a todos los registros de la provincia. Si una parte de ellos cumple ya en el nivel 1, se queda ahí, y al nivel 2 llegan solo los demás.

## 4. Ejemplo mínimo

Dos comunas de la misma provincia, mismo sexo y mismo grupo de edad. **k = 10, l = 2.**

| Comuna | Registros | K en nivel 1 | K en nivel 2 (`131**`) | K en nivel 3 (`13***`) |
|---|---:|---:|---:|---:|
| `13101` | 16 | 16 | 20 | 20 |
| `13102` | 4 | 4 | 20 | 20 |

### 4.1 Qué ocurre con el procedimiento actual

| Registros | Evaluación | Nivel asignado | Valor publicado |
|---|---|---|---|
| Los 16 de `13101` | `K_nivel1` = 16 ≥ 10 ✔ | Nivel 1 | `13101` |
| Los 4 de `13102` | `K_nivel1` = 4 < 10 ✘ → `K_nivel2` = 20 ≥ 10 ✔ | Nivel 2 | `131**` |

Base publicada:

| Valor publicado | Registros que lo comparten | ¿Cumple k = 10? |
|---|---:|---|
| `13101` | 16 | Sí |
| `131**` | **4** | **No** |

Los 20 registros que justificaron el nivel 2 nunca coexistieron en él: 16 se quedaron en el nivel 1. El grupo `131**` queda con K = 4.

Y como `cod_comuna_final` no tiene elementos vacíos, la verificación que propone el anexo —`table(is.na(base$cod_comuna_final))`— da conforme. **El incumplimiento pasa inadvertido.**

### 4.2 Qué ocurriría con el ajuste propuesto

Se mide el K del agrupamiento real en cada pasada, y solo suben de nivel los que no cumplen:

| Pasada | Nivel de los 16 | Nivel de los 4 | Grupos reales | Acción |
|:---:|---|---|---|---|
| 1 | 1 → `13101` | 1 → `13102` | `13101`: K=16 ✔ · `13102`: K=4 ✘ | Suben los 4 |
| 2 | 1 → `13101` | 2 → `131**` | `13101`: K=16 ✔ · `131**`: K=4 ✘ | Suben los 4 |
| 3 | 1 → `13101` | 3 → `13***` | `13101`: K=16 ✔ · `13***`: K=4 ✘ | Suben los 4 |
| 4 | 1 → `13101` | sin resolver | — | Nadie puede subir: termina |

Resultado: los 16 se publican como `13101`, y los 4 quedan **sin resolver** (`cod_comuna_final = NA`).

Ese `NA` es exactamente la señal que la norma ya usa: activa la supresión de `sexo`, luego la de `grupo_edad`, y por último la máscara total. El caso deja de pasar inadvertido y entra al circuito previsto para él.

En este ejemplo concreto los 4 registros son irreductibles —son demasiado pocos para k = 10—, así que terminarán en `*****` o requerirán una decisión editorial. Lo relevante es que **el procedimiento lo dice**, en vez de publicarlos como `131**` aparentando conformidad.

## 5. Cuantificación

Base sintética de 5.655 registros, 10 comunas en 3 regiones, con tamaños comunales deliberadamente heterogéneos (de 25 a 4.000 registros). Cuasi-identificadoras: sexo y grupo de edad. Variable sensible: ENO. l = 2 en todos los casos. Se aísla el paso de asignación de niveles, sin los pasos de supresión, para no mezclar efectos.

**Registros bajo el umbral que el procedimiento no detecta** (se publican aparentando cumplir), y registros marcados como no resueltos:

| k | Norma: no detectados | Norma: sin resolver | Propuesta: no detectados | Propuesta: sin resolver |
|---:|---:|---:|---:|---:|
| 2 | 3 | 0 | **0** | 3 |
| 3 | 17 | 0 | **0** | 17 |
| 5 | 32 | 0 | **0** | 32 |
| 10 | 52 | 30 | **0** | 82 |
| 25 | 216 | 135 | **0** | 351 |

Los números se leen así: con k = 25, el procedimiento actual publica 216 registros en grupos bajo el umbral sin advertirlo, y marca otros 135 como no resueltos. Con el ajuste, los 351 quedan marcados como no resueltos y ninguno se publica con la apariencia de cumplir.

**El ajuste no descarta más información: la reclasifica.** Los registros que hoy se publican con un detalle territorial que la garantía no respalda pasan a la vía de supresión que la norma ya define, donde se recuperan al menos parcialmente al colapsar `sexo` y `grupo_edad`.

Con k = 2 —el umbral base de la norma— el efecto es marginal: 3 registros sobre 5.655. Crece con k.

### 5.1 Condición que lo activa

El fenómeno requiere **heterogeneidad de tamaño entre comunas que comparten prefijo**: una comuna suficientemente grande para cumplir por sí sola junto a otras que no. Es la situación habitual en la geografía chilena, donde una comuna cabecera concentra buena parte de la población provincial.

## 6. El ajuste propuesto

### 6.1 Algoritmo

1. Asignar a todos los registros el nivel 1.
2. Construir el valor territorial que corresponde al nivel asignado a cada registro.
3. Calcular K y L agrupando por las demás cuasi-identificadoras más ese valor. **Este es el cambio**: el K se mide sobre el agrupamiento que efectivamente se publicaría.
4. Los registros cuyo grupo no alcanza los umbrales y que aún están en el nivel 1 o 2 suben un nivel. Los que ya están en el nivel 3 quedan marcados como no resueltos.
5. Si ningún registro cambió, terminar. Si no, volver al paso 2.

La iteración siempre termina: el nivel de cada registro solo puede aumentar y está acotado por el nivel 3. En la verificación convergió en 4 pasadas en todos los casos.

### 6.2 Código de reemplazo para el anexo

Sustituye únicamente al bloque `mutate(cod_comuna_final = case_when(...))`. Todo lo anterior y lo posterior del anexo se mantiene.

```r
# Los tres niveles, del menos al mas anonimizado
niveles <- list(
  codigo_comuna,
  paste0(str_sub(codigo_comuna, 1, 3), str_dup("*", nchar(codigo_comuna) - 3)),
  paste0(str_sub(codigo_comuna, 1, 2), str_dup("*", nchar(codigo_comuna) - 2))
)

# Nivel asignado a cada registro; 4 = no resuelto (equivale a NA)
nivel <- rep(1L, nrow(base))

repeat {
  # Valor territorial vigente segun el nivel de cada registro
  valor <- niveles[[1]]
  for (i in 2:3) valor[nivel == i] <- niveles[[i]][nivel == i]
  valor[nivel == 4L] <- NA_character_

  # K y L del agrupamiento que realmente se publicaria
  kl <- base %>%
    mutate(.valor = valor) %>%
    group_by(sexo, grupo_edad, .valor) %>%
    mutate(.K = n(), .L = n_distinct(ENO)) %>%
    ungroup()

  # Degradar solo a quienes no cumplen y todavia pueden bajar de detalle
  subir <- (kl$.K < 2 | kl$.L < 2) & nivel < 4L
  if (!any(subir)) break
  nivel[subir] <- nivel[subir] + 1L
}

base <- base %>% mutate(cod_comuna_final = valor)
```

Al terminar, `cod_comuna_final` queda en `NA` para los registros no resueltos, igual que en el procedimiento actual, y el anexo continúa sin cambios con `table(is.na(base$cod_comuna_final))`, la supresión de `sexo`, la de `grupo_edad` y la máscara total.

### 6.3 Verificación final que conviene agregar

Con independencia de que se adopte el ajuste, conviene cerrar el procedimiento comprobando directamente sobre la base publicada, en vez de confiar en la ausencia de `NA`:

```r
base %>%
  group_by(sexo, grupo_edad, cod_comuna_final) %>%
  summarise(K = n(), L = n_distinct(ENO), .groups = "drop") %>%
  filter(K < 2 | L < 2)
```

Si devuelve filas, la base no cumple los umbrales declarados y no debería publicarse sin revisión. Esta comprobación es la que permite detectar el problema descrito aquí, y también cualquier otro que deje grupos bajo el umbral.

## 7. Consideraciones de implementación

**Costo computacional.** La iteración implica un `GROUP BY` por pasada, acotado por el número de niveles (4 pasadas en la verificación). El procedimiento actual hace 3 agrupaciones fijas, de modo que el costo es del mismo orden.

**Bases grandes.** Para volúmenes que no caben en memoria, el algoritmo se traduce sin dificultad a SQL: cada pasada es una agregación y una actualización de la columna de nivel.

**Compatibilidad.** El ajuste no cambia la interfaz ni los umbrales. Una base procesada con el ajuste es válida bajo el procedimiento actual; la inversa no siempre.

## 8. Origen de esta observación

Surgió al implementar el procedimiento de la norma en dos variantes —una en `dplyr` en memoria y otra sobre DuckDB para bases grandes— y agregar una verificación final de k y l sobre el conjunto completo de cuasi-identificadoras. Esa verificación empezó a reportar grupos bajo el umbral en casos donde `cod_comuna_final` no tenía elementos vacíos, lo que llevó a revisar el paso de asignación de niveles.

## 9. Reproducibilidad

El script [`verificacion_propuesta.R`](verificacion_propuesta.R) reproduce todas las cifras de este documento, incluido el ejemplo mínimo de la sección 4. Se ejecuta desde la raíz del repositorio:

```
Rscript docs/verificacion_propuesta.R
```

Requiere únicamente `dplyr` y tiene la semilla fijada.

Las implementaciones, la verificación y este documento están disponibles en <https://github.com/paulovillarroel/anonimizacion-datos>.

Queda ofrecida la disponibilidad para discutir el ajuste, aportar pruebas sobre bases reales o colaborar en la actualización del anexo, según lo que el Departamento estime conveniente.
