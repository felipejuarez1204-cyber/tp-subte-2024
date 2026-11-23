# Viajes en Subte: ¿cuánto respondió la demanda a los aumentos de tarifa de 2024?

**Trabajo Práctico de Ciencia de Datos para Economistas (FCE-UBA), 2.º cuatrimestre de 2026. Entrega final, Grupo 18.**

**Integrantes:** Nohemi Cardenas, Felipe Juarez y Marco Caletti.

## Objetivo e hipótesis

Medimos cómo respondieron los pasajeros del subte de Buenos Aires a los tres aumentos del boleto de 2024 (17/5: de $125 a $574; 1/6: a $650; 1/10: a $757) y qué estaciones respondieron más. El análisis es descriptivo y asociativo: **no es causal**.

- **H1.** Tras cada aumento del boleto, los pasajeros por día hábil son menores que antes.
- **H2.** La demanda cae solo si el aumento es grande en términos reales: mayo (+233% real) sí; junio (+11%) y octubre (−0,7%) acompañan la inflación y no.
- **H3.** La caída no es igual en todas las estaciones: depende de su perfil de uso.

## Base de datos

- **Nombre:** Subte: viajes molinetes 2024.
- **Fuente:** Buenos Aires Data, Gobierno de la Ciudad de Buenos Aires (SBASE): <https://data.buenosaires.gob.ar/dataset/subte-viajes-molinetes>
- **Unidad de observación:** validaciones por molinete, estación e intervalo de 15 minutos.
- **Tamaño y período:** 11.440.440 filas × 10 variables originales; del 1/1/2024 al 31/12/2024 (364 de 366 días con registros).
- **Limitaciones:** solo registra ingresos (no hay destino), cubre un único año (no se separa la estacionalidad), la línea D estuvo cerrada del 8/1 al 7/3, y en diciembre hay molinetes con ID "null" y alias de estaciones.
- **El archivo crudo no está en el repositorio** porque pesa más de 100 MB (límite de GitHub). Las tablas ya agregadas están en `input/`; ver `raw/LEEME.md`.

### Bases auxiliares (`auxiliar/`)

| Archivo | Contenido | Fuente |
|---|---|---|
| `tarifas_subte_2024.csv` | Tarifa del boleto de subte y fecha de cada cambio | SBASE / Boletín Oficial de la Ciudad |
| `ipc_mensual_2024.csv` | Inflación mensual de 2024 (para deflactar la tarifa) | INDEC |
| `feriados_2024.csv` | Feriados y puentes de 2024 | Calendario oficial (argentina.gob.ar) |
| `brecha_subte_colectivo.csv` | Boleto de subte y boleto mínimo de colectivo AMBA ($270 desde febrero) | Notas de prensa |

Los cruces se hacen por fecha y no generan pérdida de observaciones.

## Análisis realizado

1. **Faltantes y atípicos.** El único faltante es el molinete (158.033 registros con texto "null", 1,38%, todos en diciembre): no se imputa porque no entra al análisis. La línea D cerrada y los días sin servicio (9/5 y 30/10) son faltantes estructurales: no se interpolan. Los atípicos se detectan con 1,5 × IQR dentro de cada día de la semana (19 días: 15 feriados y puentes, el 24/3, las vísperas del 24 y 31/12 y el 31/10). Se muestran las descriptivas antes y después de la limpieza.
2. **Tarifa real y elasticidad.** Se deflacta el boleto con el IPC mensual y se calcula la variación real de cada aumento.
3. **Mann-Whitney con corrección de Holm.** Compara los 10 días hábiles antes y después de cada aumento (red sin la línea D).
4. **Serie de tiempo interrumpida.** Usa todo el período desde el 18/3 y controla día de la semana, vacaciones de invierno y fin de año, con errores robustos.
5. **HCPC.** Agrupa las 88 estaciones en tres perfiles (origen / pico de la mañana, centro / pico de la tarde y mixto) con PCA y clustering jerárquico.
6. **Kruskal-Wallis.** Compara la respuesta de los tres perfiles.

## Estructura del repositorio

```
tp_subte/
├── tp_subte.Rproj
├── README.md
├── raw/                  # datos crudos (no se suben por su tamaño; ver raw/LEEME.md)
├── auxiliar/             # tarifas, IPC, feriados y brecha con el colectivo
├── input/                # tablas agregadas y tablas intermedias para el análisis
├── output/
│   ├── tablas/           # tablas de resultados
│   └── graficos/         # gráficos (JPG, 300 dpi)
├── script/               # scripts numerados, en orden de ejecución
└── utils/                # funciones y constantes compartidas (una función por script)
```

## Cómo reproducir los resultados

1. Abrir `tp_subte.Rproj` en RStudio (así R trabaja desde la carpeta del proyecto y las rutas relativas funcionan).
2. Instalar los paquetes una sola vez:

```r
install.packages(c("tidyverse", "here", "data.table", "ggrepel", "patchwork",
                   "FactoMineR", "cluster", "lmtest", "sandwich"))
```

3. Correr los scripts de `script/` **en este orden**. Cada uno guarda sus resultados y el siguiente los lee desde los archivos guardados.

| Script | Qué hace | Qué genera |
|---|---|---|
| `01_agregar_datos.R` | Resume la base cruda de `raw/` (se saltea si `raw/` está vacía) | `input/agregado_diario_linea.csv`, `input/agregado_estacion_hora.csv`, `input/nulos_molinete.csv` |
| `02_descriptivas_limpieza.R` | Descriptivas, faltantes, atípicos y efecto de la limpieza | `input/dias_diarios.csv`, tablas, gráficos g01 a g06 |
| `03_tarifa_real.R` | Tarifa nominal y real, y variación real de cada aumento | `input/tarifa_real_diaria.csv`, `output/tablas/variacion_real_tarifa.csv`, g11 y g12 |
| `04_inferencia.R` | Mann-Whitney, serie interrumpida y elasticidad | tablas, g07, g08 y el gráfico comunicativo |
| `05_perfiles_estaciones.R` | PCA + HCPC de estaciones | `input/perfil_estaciones.csv`, tablas, g09 |
| `06_perfil_respuesta.R` | Respuesta de cada estación a cada aumento y Kruskal-Wallis | tablas, gráfico exploratorio y g10 |

## Conclusiones principales

- **H1 (parcial).** Tras el aumento de mayo los pasajeros por día hábil bajaron 16,5% (de 643 mil a 537 mil; p de Holm = 0,0007). Tras los de junio (−0,2%) y octubre (−1,8%) no hay diferencias que se distingan del azar.
- **H2 (se apoya).** La respuesta se ordena con el tamaño real del aumento (+233%, +11% y −0,7%). La elasticidad simple es baja: −0,07 en mayo.
- **H3 (se confirma, con efecto chico).** En mayo las estaciones de oficina cayeron −14,8% (mediana), las de origen −17,1% y las de uso mixto −20,0% (Kruskal-Wallis p = 0,001).
- **Con una serie de tiempo interrumpida**, el salto inmediato de mayo es de −16% a −21% según la especificación, pero el nivel medio posterior queda 9% a 14% abajo: hubo recuperación parcial que, con un solo año, no se puede separar de la estacionalidad.
- **Límites.** No se identifica causalidad, no hay datos de otros modos de transporte y los perfiles tienen una estructura débil (silueta de 0,315).
