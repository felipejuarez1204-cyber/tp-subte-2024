# 01 - Agregar la base cruda de molinetes
# Lee los CSV crudos de raw/ (11,4 millones de filas) y los resume en tres tablas que se guardan en input/:
#   - agregado_diario_linea.csv   : pasajeros por fecha y línea
#   - agregado_estacion_hora.csv  : pasajeros por fecha, línea, estación y hora
#   - nulos_molinete.csv          : registros y molinetes "null" por línea, estación y mes
# Si raw/ está vacía (el archivo crudo pesa demasiado para GitHub), el script avisa y no hace nada:
# las tres tablas ya están en input/ y los scripts siguientes funcionan igual.
# (data.table se usa solo para leer rápido: no se carga con library() para que no tape funciones de lubridate)

library(tidyverse)
library(lubridate)
library(here)

archivos <- list.files(here("raw"), pattern = "\\.csv$", full.names = TRUE)

if (length(archivos) == 0) {
  message("No hay archivos CSV en raw/: se usan las tablas agregadas que ya están en input/ (ver README).")
} else {
  # nombre de cada columna en la base cruda
  columnas <- c(fecha = "FECHA", desde = "DESDE", linea = "LINEA", molinete = "MOLINETE",
                estacion = "ESTACION", pax = "pax_TOTAL")

  crudo <- data.table::rbindlist(lapply(archivos, data.table::fread, select = unname(columnas)), use.names = TRUE) %>%
    as_tibble()
  names(crudo) <- names(columnas)

  crudo <- crudo %>%
    mutate(fecha = as.Date(fecha),
           hora = as.integer(sub(":.*", "", as.character(desde))),
           mes = month(fecha),
           # el faltante de molinete viene como texto "null" (no como NA): se lo trata como faltante
           nulo = is.na(molinete) | molinete == "" | molinete == "null")

  diario <- crudo %>%
    group_by(FECHA = fecha, LINEA = linea) %>%
    summarise(pax_TOTAL = sum(pax), .groups = "drop")
  estacion_hora <- crudo %>%
    group_by(FECHA = fecha, LINEA = linea, ESTACION = estacion, HORA = hora) %>%
    summarise(pax_total = sum(pax), .groups = "drop")
  nulos <- crudo %>%
    group_by(LINEA = linea, ESTACION = estacion, mes) %>%
    summarise(registros = n(), nulos = sum(nulo), .groups = "drop")

  write_csv(diario, here("input", "agregado_diario_linea.csv"))
  write_csv(estacion_hora, here("input", "agregado_estacion_hora.csv"))
  write_csv(nulos, here("input", "nulos_molinete.csv"))
  message("Listo: tablas agregadas guardadas en input/")
}
