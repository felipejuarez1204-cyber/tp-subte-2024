# Guarda una tabla de resultados en output/tablas/ (CSV)
guardar_tabla <- function(tabla, nombre) {
  dir.create(here::here("output", "tablas"), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(tabla, here::here("output", "tablas", nombre))
}
