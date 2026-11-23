# Toma los 10 días hábiles válidos anteriores a una fecha y los 10 posteriores
# validos: tabla con una columna "fecha" (solo días hábiles sin atípicos), ordenada por fecha
ventana_habil <- function(validos, f, k = 10) {
  dplyr::bind_rows(
    validos %>% dplyr::filter(fecha < f) %>% dplyr::slice_tail(n = k) %>% dplyr::mutate(etapa = "antes"),
    validos %>% dplyr::filter(fecha >= f) %>% dplyr::slice_head(n = k) %>% dplyr::mutate(etapa = "despues")) %>%
    dplyr::mutate(aumento = paste0(lubridate::day(f), "/", lubridate::month(f)))
}
