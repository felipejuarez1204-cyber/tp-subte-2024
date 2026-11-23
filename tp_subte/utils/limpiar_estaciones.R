# Unifica los nombres de estación: saca los alias de diciembre (Retiro.C, Callao.D, etc.),
# corrige dos nombres con problemas de codificación y arma la clave "línea - estación"
limpiar_estaciones <- function(datos) {
  datos %>%
    dplyr::mutate(estacion = stringr::str_remove(ESTACION, "\\.[A-H]$"),
                  estacion = dplyr::case_when(
                    stringr::str_detect(estacion, "^Ag.*ero$") ~ "Ag\u00fcero",
                    stringr::str_detect(estacion, "^Saenz") ~ "S\u00e1enz Pe\u00f1a",
                    TRUE ~ estacion),
                  estacion = paste(stringr::str_sub(LINEA, -1), estacion, sep = " - "))
}
