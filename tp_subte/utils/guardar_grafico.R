# Guarda un gráfico en output/graficos/ (JPG, 300 dpi, tamaño en cm)
guardar_grafico <- function(grafico, nombre, ancho, alto) {
  dir.create(here::here("output", "graficos"), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(here::here("output", "graficos", nombre), plot = grafico,
                  width = ancho, height = alto, units = "cm", dpi = 300, bg = "white")
}
