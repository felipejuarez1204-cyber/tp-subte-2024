# Paleta y tema de los gráficos (requiere ggplot2 y scales)

txt <- "#1D2B3A"; gris <- "#8A94A0"; azul <- "#1B4965"; acento <- "#D1495B"

miles <- scales::label_number(big.mark = ".", decimal.mark = ",")
en_mil <- scales::label_number(scale = 1/1000, suffix = " mil", big.mark = ".")

tema_tp <- ggplot2::theme_minimal(base_size = 10) +
  ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                 panel.grid.major.x = ggplot2::element_blank(),
                 panel.grid.major.y = ggplot2::element_line(colour = "#E6E9ED"),
                 axis.text = ggplot2::element_text(colour = gris),
                 axis.title = ggplot2::element_text(colour = txt, size = 9),
                 plot.title = ggplot2::element_text(face = "bold", size = 11, colour = txt),
                 plot.caption = ggplot2::element_text(size = 7, hjust = 0, colour = "#5B6573"),
                 plot.title.position = "plot", plot.caption.position = "plot")
