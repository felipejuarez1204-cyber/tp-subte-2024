# 06 - Respuesta de cada estación a cada aumento, según su perfil
# Calcula la variación de pasajeros de cada estación (10 días hábiles antes vs. 10 después de cada aumento),
# la cruza con el perfil de la estación (script 05) y compara los tres perfiles con Kruskal-Wallis.
# Guarda: tablas en output/tablas/ y los gráficos exploratorio y g10 en output/graficos/

library(tidyverse)
library(lubridate)
library(ggrepel)
library(here)

source(here("utils", "constantes.R"))
source(here("utils", "tema_tp.R"))
source(here("utils", "guardar_grafico.R"))
source(here("utils", "guardar_tabla.R"))
source(here("utils", "formatear_pct.R"))
source(here("utils", "ventana_habil.R"))
source(here("utils", "limpiar_estaciones.R"))

# 1. Datos guardados por los scripts anteriores ---------------------------------------------------
datos <- read_csv(here("input", "agregado_estacion_hora.csv")) %>%
  mutate(fecha = as.Date(FECHA)) %>%
  limpiar_estaciones()

validos <- read_csv(here("input", "dias_diarios.csv")) %>% filter(valido) %>% arrange(fecha)
perfil <- read_csv(here("input", "perfil_estaciones.csv"))
var_real <- read_csv(here("output", "tablas", "variacion_real_tarifa.csv")) %>%
  slice(match(c("17/5", "1/6", "1/10"), aumento)) %>% pull(var_real_pct)

# pasajeros por estación y día
est_dia <- datos %>%
  group_by(estacion, fecha) %>%
  summarise(pax = sum(pax_total), .groups = "drop")

# 2. Respuesta de cada estación a cada aumento ----------------------------------------
# 10 días hábiles antes vs. 10 días hábiles después (igual que en la entrega 2)
variacion <- function(f) {
  v <- ventana_habil(validos, f)
  antes <- v$fecha[v$etapa == "antes"]
  despues <- v$fecha[v$etapa == "despues"]
  est_dia %>%
    filter(fecha %in% c(antes, despues)) %>%
    group_by(estacion) %>%
    summarise(variacion = 100 * (mean(pax[fecha %in% despues]) / mean(pax[fecha %in% antes]) - 1))
}

# título de cada panel, con la variación real de la tarifa calculada en el script 03
paneles <- c(paste0("17/5: de $125 a $574\ntarifa real ", formatear_pct(var_real[1])),
             paste0("1/6: de $574 a $650\ntarifa real ", formatear_pct(var_real[2])),
             paste0("1/10: de $650 a $757\ntarifa real ", formatear_pct(var_real[3], 1)))

resp <- bind_rows(
  variacion(aumentos[1]) %>% mutate(aumento = paneles[1]),
  variacion(aumentos[2]) %>% mutate(aumento = paneles[2]),
  variacion(aumentos[3]) %>% mutate(aumento = paneles[3]))

graf <- resp %>%
  inner_join(perfil, by = "estacion") %>%
  filter(is.finite(variacion)) %>%
  mutate(aumento = factor(aumento, levels = paneles),
         x = 100 * ratio_finde)

medianas <- graf %>%
  group_by(aumento) %>%
  summarise(mediana = median(variacion))

# etiqueto solo las 2 estaciones que más cayeron y las 2 que menos cayeron (o más subieron) de cada panel
extremos <- graf %>%
  group_by(aumento) %>%
  filter(rank(variacion, ties.method = "first") <= 2 | rank(-variacion, ties.method = "first") <= 2) %>%
  ungroup() %>%
  mutate(etiqueta = str_replace(estacion, "^([A-H]) - (.*)$", "\\2 (\\1)"))   # "D - Plaza Italia" -> "Plaza Italia (D)"

# 3. Gráfico -----------------------------------------------------------------------------

# un color y una forma para cada perfil (se distinguen también en escala de grises)
colores <- c("Origen / pico de la mañana" = "#1B4965",
             "Centro / pico de la tarde" = "#F4A261",
             "Mixto" = "#52B69A")
formas <- c("Origen / pico de la mañana" = 21,
            "Centro / pico de la tarde" = 24,
            "Mixto" = 22)

grafico <- ggplot(graf, aes(x = x, y = variacion)) +
  geom_hline(yintercept = 0, colour = "grey25", linewidth = 0.5) +
  geom_hline(data = medianas, aes(yintercept = mediana), linetype = "dashed",
             colour = c(acento, gris, gris), linewidth = 0.6) +
  geom_vline(xintercept = median(graf$x), linetype = "dashed", colour = gris, linewidth = 0.6) +
  geom_point(aes(fill = perfil_uso, shape = perfil_uso, size = habil),
             colour = "white", stroke = 0.5, alpha = 0.55) +                     # alpha bajo: se ven los puntos superpuestos
  geom_point(data = extremos, aes(size = habil), shape = 21, fill = NA, colour = txt, stroke = 0.9) +   # aro negro
  geom_text_repel(data = medianas, aes(x = -Inf, y = mediana, label = paste0("mediana ", round(mediana, 1), "%")),
                  inherit.aes = FALSE, hjust = 0, size = 2.8, fontface = "bold", colour = "grey30",
                  bg.colour = "white", bg.r = 0.2, direction = "y", nudge_y = -2.5, segment.colour = NA) +
  geom_text_repel(data = extremos, aes(label = etiqueta), size = 3.2, fontface = "bold", colour = txt,
                  bg.colour = "white", bg.r = 0.2,                  # halo blanco para que se lea sobre los puntos
                  segment.colour = txt, segment.size = 0.4, min.segment.length = 0,
                  box.padding = 0.7, point.padding = 0.6, max.overlaps = Inf, seed = 18) +
  facet_wrap(~aumento) +
  scale_fill_manual(values = colores) +
  scale_shape_manual(values = formas) +
  scale_size_area(max_size = 7) +
  scale_x_continuous(breaks = seq(20, 80, 20), labels = scales::label_number(suffix = "%")) +
  scale_y_continuous(labels = scales::label_number(suffix = "%")) +
  guides(fill = guide_legend(override.aes = list(size = 4, alpha = 1)),
         shape = guide_legend(override.aes = list(size = 4, alpha = 1)),
         size = "none") +
  labs(title = "Variación de pasajeros de cada estación tras cada aumento del subte, según su perfil de uso",
       subtitle = str_wrap(paste0("Cada punto es una estación (n = ", n_distinct(graf$estacion), "). Eje y: variación del promedio de pasajeros por día hábil entre los 10 días posteriores y los 10 anteriores a cada aumento (-20% = tuvo 20% menos). ",
                                  "Línea negra: sin cambio; punteadas: medianas; aro y nombre: las estaciones con mayor y menor variación."), 160),
       x = "Uso de fin de semana: pasajeros de sábados y domingos como % de un día hábil (a la izquierda, estaciones de oficina; a la derecha, más recreativas)",
       y = "Variación de pasajeros por día hábil (%)\n(10 días después vs. 10 días antes)",
       fill = "Perfil de la estación (tamaño del punto: pasajeros por día hábil)", shape = "Perfil de la estación (tamaño del punto: pasajeros por día hábil)",
       caption = str_wrap("Fuente: elaboración propia en base a validaciones en molinetes (GCBA, Buenos Aires Data) y tarifas de SBASE / Boletín Oficial de la Ciudad. Días hábiles sin feriados ni atípicos. Perfiles (PCA + clustering jerárquico) calculados desde el 11/3.", 200)) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 8.5), legend.text = element_text(size = 8.5),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#E9ECF0"),
        panel.spacing = unit(1, "lines"),
        axis.text = element_text(colour = gris),
        axis.title = element_text(colour = txt, size = 9),
        strip.text = element_text(face = "bold", colour = txt, size = 10),
        plot.title = element_text(face = "bold", size = 12.5, colour = txt),
        plot.subtitle = element_text(size = 8.5, colour = "#4A5563", lineheight = 1.2),
        plot.caption = element_text(size = 7, hjust = 0, colour = "#5B6573"),
        plot.title.position = "plot", plot.caption.position = "plot")

guardar_grafico(grafico, "grupo_18_exploratorio.jpg", 26, 16)

# 6. Test de Kruskal-Wallis por perfil ----------------------------------------------------------------
# test de Kruskal-Wallis: ¿responden distinto los tres perfiles? (sin Plaza Italia, afectada por eventos)
sin_pi <- graf %>% filter(!str_detect(estacion, "Plaza Italia"))
tabla_kw <- sin_pi %>%
  group_by(aumento) %>%
  summarise(n = n(), H = kruskal.test(variacion ~ perfil_uso)$statistic,
            p = kruskal.test(variacion ~ perfil_uso)$p.value, .groups = "drop") %>%
  mutate(p_holm = p.adjust(p, method = "holm"), epsilon2 = (H - 3 + 1) / (n - 3))
medianas_perfil <- sin_pi %>% group_by(aumento, perfil_uso) %>% summarise(mediana = median(variacion), n = n(), .groups = "drop")
print(tabla_kw); print(medianas_perfil)
guardar_tabla(tabla_kw, "kruskal_wallis.csv"); guardar_tabla(medianas_perfil, "medianas_por_perfil.csv")

mayo <- sin_pi %>% filter(aumento == paneles[1]) %>%
  mutate(perfil_uso = factor(perfil_uso, levels = c("Centro / pico de la tarde", "Origen / pico de la mañana", "Mixto")))
g10 <- ggplot(mayo, aes(perfil_uso, variacion)) +
  geom_boxplot(outlier.shape = NA, fill = "#E8EDF3", colour = gris) +
  geom_jitter(aes(colour = perfil_uso), width = 0.15, size = 1.8, alpha = 0.75, show.legend = FALSE) +
  geom_text(data = mayo %>% group_by(perfil_uso) %>% summarise(m = median(variacion), .groups = "drop"),
            aes(perfil_uso, -5.2, label = paste0("mediana ", format(round(m, 1), decimal.mark = ","), "%")),
            inherit.aes = FALSE, size = 3, fontface = "bold") +
  coord_cartesian(ylim = c(-30, -4)) +
  scale_colour_manual(values = colores) +
  scale_x_discrete(labels = function(x) str_replace(x, " / ", "\n")) +
  scale_y_continuous(labels = scales::label_number(suffix = "%")) +
  labs(title = "Respuesta al aumento de mayo según el perfil de la estación", x = NULL,
       y = "Variación de pasajeros tras el aumento del 17/5",
       caption = str_wrap(paste0("Fuente: elaboración propia en base a validaciones en molinetes (GCBA, Buenos Aires Data). 87 estaciones (sin Plaza Italia). Kruskal-Wallis: p = ",
                        format(round(tabla_kw$p[1], 3), decimal.mark = ",")), 90)) +
  theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), plot.title.position = "plot",
        plot.title = element_text(face = "bold", size = 11), plot.caption = element_text(size = 7, hjust = 0, colour = "#5B6573"),
        plot.caption.position = "plot")
guardar_grafico(g10, "g10_perfil_respuesta.jpg", 13, 9.5)

# comparaciones de a pares entre perfiles en mayo (Mann-Whitney con corrección de Holm)
pares <- pairwise.wilcox.test(mayo$variacion, mayo$perfil_uso, p.adjust.method = "holm", exact = FALSE)
print(pares)
guardar_tabla(as.data.frame(as.table(pares$p.value)), "pares_perfiles_mayo.csv")
