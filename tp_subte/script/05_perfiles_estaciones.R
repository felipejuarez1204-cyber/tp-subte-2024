# 05 - Perfiles de estaciones (HCPC)
# Agrupa las estaciones según su volumen, su uso de fin de semana y sus horarios, con PCA + clustering jerárquico (HCPC).
# Variables estandarizadas; período desde el 11/3 (reapertura de la línea D); se usan las estaciones con más de 250 días de datos.
# Guarda: input/perfil_estaciones.csv, tablas en output/tablas/ y el gráfico g09 en output/graficos/

library(tidyverse)
library(lubridate)
library(FactoMineR)   # PCA y HCPC (clase 15)
library(cluster)      # silueta
library(here)

source(here("utils", "constantes.R"))
source(here("utils", "tema_tp.R"))
source(here("utils", "guardar_grafico.R"))
source(here("utils", "guardar_tabla.R"))
source(here("utils", "limpiar_estaciones.R"))

# 1. Datos ---------------------------------------------------------------------------------
datos <- read_csv(here("input", "agregado_estacion_hora.csv")) %>%
  mutate(fecha = as.Date(FECHA)) %>%
  limpiar_estaciones()

# días hábiles válidos (sin feriados ni atípicos), definidos en el script 02
dias_habiles <- read_csv(here("input", "dias_diarios.csv")) %>% filter(valido) %>% pull(fecha)

# pasajeros por estación y día
est_dia <- datos %>%
  group_by(estacion, fecha) %>%
  summarise(pax = sum(pax_total), .groups = "drop")

# 2. Perfil de cada estación (desde que reabrió la línea D) --------------------------
desde <- as.Date("2024-03-11")

perfil <- est_dia %>%
  filter(fecha >= desde) %>%
  group_by(estacion) %>%
  summarise(dias = n(),
            habil = mean(pax[fecha %in% dias_habiles]),
            finde = mean(pax[lubridate::wday(fecha, week_start = 1) >= 6]),
            .groups = "drop") %>%
  filter(dias > 250)          # saco las estaciones que estuvieron cerradas por obras

horas <- datos %>%
  filter(fecha >= desde, fecha %in% dias_habiles) %>%
  group_by(estacion) %>%
  summarise(manana = sum(pax_total[HORA %in% 7:8]) / sum(pax_total),
            mediodia = sum(pax_total[HORA %in% 11:14]) / sum(pax_total),
            tarde = sum(pax_total[HORA %in% 17:18]) / sum(pax_total),
            noche = sum(pax_total[HORA >= 21]) / sum(pax_total))

perfil <- perfil %>%
  left_join(horas, by = "estacion") %>%
  mutate(log_vol = log(habil), ratio_finde = finde / habil)

# 3. Grupos de estaciones con HCPC (PCA + cluster jerárquico) ------------------------
X <- perfil %>%
  select(log_vol, ratio_finde, manana, mediodia, tarde, noche) %>%
  as.data.frame()
rownames(X) <- perfil$estacion

res_pca <- PCA(X, scale.unit = TRUE, ncp = 4, graph = FALSE)
res_hcpc <- HCPC(res_pca, nb.clust = 3, graph = FALSE)
perfil$grupo <- res_hcpc$data.clust$clust

# nombro los grupos según su perfil horario
medias <- perfil %>%
  group_by(grupo) %>%
  summarise(manana = mean(manana), tarde = mean(tarde))
g_manana <- medias$grupo[which.max(medias$manana)]
g_tarde <- medias$grupo[which.max(medias$tarde)]

perfil <- perfil %>%
  mutate(perfil_uso = case_when(grupo == g_manana ~ "Origen / pico de la mañana",
                                grupo == g_tarde ~ "Centro / pico de la tarde",
                                TRUE ~ "Mixto"))


# 4. Resultados del clustering ---------------------------------------------------------------------
# varianza explicada por los 4 componentes y silueta para k = 2 a 6
var_pca <- res_pca$eig[1:4, "cumulative percentage of variance"]
silueta <- sapply(2:6, function(k) {
  r <- HCPC(res_pca, nb.clust = k, graph = FALSE)
  mean(silhouette(as.integer(r$data.clust$clust), dist(res_pca$ind$coord[, 1:4]))[, "sil_width"])
})
tabla_silueta <- tibble(k = 2:6, silueta = round(silueta, 3))
print(round(var_pca, 1)); print(tabla_silueta); print(table(perfil$perfil_uso))
guardar_tabla(tabla_silueta, "silueta.csv")

# perfil promedio de cada grupo (variables estandarizadas)
vars <- c("log_vol", "ratio_finde", "manana", "tarde", "mediodia", "noche")
perfil_z <- perfil %>%
  mutate(across(all_of(vars), ~ as.numeric(scale(.x)))) %>%
  group_by(perfil_uso) %>%
  summarise(across(all_of(vars), mean), n = n(), .groups = "drop")
guardar_tabla(perfil_z, "perfil_grupos.csv")

nombres_vars <- c(log_vol = "Volumen\n(log)", ratio_finde = "Fin de semana /\ndía hábil", manana = "Mañana\n(7 a 9 hs)",
                  tarde = "Tarde\n(17 a 19 hs)", mediodia = "Mediodía\n(11 a 15 hs)", noche = "Noche\n(desde 21 hs)")
largo <- perfil_z %>%
  pivot_longer(all_of(vars), names_to = "variable", values_to = "z") %>%
  mutate(variable = factor(nombres_vars[variable], levels = nombres_vars),
         perfil_n = paste0(perfil_uso, "\n(n = ", n, ")"))

g09 <- ggplot(largo, aes(variable, perfil_n, fill = z)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = format(round(z, 1), decimal.mark = ","), colour = abs(z) > 1.1), size = 3) +
  scale_colour_manual(values = c("TRUE" = "white", "FALSE" = txt), guide = "none") +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", limits = c(-2, 2), name = "Desvíos") +
  labs(title = "Perfil de cada grupo de estaciones: desvíos respecto del promedio", x = NULL, y = NULL,
       caption = "Fuente: elaboración propia en base a validaciones en molinetes (GCBA, Buenos Aires Data). Variables estandarizadas.") +
  theme_minimal(base_size = 10) +
  theme(panel.grid = element_blank(), axis.text.x = element_text(size = 7.5), plot.title.position = "plot",
        plot.title = element_text(face = "bold", size = 11), plot.caption = element_text(size = 7, hjust = 0, colour = "#5B6573"),
        plot.caption.position = "plot")
guardar_grafico(g09, "g09_hcpc_perfiles.jpg", 15.5, 7.5)


# se guarda el perfil de cada estación para el script siguiente
perfil %>%
  mutate(grupo = as.character(grupo)) %>%
  write_csv(here("input", "perfil_estaciones.csv"))
