# 02 - Descriptivas, faltantes y atípicos
# Describe los pasajeros por día, diagnostica los faltantes (molinete "null", línea D cerrada),
# detecta los días atípicos (1,5 x IQR dentro de cada día de la semana) y muestra
# las estadísticas descriptivas antes y después de la limpieza.
# Guarda: input/dias_diarios.csv (serie diaria con las marcas de feriado, atípico y día válido),
#         tablas en output/tablas/ y los gráficos g01 a g06 en output/graficos/

library(tidyverse)
library(lubridate)
library(ggrepel)
library(here)

source(here("utils", "constantes.R"))
source(here("utils", "tema_tp.R"))
source(here("utils", "guardar_grafico.R"))
source(here("utils", "guardar_tabla.R"))

# 1. Datos ---------------------------------------------------------------------------------
feriados <- read_csv(here("auxiliar", "feriados_2024.csv")) %>% pull(fecha) %>% as.Date()

diario <- read_csv(here("input", "agregado_diario_linea.csv")) %>% mutate(fecha = as.Date(FECHA))

dia <- diario %>%
  group_by(fecha) %>%
  summarise(pax = sum(pax_TOTAL), sinD = sum(pax_TOTAL[LINEA != "LineaD"]), .groups = "drop") %>%
  mutate(dow = lubridate::wday(fecha, week_start = 1),
         dia_sem = factor(dow, levels = 1:7, labels = dias_sem),
         feriado = fecha %in% feriados,
         habil = dow <= 5 & !feriado) %>%
  group_by(dow) %>%                                   # atípicos: 1,5 x IQR dentro de cada día de la semana
  mutate(q1 = quantile(pax, .25), q3 = quantile(pax, .75),
         lim_inf = q1 - 1.5 * (q3 - q1), lim_sup = q3 + 1.5 * (q3 - q1),
         atipico = pax < lim_inf | pax > lim_sup) %>%
  ungroup()

validos <- dia %>% filter(habil, !atipico) %>% arrange(fecha)

# se guarda la serie diaria para que la usen los scripts siguientes
dia %>%
  mutate(valido = habil & !atipico) %>%
  select(fecha, pax, sinD, dow, feriado, habil, atipico, valido) %>%
  write_csv(here("input", "dias_diarios.csv"))

# 2. Estadísticas descriptivas antes y después de la limpieza ------------------------------------
resumen <- function(x) tibble(n = length(x), media = mean(x), mediana = median(x), desvio = sd(x),
                              minimo = min(x), maximo = max(x), p5 = quantile(x, .05), p95 = quantile(x, .95),
                              cv_pct = 100 * sd(x) / mean(x))
descriptivas <- bind_rows(
  resumen(dia$pax[dia$dow <= 5]) %>% mutate(version = "1. Original (lunes a viernes)"),
  resumen(dia$pax[dia$habil]) %>% mutate(version = "2. Sin feriados"),
  resumen(validos$pax) %>% mutate(version = "3. Sin feriados ni atípicos"),
  resumen(validos$sinD) %>% mutate(version = "4. Además sin línea D")) %>%
  select(version, everything())
print(descriptivas)
guardar_tabla(descriptivas, "descriptivas_antes_despues.csv")
guardar_tabla(dia %>% filter(atipico) %>% select(fecha, dia_sem, pax, feriado), "atipicos.csv")

# 3. Exploración descriptiva ---------------------------------------------------------------------
g01 <- ggplot(dia, aes(dia_sem, pax)) +
  geom_boxplot(outlier.shape = NA, fill = "#E8EDF3", colour = gris) +
  geom_jitter(aes(colour = feriado), width = 0.18, size = 0.9) +
  scale_colour_manual(values = c("FALSE" = "#C9CED6", "TRUE" = acento), labels = c("Día común", "Feriado"), name = NULL) +
  scale_y_continuous(labels = miles) +
  labs(title = "Pasajeros por día según el día de la semana (364 días)", x = NULL, y = "Pasajeros por día",
       caption = paste(fuente, "Rojo: feriados.")) +
  tema_tp + theme(legend.position = "bottom")
guardar_grafico(g01, "g01_pasajeros_dia_semana.jpg", 15.6, 9)

hora <- read_csv(here("input", "agregado_estacion_hora.csv")) %>% mutate(fecha = as.Date(FECHA))
horario <- hora %>%
  group_by(fecha, HORA) %>% summarise(pax = sum(pax_total), .groups = "drop") %>%
  left_join(dia %>% select(fecha, habil, dow), by = "fecha") %>%
  mutate(tipo = case_when(habil ~ "Día hábil (sin feriados)", dow >= 6 ~ "Fin de semana", TRUE ~ NA_character_)) %>%
  filter(!is.na(tipo)) %>%
  group_by(tipo, HORA) %>% summarise(prom = mean(pax), .groups = "drop")
g02 <- ggplot(horario, aes(HORA, prom, colour = tipo)) +
  geom_line(linewidth = 1.1) +
  scale_colour_manual(values = c("Día hábil (sin feriados)" = azul, "Fin de semana" = "#52B69A"), name = NULL) +
  scale_x_continuous(breaks = seq(0, 21, 3), labels = function(x) paste0(x, ":00")) +
  scale_y_continuous(labels = en_mil) +
  labs(title = "Pasajeros por hora del día, promedio de 2024", x = "Hora de inicio de la franja",
       y = "Pasajeros por hora (toda la red)", caption = fuente) +
  tema_tp + theme(legend.position = "bottom")
guardar_grafico(g02, "g02_perfil_horario.jpg", 15.6, 9)

# 4. Faltantes ---------------------------------------------------------------------------------
nulos <- read_csv(here("input", "nulos_molinete.csv")) %>%
  group_by(mes) %>% summarise(registros = sum(registros), nulos = sum(nulos), .groups = "drop") %>%
  mutate(pct = 100 * nulos / registros)
guardar_tabla(nulos, "nulos_molinete_por_mes.csv")
nulos <- nulos %>% mutate(mes_f = factor(mes, levels = 1:12, labels = c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic")))
g03 <- ggplot(nulos, aes(mes_f, pct, fill = pct > 0)) +
  geom_col(width = 0.65, show.legend = FALSE) +
  geom_text(data = filter(nulos, pct > 0), aes(label = paste0(format(round(pct, 1), decimal.mark = ","), "%")),
            vjust = -0.5, size = 3.4, fontface = "bold", colour = acento) +
  scale_fill_manual(values = c("FALSE" = "#C9CED6", "TRUE" = acento)) +
  scale_y_continuous(labels = scales::label_number(suffix = "%"), limits = c(0, 24)) +
  labs(title = "Registros con molinete \"null\", por mes (% de los registros del mes)", x = NULL,
       y = "Registros sin ID de molinete (%)", caption = fuente) +
  tema_tp
guardar_grafico(g03, "g03_faltantes_molinete_mes.jpg", 15.2, 8.9)

linea_d <- diario %>% filter(LINEA == "LineaD") %>% group_by(mes = month(fecha)) %>% summarise(pax = sum(pax_TOTAL), .groups = "drop")
linea_d <- linea_d %>% mutate(mes_f = factor(mes, levels = 1:12, labels = c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic")))
g04 <- ggplot(linea_d, aes(mes_f, pax, fill = mes %in% 1:2)) +
  geom_col(width = 0.65, show.legend = FALSE) +
  geom_text(data = filter(linea_d, mes == 2), aes(label = pax), vjust = -0.5, size = 3.4, fontface = "bold", colour = acento) +
  scale_fill_manual(values = c("FALSE" = azul, "TRUE" = acento)) +
  scale_y_continuous(labels = scales::label_number(scale = 1/1e6, suffix = " M", decimal.mark = ",")) +
  labs(title = "Pasajeros de la línea D por mes: cerrada por obras del 8/1 al 7/3", x = NULL,
       y = "Pasajeros por mes (línea D)", caption = fuente) +
  tema_tp
guardar_grafico(g04, "g04_linea_d_mes.jpg", 15.2, 8.9)

# 5. Atípicos y efecto de la limpieza --------------------------------------------------------------
limites <- dia %>% distinct(dia_sem, lim_inf, lim_sup)
etiquetas <- dia %>% filter(fecha %in% as.Date(c("2024-10-31", "2024-12-24", "2024-12-31", "2024-03-24"))) %>%
  mutate(texto = paste0(day(fecha), "/", month(fecha)))
g05 <- ggplot(dia, aes(dia_sem, pax)) +
  geom_jitter(aes(colour = atipico, size = atipico), width = 0.2, show.legend = FALSE) +
  geom_errorbar(data = limites, aes(x = dia_sem, ymin = lim_inf, ymax = lim_inf), width = 0.6, colour = gris, linetype = "dotted", inherit.aes = FALSE) +
  geom_errorbar(data = limites, aes(x = dia_sem, ymin = lim_sup, ymax = lim_sup), width = 0.6, colour = gris, linetype = "dotted", inherit.aes = FALSE) +
  scale_colour_manual(values = c("FALSE" = "#C9CED6", "TRUE" = acento)) +
  scale_size_manual(values = c("FALSE" = 0.9, "TRUE" = 1.8)) +
  geom_text_repel(data = etiquetas, aes(label = texto), size = 2.8, colour = txt) +
  scale_y_continuous(labels = miles) +
  labs(title = "Días atípicos según el criterio 1,5 × IQR dentro de cada día de la semana", x = NULL, y = "Pasajeros por día",
       caption = paste(fuente, "Rojo: atípicos (19 días).")) +
  tema_tp
guardar_grafico(g05, "g05_atipicos.jpg", 16.3, 10.2)

versiones <- bind_rows(
  dia %>% filter(dow <= 5) %>% transmute(version = "Original\n(lunes a viernes)", pax),
  dia %>% filter(habil) %>% transmute(version = "Sin feriados", pax),
  dia %>% filter(habil, !atipico) %>% transmute(version = "Sin feriados\nni atípicos", pax),
  dia %>% filter(habil, !atipico) %>% transmute(version = "Además sin\nlínea D", pax = sinD)) %>%
  mutate(version = factor(version, levels = rev(unique(version))))
g06 <- ggplot(versiones, aes(pax, version)) +
  geom_boxplot(fill = "#E8EDF3", colour = gris, outlier.colour = acento, outlier.size = 1.2) +
  stat_summary(fun = mean, geom = "point", shape = 23, fill = txt, size = 2.6) +
  scale_x_continuous(labels = en_mil, limits = c(0, 1000000)) +
  labs(title = "Pasajeros por día antes y después de cada decisión de limpieza", x = "Pasajeros por día hábil", y = NULL,
       caption = "Fuente: elaboración propia en base a molinetes (GCBA), 2024. Rombo: media; rojo: atípicos de cada versión.") +
  tema_tp + theme(panel.grid.major.x = element_line(colour = "#E6E9ED"), panel.grid.major.y = element_blank())
guardar_grafico(g06, "g06_antes_despues_limpieza.jpg", 17.8, 9.9)

