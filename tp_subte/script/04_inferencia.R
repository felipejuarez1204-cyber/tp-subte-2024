# 04 - Inferencia: efecto de cada aumento sobre los pasajeros
# Método 1: Mann-Whitney con corrección de Holm (10 días hábiles antes vs. 10 después de cada aumento)
# Método 2: serie de tiempo interrumpida (todo el período desde el 18/3, con errores robustos)
# Elasticidad simple = variación % de pasajeros / variación % real de la tarifa
# Guarda: tablas en output/tablas/ y los gráficos g07, g08 y el gráfico comunicativo en output/graficos/
# Trabaja con la red sin la línea D (cerrada del 8/1 al 7/3)

library(tidyverse)
library(lubridate)
library(patchwork)
library(here)

source(here("utils", "constantes.R"))
source(here("utils", "tema_tp.R"))
source(here("utils", "guardar_grafico.R"))
source(here("utils", "guardar_tabla.R"))
source(here("utils", "formatear_pct.R"))
source(here("utils", "ventana_habil.R"))

# 1. Datos guardados por los scripts 02 y 03 -------------------------------------------------------
dias_diarios <- read_csv(here("input", "dias_diarios.csv"))
validos <- dias_diarios %>% filter(valido) %>% arrange(fecha)

var_real <- read_csv(here("output", "tablas", "variacion_real_tarifa.csv")) %>%
  slice(match(c("17/5", "1/6", "1/10"), aumento)) %>% pull(var_real_pct)
real <- read_csv(here("input", "tarifa_real_diaria.csv"))
valor <- function(f) real$tarifa_real[real$fecha == as.Date(f)]

# 2. Mann-Whitney: 10 días hábiles antes vs. 10 después (red sin la línea D)
ventanas <- map_dfr(aumentos, ~ ventana_habil(validos, .x))
tabla_mw <- ventanas %>%
  group_by(aumento) %>%
  summarise(antes = mean(sinD[etapa == "antes"]), despues = mean(sinD[etapa == "despues"]),
            variacion = 100 * (despues / antes - 1),
            U = wilcox.test(sinD[etapa == "despues"], sinD[etapa == "antes"], exact = FALSE)$statistic,
            p = wilcox.test(sinD[etapa == "despues"], sinD[etapa == "antes"], exact = FALSE)$p.value,
            levene_p = { z <- abs(sinD - ave(sinD, etapa, FUN = median)); summary(aov(z ~ etapa))[[1]][["Pr(>F)"]][1] },
            .groups = "drop") %>%
  mutate(P_despues_mayor = U / 100, p_holm = p.adjust(p, method = "holm"))
print(tabla_mw)
guardar_tabla(tabla_mw, "mann_whitney.csv")

g07 <- ventanas %>%
  mutate(aumento = factor(aumento, levels = c("17/5", "1/6", "1/10")),
         etapa = factor(etapa, levels = c("antes", "despues"), labels = c("10 días\nantes", "10 días\ndespués"))) %>%
  ggplot(aes(etapa, sinD, colour = etapa)) +
  geom_jitter(width = 0.12, size = 1.8, alpha = 0.85) +
  stat_summary(fun = mean, geom = "errorbar", aes(ymin = after_stat(y), ymax = after_stat(y)), width = 0.6, linewidth = 1) +
  facet_wrap(~aumento) +
  scale_colour_manual(values = c(azul, acento), guide = "none") +
  scale_y_continuous(labels = en_mil) +
  labs(title = "Pasajeros de los 10 días hábiles antes y después de cada aumento", x = NULL,
       y = "Pasajeros por día hábil (red sin línea D)", caption = str_wrap(paste(fuente, "Cada punto es un día; la línea es el promedio. El eje vertical no empieza en cero."), 120)) +
  tema_tp + theme(strip.text = element_text(face = "bold"))
guardar_grafico(g07, "g07_ventanas_aumentos.jpg", 19.3, 8.9)

# 3. Serie de tiempo interrumpida (desde el 18/3, red sin la línea D)
its <- validos %>%
  filter(fecha >= as.Date("2024-03-18")) %>%
  mutate(y = log(sinD),
         salto_may = fecha >= aumentos[1], salto_jun = fecha >= aumentos[2], salto_oct = fecha >= aumentos[3],
         vac_invierno = fecha >= as.Date("2024-07-15") & fecha <= as.Date("2024-07-26"),
         fin_anio = fecha >= as.Date("2024-12-16"),
         dia_sem = factor(lubridate::wday(fecha, week_start = 1)))
modelo <- lm(y ~ salto_may + salto_jun + salto_oct + dia_sem + vac_invierno + fin_anio, data = its)
robusto <- lmtest::coeftest(modelo, vcov = sandwich::NeweyWest(modelo, lag = 5, prewhite = FALSE))
saltos <- c("salto_mayTRUE", "salto_junTRUE", "salto_octTRUE")
tabla_its <- tibble(aumento = c("17/5", "1/6", "1/10"),
                    efecto_pct = 100 * (exp(coef(modelo)[saltos]) - 1),
                    ic_inf = 100 * (exp(coef(modelo)[saltos] - 1.96 * robusto[saltos, 2]) - 1),
                    ic_sup = 100 * (exp(coef(modelo)[saltos] + 1.96 * robusto[saltos, 2]) - 1),
                    t_robusto = robusto[saltos, 3])
print(tabla_its); print(summary(modelo)$r.squared); print(lmtest::dwtest(modelo)$statistic)
guardar_tabla(tabla_its, "serie_interrumpida.csv")

mm <- model.matrix(modelo)
cols_ctrl <- grep("dia_sem|vac_invierno|fin_anio", colnames(mm))
its$ajustado <- exp(its$y - as.numeric(mm[, cols_ctrl] %*% coef(modelo)[cols_ctrl]))
its$nivel <- exp(as.numeric(mm[, c("(Intercept)", saltos)] %*% coef(modelo)[c("(Intercept)", saltos)]))
g08 <- ggplot(its, aes(fecha)) +
  geom_point(aes(y = ajustado), colour = "#C9CED6", size = 1) +
  geom_step(aes(y = nivel), colour = azul, linewidth = 1.1) +
  geom_hline(yintercept = its$nivel[1], colour = gris, linetype = "dotted") +
  geom_vline(xintercept = aumentos[1], colour = acento) +
  annotate("text", x = aumentos[1] - 4, y = its$nivel[1] * 0.80, hjust = 1, colour = acento, fontface = "bold", size = 3.2,
           label = paste0("17/5: ", format(round(tabla_its$efecto_pct[1], 0)), "%")) +
  annotate("text", x = aumentos[2] + 20, y = its$nivel[1] * 0.74, hjust = 0, colour = txt, size = 2.8, lineheight = 1,
           label = "1/6: +11% (recuperación estacional,\nno efecto del aumento)") +
  scale_y_continuous(labels = en_mil) +
  scale_x_date(date_breaks = "1 month", labels = function(x) c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic")[month(x)]) +
  labs(title = "Serie de tiempo interrumpida: nivel de pasajeros estimado, 18/3 al 31/12", x = NULL,
       y = "Pasajeros por día hábil (sin línea D)",
       caption = str_wrap(paste(fuente, "Modelo con saltos el 17/5, 1/6 y 1/10 y errores robustos. El eje vertical no empieza en cero."), 125)) +
  tema_tp
guardar_grafico(g08, "g08_serie_interrumpida.jpg", 18.8, 9.1)



# Elasticidad simple
tabla_elasticidad <- tabla_mw %>%
  slice(match(c("17/5", "1/6", "1/10"), aumento)) %>%
  transmute(aumento, var_pasajeros = variacion, var_tarifa_real = var_real, elasticidad = variacion / var_real)
print(tabla_elasticidad)
guardar_tabla(tabla_elasticidad, "elasticidad.csv")

# 4. Gráfico comunicativo ------------------------------------------------------------------------------
# serie diaria con promedio móvil de 5 días
serie <- validos %>%
  transmute(fecha, pax = sinD,
            prom5 = as.numeric(stats::filter(sinD, rep(1/5, 5), sides = 2)),
            etapa = if_else(fecha < aumentos[1], "antes", "despues"))
orden <- match(c("17/5", "1/6", "1/10"), tabla_mw$aumento)
var_pax <- tabla_mw$variacion[orden]
v_mayo <- list(antes = tabla_mw$antes[tabla_mw$aumento == "17/5"], despues = tabla_mw$despues[tabla_mw$aumento == "17/5"])

# Gráfico ---------------------------------------------------------------------------------------------

# colores: uno de acento (el aumento de mayo) y el resto en azul y grises
banda <- "#F1F3F6"; relleno <- "#DCE6F0"

eje_x <- scale_x_date(limits = c(as.Date("2024-01-01"), as.Date("2024-12-31")),
                      breaks = as.Date(paste0("2024-", 1:12, "-15")),
                      labels = function(x) meses_es[month(x)])

tema <- ggplot2::theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#E6E9ED"),
        axis.text = element_text(colour = gris), axis.title = element_text(colour = txt, size = 9))

# zonas grises: vacaciones de verano, de invierno y fin de año
bandas <- list(
  annotate("rect", xmin = as.Date("2024-01-01"), xmax = as.Date("2024-02-29"), ymin = 0, ymax = Inf, fill = banda),
  annotate("rect", xmin = as.Date("2024-07-15"), xmax = as.Date("2024-07-26"), ymin = 0, ymax = Inf, fill = banda),
  annotate("rect", xmin = as.Date("2024-12-16"), xmax = as.Date("2024-12-31"), ymin = 0, ymax = Inf, fill = banda))

# panel de arriba: pasajeros
arriba <- ggplot(serie, aes(fecha, pax)) +
  bandas +
  geom_vline(xintercept = aumentos[2:3], colour = gris, linetype = "dashed") +
  geom_vline(xintercept = aumentos[1], colour = acento, linewidth = 1) +
  geom_point(colour = "#C9CED6", size = 1.2) +
  geom_line(aes(y = prom5, colour = etapa, group = 1), linewidth = 1.2, na.rm = TRUE) +
  scale_colour_manual(values = c(antes = azul, despues = acento), guide = "none") +
  # el mensaje: la caída de mayo
  annotate("text", x = aumentos[1] - 8, y = 500000, hjust = 1, colour = acento, fontface = "bold", size = 10,
           label = formatear_pct(var_pax[1], 1)) +
  annotate("text", x = aumentos[1] - 8, y = 395000, hjust = 1, colour = txt, size = 3, lineheight = 1.1,
           label = "en los 10 días hábiles\nposteriores al aumento") +
  annotate("text", x = aumentos[1] - 4, y = 935000, hjust = 1, vjust = 1, colour = acento, fontface = "bold", size = 3.4,
           label = "17/5: boleto de $125 a $574") +
  annotate("text", x = aumentos[2] + 4, y = 935000, hjust = 0, vjust = 1, colour = gris, size = 3.1,
           label = paste0("1/6: ", formatear_pct(var_pax[2], 1))) +
  annotate("text", x = aumentos[3] + 4, y = 935000, hjust = 0, vjust = 1, colour = gris, size = 3.1,
           label = paste0("1/10: ", formatear_pct(var_pax[3], 1))) +
  scale_y_continuous(limits = c(0, 960000), breaks = seq(0, 800000, 200000),
                     labels = scales::label_number(scale = 1/1000, suffix = " mil", big.mark = ".")) +
  eje_x +
  labs(x = NULL, y = "Pasajeros por día hábil") +
  tema +
  theme(axis.text.x = element_blank())

# panel de abajo: tarifa real
puntos <- tibble(fecha = aumentos,
                 y = c(valor("2024-05-17"), valor("2024-06-01"), valor("2024-10-01")))

abajo <- ggplot(real, aes(fecha, tarifa_real)) +
  bandas +
  geom_vline(xintercept = aumentos[2:3], colour = gris, linetype = "dashed") +
  geom_vline(xintercept = aumentos[1], colour = acento, linewidth = 1) +
  geom_area(fill = relleno) +
  geom_line(colour = azul, linewidth = 1.1) +
  geom_point(data = puntos, aes(fecha, y), colour = c(acento, gris, gris), size = 2.6) +
  annotate("text", x = aumentos[1] - 4, y = puntos$y[1] + 60, hjust = 1, vjust = 0, colour = acento,
           fontface = "bold", size = 5, label = paste0(formatear_pct(var_real[1]), " real")) +
  annotate("text", x = aumentos[2] + 4, y = puntos$y[2] + 60, hjust = 0, vjust = 0, colour = gris,
           size = 3.4, label = paste0(formatear_pct(var_real[2]), " real")) +
  annotate("text", x = aumentos[3] + 4, y = puntos$y[3] + 40, hjust = 0, vjust = 0, colour = gris,
           size = 3.4, label = paste0(formatear_pct(var_real[3], 1), " real")) +
  scale_y_continuous(limits = c(0, 980), breaks = c(0, 300, 600, 900),
                     labels = scales::label_number(prefix = "$", big.mark = ".")) +
  eje_x +
  labs(x = NULL, y = "Tarifa real\n(pesos del 1/10/2024)") +
  tema

comunicativo <- arriba / abajo +
  plot_layout(heights = c(3.2, 1.3)) +
  plot_annotation(
    title = "Los pasajeros del subte solo cayeron tras el aumento de mayo;\nlos otros dos apenas acompañaron la inflación",
    subtitle = "Pasajeros por día hábil (promedio móvil de 5 días, sin feriados ni atípicos, red sin la línea D) y tarifa real del boleto de subte, 2024",
    caption = str_wrap(paste0("Fuente: elaboración propia en base a validaciones en molinetes (GCBA, Buenos Aires Data), tarifas de SBASE / Boletín Oficial de la Ciudad e IPC del INDEC. ",
                              "Tarifa real: precio deflactado por el IPC, en pesos del 1/10/2024. Zonas grises: vacaciones y fin de año."), 190),
    theme = theme(plot.title = element_text(face = "bold", size = 15.5, colour = txt),
                  plot.subtitle = element_text(size = 8.8, colour = "#4A5563"),
                  plot.caption = element_text(size = 7, hjust = 0, colour = "#5B6573"),
                  plot.title.position = "plot", plot.caption.position = "plot"))


guardar_grafico(comunicativo, "grupo_18_comunicativo.jpg", 24, 17.5)
