# 03 - Tarifa real del boleto de subte
# Pasa la tarifa nominal a pesos del 1/10/2024 con el IPC mensual (la inflación de cada mes se reparte día a día)
# y calcula la variación real de cada aumento: se compara con la tarifa real que había
# cuando entró en vigencia el aumento anterior.
# Guarda: input/tarifa_real_diaria.csv, output/tablas/variacion_real_tarifa.csv
#         y los gráficos g11 (tarifa nominal y real) y g12 (brecha con el colectivo)

library(tidyverse)
library(lubridate)
library(here)

source(here("utils", "constantes.R"))
source(here("utils", "tema_tp.R"))
source(here("utils", "guardar_grafico.R"))
source(here("utils", "guardar_tabla.R"))

# 1. Datos auxiliares -----------------------------------------------------------------------
tarifas <- read_csv(here("auxiliar", "tarifas_subte_2024.csv")) %>% arrange(desde)
ipc <- read_csv(here("auxiliar", "ipc_mensual_2024.csv")) %>% arrange(mes) %>% pull(ipc_pct) / 100

# 2. Tarifa nominal y real, día por día ------------------------------------------------------
dias <- seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "day")
tarifa <- tarifas$tarifa[findInterval(as.numeric(dias), as.numeric(tarifas$desde))]

precios <- c(1, cumprod(1 + ipc))[month(dias)] * (1 + ipc[month(dias)])^((mday(dias) - 1) / days_in_month(dias))
tarifa_real <- tarifa * precios[dias == as.Date("2024-10-01")] / precios

diaria <- tibble(fecha = dias, tarifa = tarifa, tarifa_real = tarifa_real)
write_csv(diaria, here("input", "tarifa_real_diaria.csv"))

valor <- function(f) tarifa_real[dias == as.Date(f)]
variacion_real <- tibble(
  aumento = c("17/5", "1/6", "1/10"),
  var_real_pct = 100 * c(valor("2024-05-17") / valor("2024-02-04") - 1,
                         valor("2024-06-01") / valor("2024-05-17") - 1,
                         valor("2024-10-01") / valor("2024-06-01") - 1))
print(variacion_real)
guardar_tabla(variacion_real, "variacion_real_tarifa.csv")

# 3. Gráficos ------------------------------------------------------------------------------------
tarifas_largo <- diaria %>%
  select(fecha, Nominal = tarifa, Real = tarifa_real) %>%
  pivot_longer(-fecha, names_to = "serie", values_to = "valor")
g11 <- ggplot(tarifas_largo, aes(fecha, valor, colour = serie)) +
  geom_line(linewidth = 1.1) +
  scale_colour_manual(values = c(Nominal = gris, Real = azul), labels = c("Tarifa nominal ($ del momento)", "Tarifa real (pesos del 1/10/2024)"), name = NULL) +
  scale_x_date(date_breaks = "1 month", labels = function(x) meses_es[month(x)]) +
  scale_y_continuous(labels = scales::label_number(prefix = "$", big.mark = ".")) +
  labs(title = "Boleto de subte: tarifa nominal y tarifa real, 2024", x = NULL, y = "Boleto de subte (pesos)",
       caption = "Fuente: SBASE / Boletín Oficial de la Ciudad e IPC del INDEC. Real: deflactado por el IPC mensual.") +
  tema_tp + theme(legend.position = "bottom")
guardar_grafico(g11, "g11_tarifa_real.jpg", 15.2, 8.9)

# Brecha entre el subte y el colectivo (boleto mínimo del colectivo AMBA: $270 desde febrero)
brecha <- read_csv(here("auxiliar", "brecha_subte_colectivo.csv")) %>%
  mutate(tramo = factor(tramo, levels = unique(tramo))) %>%
  pivot_longer(-tramo, names_to = "modo", values_to = "boleto") %>%
  mutate(modo = factor(modo, levels = c("Subte", "Colectivo")))
razones <- brecha %>% pivot_wider(names_from = modo, values_from = boleto) %>%
  mutate(texto = paste0(format(round(Subte / Colectivo, 1), decimal.mark = ","), " veces el colectivo"))
g12 <- ggplot(brecha, aes(tramo, boleto, fill = modo)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  geom_text(aes(label = paste0("$", boleto)), position = position_dodge(width = 0.75), vjust = -0.5, size = 3) +
  geom_text(data = razones, aes(x = tramo, y = -60, label = texto), inherit.aes = FALSE, size = 3, colour = txt) +
  scale_fill_manual(values = c(Subte = azul, Colectivo = "#C9CED6"), name = NULL) +
  scale_y_continuous(labels = scales::label_number(prefix = "$", big.mark = "."), limits = c(-90, 700)) +
  labs(title = "Boleto de subte y boleto mínimo de colectivo en el AMBA, 2024", x = NULL, y = "Boleto mínimo (pesos)",
       caption = "Fuente: SBASE / Boletín Oficial de la Ciudad y notas de prensa (colectivo AMBA: $270 desde febrero).") +
  tema_tp + theme(legend.position = "top")
guardar_grafico(g12, "g12_brecha_modos.jpg", 15.2, 8.9)
