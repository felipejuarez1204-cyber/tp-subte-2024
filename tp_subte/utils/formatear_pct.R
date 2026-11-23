# Formatea un número como porcentaje con signo y coma decimal (para un solo valor)
# Ejemplo: formatear_pct(-16.52, 1) devuelve "-16,5%" y formatear_pct(233) devuelve "+233%"
formatear_pct <- function(x, dec = 0) {
  paste0(ifelse(x > 0, "+", ""), format(round(x, dec), decimal.mark = ",", trim = TRUE), "%")
}
