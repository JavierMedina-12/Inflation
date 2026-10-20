
library(fpp3)
library(tidyverse)
library(here)
here::i_am("main/graficas.R")

inflacion <- read_csv(here("Data", "DATA.csv"), show_col_types = FALSE) |>
  mutate(fecha = yearmonth(fecha)) |>
  select(-Date) |>
  as_tsibble(index = fecha) #|>
  #mutate(across(
   # c(anual, acumulada),
    #~ if_else(fecha < yearmonth("2007 Jan"), NA_real_, .x)
 # ))

# Serie mensual

autoplot(inflacion, mensual) +
  labs(
    title = "Evolución de la inflación mensual",
    subtitle = "Variación porcentual mensual (feb 2006 - ago 2026)",
    x = "Mes",
    y = "Variación mensual (%)"
  ) +
  theme_minimal()

# Estacionalidad dentro de cada año
# cada línea representa un año y se recorre mes a mes.
gg_season(inflacion, mensual, period = "year") +
  labs(
    title = "Estacionalidad anual de la inflación mensual",
    subtitle = "Cada línea es un año",
    x = "Mes",
    y = "Variación mensual (%)"
  ) +
  theme_minimal()

# Último dato de cada año
# en 2026 el último dato es agosto (año incompleto).
inflacion_anual <- inflacion |>
  index_by(anio = year(fecha)) |>
  summarise(mensual = last(mensual))

autoplot(inflacion_anual, mensual) +
  labs(
    title = "Inflación mensual al cierre de cada año",
    subtitle = "Último dato mensual disponible de cada año",
    x = "Año",
    y = "Variación mensual (%)"
  ) +
  theme_minimal()

#Subseries por mes 
# un panel por mes y, dentro de cada panel, la evolución año a año.
gg_subseries(inflacion, mensual) +
  scale_x_yearmonth(
    breaks = yearmonth(c("2010 Jan", "2020 Jan")),
    labels = c("2010", "2020")
  ) +
  labs(
    title = "Subseries mensuales de la inflación",
    subtitle = "Comportamiento de cada mes a lo largo de los años",
    x = "Año",
    y = "Variación mensual (%)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 7))

# Comparación de las tres variables 
# Las tres están en porcentaje, por eso comparten el mismo eje.
# Pregunta para quien revise: Que cambia si quitamos los comentarios 
# en la lectura de datos? 
inflacion |>
  pivot_longer(
    cols = c(mensual, anual, acumulada),
    names_to = "variable",
    values_to = "porcentaje"
  ) |>
  ggplot(aes(x = fecha, y = porcentaje, color = variable)) +
  geom_line(linewidth = 0.5, na.rm = TRUE) +
  labs(
    title = "Comportamiento de la inflación: mensual, anual y acumulada",
    subtitle = "Comparación de las tres medidas de inflación (%)",
    x = "Mes",
    y = "Porcentaje (%)",
    color = "Variable"
  ) +
  theme_minimal()
