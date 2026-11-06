#-------------------------------------------------
#Descomposición Aditiva
#Ana Aldás Cárdenas
#------------------------------------------------

library(fpp3)
library(slider)
library(readxl)
library(tidyverse)
library(here)

here::i_am("main/tratamiento.R")

dt <- read_xlsx(
  here("Data", "inflacin.xlsx"),
  skip = 1
)

dt2 <- dt[, 1:4]

names(dt2) <- c(
  "fecha", "mensual", "anual", "acumulada"
)

#tsibble
base <- dt2 |>
  mutate(Date = yearmonth(fecha)) |>
  select(Date, mensual, anual, acumulada) |>
  arrange(Date) |>
  as_tsibble(index = Date)

#para comprobar los saltos entre fechas
#calculamos la diferencia en meses entre observaciones
#porque la serie es mensual

base |>
  as_tibble() |>
  arrange(Date) |>
  mutate(
    meses_entre_fechas =
      12 * (year(Date) - year(lag(Date))) +
      (month(Date) - month(lag(Date)))
  ) |>
  filter(meses_entre_fechas > 1) |>
  select(Date, meses_entre_fechas) |>
  head(10)

#Verificar el intervalo temporal
interval(base)

#Comprobar si existen meses faltantes
count_gaps(base)


#-----------------------------------------
#ESTIMAR LA TENDENCIA-CICLO
#Se utiliza m = 12 porque trabajamos con
#inflación mensual y estacionalidad anual
#Como m = 12 es par entonces se calcula una
#media móvil centrada de orden 2x12

base_ma <- base |>
  mutate(
    `12-MA` = slider::slide_dbl(
      mensual,
      mean,
      .before = 5,
      .after = 6,
      .complete = TRUE
    ),
    
    `2x12-MA` = slider::slide_dbl(
      `12-MA`,
      mean,
      .before = 1,
      .after = 0,
      .complete = TRUE
    )
  )

base_ma

#serie original y la tendencia
base_ma |>
  autoplot(mensual) +
  geom_line(
    aes(y = `2x12-MA`),
    colour = "red"
  ) +
  labs(
    x = "Fecha",
    y = "Inflación mensual (%)",
    title = "Media móvil centrada 2x12 - Inflación mensual"
  )


#-----------------------------------------
#ELIMINAR TENDENCIA
# Yt - Tt = St + Rt

base_ma <- base_ma |>
  mutate(
    sin_tendencia = mensual - `2x12-MA`
  )

base_ma

#serie sin tendencia
base_ma |>
  autoplot(sin_tendencia) +
  labs(
    x = "Fecha",
    y = "Inflación",
    title = "Serie sin tendencia - Inflación mensual"
  )

#-----------------------------------------
#COMPONENTE ESTACIONAL

#las 12 posiciones del ciclo corresponden a un mes del añ

base_ma <- base_ma |>
  mutate(
    estacion = month(Date)
  )

#promedio estacional para cada uno de los 12 meses

base_ma <- base_ma |>
  group_by(estacion) |>
  mutate(
    media_estacional = mean(
      sin_tendencia,
      na.rm = TRUE
    )
  ) |>
  ungroup()

base_ma

#12 promedios
medias <- base_ma |>
  distinct(estacion, media_estacional)

#promedio de los 12 valores
ajuste <- mean(medias$media_estacional)

#componente estacional ajustada
base_ma <- base_ma |>
  mutate(
    estacional = media_estacional - ajuste
  )
base_ma |>
  distinct(estacion, estacional) |>
  arrange(estacion)

#Comprobar que los 12 factores sumen cero
base_ma |>
  distinct(estacion, estacional) |>
  summarise(
    suma = sum(estacional)
  )

#componente estacional
base_ma |>
  autoplot(estacional) +
  labs(
    x = "Fecha",
    y = "Efecto estacional",
    title = "Componente estacional - Inflación mensual"
  )


#-----------------------------------------
#COMPONENTE RESTANTE
# Rt = Yt - Tt - St
# Rt = sin_tendencia - estacional

base_ma <- base_ma |>
  mutate(
    restante = sin_tendencia - estacional
  )

base_ma

#componente restante
base_ma |>
  autoplot(restante) +
  labs(
    x = "Fecha",
    y = "Componente restante",
    title = "Componente restante - Inflación mensual"
  )
