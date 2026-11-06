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

##El archivo DATA.csv ya esta procesado, inflaccin.xlsx es la base cruda JM
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
## Y esto para????? JM
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

##Validación JM
base_desc<-base %>%
  model(
    classical_decomposition(mensual, type = "additive")
  ) %>%
  components()

base_desc %>%
  autoplot() +
  labs(
    title = "Descomposición clásica aditiva de la inflación mensual en Ecuador"
  )

##De forma visual parece que la descomposición es adecuada 

##Descomposición multiplicativa
# Y_t = T_t * S_t * R_t
# La descomposición multiplicativa exige Y_t > 0.
# La inflación mensual contiene meses con deflación (Y_t <= 0), por lo que
# se trabaja con la serie desplazada Y*_t = Y_t + c, con c > |min(Y_t)|.

m <- 12  # periodicidad mensual

# Desplazamiento mínimo para garantizar que todos los valores sean positivos
c <- abs(min(base$mensual, na.rm = TRUE)) + 0.01

base_mult <- base |>
  mutate(
    mensual_pos = mensual + c
  )

# -------------------------------------------------------------------
# Pasos 1 y 2:
# Tendencia-ciclo 2x12-MA y serie sin tendencia Y*_t / T_hat
# -------------------------------------------------------------------
div_mult <- base_mult |>
  mutate(
    # Primera media móvil de 12 meses
    T_hat = slider::slide_dbl(
      mensual_pos,
      mean,
      .before = 5,
      .after = 6,
      .complete = TRUE
    ),
    # Segunda media móvil de orden 2: centra la media de 12 meses
    T_hat = slider::slide_dbl(
      T_hat,
      mean,
      .before = 1,
      .after = 0,
      .complete = TRUE
    ),
    # Paso 2: eliminación de la tendencia
    y_sobre_T = mensual_pos / T_hat,
    mes = month(Date, label = TRUE, abbr = TRUE)
  )

# Serie original y tendencia-ciclo
div_mult |>
  autoplot(mensual, colour = "gray") +
  geom_line(aes(y = T_hat - c), colour = "#D55E00") +
  labs(
    x = "Fecha",
    y = "Inflación mensual (%)",
    title = "Inflación mensual y tendencia-ciclo 2x12-MA"
  )

# Serie sin tendencia
div_mult |>
  autoplot(y_sobre_T) +
  labs(
    x = "Fecha",
    y = "Y*_t / T_hat",
    title = "Serie sin tendencia - Descomposición multiplicativa"
  )


# Índice estacional por mes y normalización para que sume m = 12

indice_estacional <- div_mult |>
  as_tibble() |>
  filter(!is.na(y_sobre_T)) |>
  group_by(mes) |>
  summarise(
    S_hat = mean(y_sobre_T),
    .groups = "drop"
  ) |>
  mutate(
    # Normalización: suma de los índices = m, media = 1
    S_hat = S_hat * m / sum(S_hat)
  )

# Replicar el índice estacional en toda la serie
div_mult <- div_mult |>
  as_tibble() |>
  left_join(indice_estacional, by = "mes") |>
  mutate(
    # Paso 4: componente irregular
    R_hat = mensual_pos / (T_hat * S_hat)
  )

# Objeto tsibble para graficar los componentes
div_mult_ts <- div_mult |>
  as_tsibble(index = Date)

# Índice estacional (repetido en el tiempo, igual que en desc.R)
div_mult_ts |>
  autoplot(S_hat) +
  labs(
    x = "Fecha",
    y = "Índice estacional",
    title = "Índice estacional multiplicativo - Inflación mensual"
  )

# Componente irregular
div_mult_ts |>
  autoplot(R_hat) +
  labs(
    x = "Fecha",
    y = "Componente irregular",
    title = "Componente irregular - Descomposición multiplicativa"
  )

# -------------------------------------------------------------------
# Verificaciones
# -------------------------------------------------------------------
sum(indice_estacional$S_hat)   # debe ser 12
mean(indice_estacional$S_hat)  # debe ser 1

# La serie reconstruida debe coincidir con la observada
div_mult |>
  mutate(ajuste = T_hat * S_hat * R_hat) |>
  summarise(
    max_error = max(abs(mensual_pos - ajuste), na.rm = TRUE)
  )

###############################################
base_mdesc <- base_mult |>
  model(
    classical_decomposition(mensual_pos, type = "multiplicative")
  ) |>
  components()

base_mdesc |>
  autoplot() +
  labs(
    title = "Descomposición clásica multiplicativa de la inflación mensual (serie desplazada)"
  )
