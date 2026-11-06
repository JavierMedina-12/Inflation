library(fpp3)
library(readxl)
library(tidyverse)
library(here)
here::i_am("proyecto.R")

inflacion <- read_csv(here("Data", "DATA.csv"), show_col_types = FALSE) |>
  mutate(fecha = yearmonth(fecha)) |>
  select(-Date) |>
  as_tsibble(index = fecha) 
dt2<- dt[,1:4];names(dt2)<-c("fecha","mensual","anual","acumulada")
head(dt2)
dt_tsibble<-dt2|>mutate(Date=yearmonth(fecha))|> as_tsibble(index = Date)
print(dt_tsibble)
## Graficas

autoplot(dt_tsibble, mensual) +
  labs(
    title = "Inflación",
    subtitle = "---",
    y = "%"
  )

dt_tsibble|> gg_subseries(mensual)

dt_tsibble|>ACF(mensual,lag_max = 32)|>autoplot()+labs(title = "Inflación mensual")

set.seed(12)
y <- tsibble(sample = 1:100, wn = rnorm(100), index = sample)
y |> autoplot(wn) + labs(title = "White noise", y = "")
y|>ACF(wn)|>autoplot()+labs(title = "ruido blanco")


