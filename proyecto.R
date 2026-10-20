library(fpp3)
library(readxl)
library(tidyverse)
dt<-read_xlsx("C:/Users/Usuario/Desktop/Series Temporales/Inflation/inflacin.xlsx",skip=1)
dt2<- dt[,1:4];names(dt2)<-c("fecha","mensual","anual","acumulada")
head(dt2)
dt_tsibble<-dt2|>mutate(Date=yearmonth(fecha))|> as_tsibble(index = Date)
print(dt_tsibble)
#inflacion_2026<-dt_tsibble$fecha["2025"]

autoplot(dt_tsibble, mensual) +
  labs(
    title = "Inflación",
    subtitle = "---",
    y = "%"
  )

dt_tsibble|> gg_subseries(mensual)
