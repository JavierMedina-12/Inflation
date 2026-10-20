library(fpp3)
library(readxl)
library(tidyverse)
#Una mejora que considero importante es el uso de la librería here(), para no tener problemas con la ruta del archivo
library(here)

#ORIGINAL
#dt<-read_xlsx("C:/Users/Usuario/Desktop/Series Temporales/Inflation/inflacin.xlsx")
#CAMBIO
dt <- read_xlsx(here("Data", "inflacin.xlsx"), skip=1) #agregamos que se salte una línea que viene siendo el encabezado 
#si no se hace el salto entonces al hacer el tibble se toma como una fila más y al hacer el tsibble no lee de forma correcta
dt2<- dt[,1:4];names(dt2)<-c("fecha","mensual","anual","acumulada")
head(dt2)
dt_tsibble<-dt2|>mutate(Date=yearmonth(fecha))|> as_tsibble(index = Date)
print(dt_tsibble)
#Todo lo demás a mi parecer está bien
