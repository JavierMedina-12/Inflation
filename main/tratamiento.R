library(fpp3)
library(readxl)
library(tidyverse)
library(here)

here::i_am("main/tratamiento.R")


dt <- read_xlsx(here("Data", "inflacin.xlsx"), skip=1) #agregamos que se salte una línea que viene siendo el encabezado 
#si no se hace el salto entonces al hacer el tibble se toma como una fila más y al hacer el tsibble no lee de forma correcta
dt2<- dt[,1:4];names(dt2)<-c("fecha","mensual","anual","acumulada")
head(dt2)
dt_tsibble<-dt2|>mutate(Date=yearmonth(fecha))|> as_tsibble(index = Date)
print(dt_tsibble)
#Todo lo demás a mi parecer está bien

# Guardar el tsibble en un archivo CSV dentro de la carpeta Data
write_csv(dt_tsibble, here("Data", "DATA.csv"))