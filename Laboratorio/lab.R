library(fpp3)
library(tidyverse)


unique(aus_livestock$State)

autoplot(aus_livestock)
aus_tbn<- aus_livestock|> filter(State=="Victoria",Animal=="Bulls, bullocks and steers")|>
  group_by(Animal)|>summarise(Total = sum(Count))
aus_tbn
autoplot(aus_tbn)

##Transformación
#no hice ninguna transformacion porque los datos no muestran estacionalidad
#tampoco es perdinente un ajuste poblacional y mucho menos por inflacion

aus_tbn|>
  model(
    STL(Total~ trend(window = 12) +
          season(window = "periodic"),
        robust = FALSE)
  )|>
  components() |>
  autoplot() +
  labs(x = "Mes")
gg_subseries(aus_tbn)

##configuración
# Escogi robust=FALSE porque los restos toman valores menores
# con window note que segun aumenta trend toma valores mas pequeños 
# así que tome el valor de 12 ya que mis datos son mensuales
