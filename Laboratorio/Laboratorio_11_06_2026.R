

#cargar la bd

us_retail_employment <- us_employment |>
  filter(year(Month) >= 1990, Title == "Retail Trade")

#X11
#################################################

x11_dcmp <- us_retail_employment |>
  model(x11 = X_13ARIMA_SEATS(Employed ~ x11())) |>
  components()
autoplot(x11_dcmp) +
  labs(
    x = "Mes",
    title = "Descomposición X-11 del empleo total en el comercio minorista de EE.UU."
  )

#Aditivo
#################################################

clasical_dcmp<- us_retail_employment |>
  model(
    classical_decomposition(Employed, type = "additive")
  ) |>
  components() 

autoplot(clasical_dcmp) +
  labs(
    x="Mes",
    title = "Classical additive decomposition of total
                  US retail employment"
  )

#Método seats
######################################

seats_dcmp <- us_retail_employment |>
  model(seats = X_13ARIMA_SEATS(Employed ~ seats())) |>
  components()
autoplot(seats_dcmp) +
  labs(
    x = "Mes",
    title = "Descomposición SEATS del empleo total en el comercio minorista de EE.UU."
  )  

#Método STL
##########################################

us_retail_employment |>
  model(
    STL(
      Employed ~ trend(window = 7) +
        season(window = "periodic"),
      robust = FALSE
    )
  ) |>
  components() |>
  autoplot() +
  labs(x = "Mes")