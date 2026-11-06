library(fpp3)
library(tidyverse)
aus_exports <- global_economy |>
  filter(Country == "Australia") |>
  mutate(
    `5-MA` = slider::slide_dbl(
      Exports,
      mean,
      .before = 4,
      .after = 4,
      .complete = TRUE
    )
  )
aus_exports |>
  autoplot(Exports) +
  geom_line(aes(y = `5-MA`), colour = "#D55E00") +
  labs(y = "% del PIB", title = "Exportaciones totales de Australia")


us_retail_employment <- us_employment |>
  filter(year(Month) >= 1990, Title == "Retail Trade")

us_retail_employment_ma <- us_retail_employment |>
  mutate(
    `12-MA` = slider::slide_dbl(
      Employed,
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

us_retail_employment_ma |>
  autoplot(Employed, colour = "gray") +
  geom_line(aes(y = `2x12-MA`), colour = "#D55E00") +
  labs(
    y = "Personas (miles)",
    title = "Empleo total en el comercio minorista de EE.UU."
  )

#####################################

us_retail_employment |>
  model(
    classical_decomposition(Employed, type = "additive")
  ) |>
  components() |>
  autoplot() +
  labs(
    title = "Classical additive decomposition of total
                  US retail employment"
  )
us_retail_employment_pt<- us_retail_employment|>mutate(pt = Employed - us_retail_employment_ma$`2x12-MA`)
autoplot(us_retail_employment_pt,pt)

#Seasonal
us_retail_employment_st <- us_retail_employment_pt|>group_by(month(Month))|>mutate(secuencia= mean(pt,na.rm=TRUE))
autoplot(us_retail_employment_st,secuencia)

restante<- us_retail_employment_st|>mutate(resto = pt- secuencia)
autoplot(restante,resto)
