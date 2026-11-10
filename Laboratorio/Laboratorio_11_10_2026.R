library(fpp3)

#ch4.1 a10

PBS |>
  filter(ATC2 == "A10") |>
  select(Month, Concession, Type, Cost) |>
  summarise(TotalC = sum(Cost)) |>
  mutate(Cost = TotalC / 1e6) -> a10

a10|>features(Cost,list(mean=mean))|>arrange(mean)
a10|>features(Cost,list(mean=median))|>arrange(mean)
a10|>features(Cost,quantile)

#ch4.2 tourism
tourism
tourism |> features(Trips, feat_acf)

#ch4.2 a10

a10|>features(Cost,feat_acf)

#ch4.3 tourism

tourism_STL<-tourism |>
  features(Trips, feat_stl)
max(tourism_STL$trend_strength)
tourism_STL_max<-tourism_STL[tourism_STL$trend_strength==max(tourism_STL$trend_strength),]
tourism_STL_max

tourism |>
  features(Trips, feat_stl) |>
  ggplot(aes(x = trend_strength, y = seasonal_strength_year, col = Purpose)) +
  geom_point() +
  facet_wrap(vars(State)) +
  labs(
    x = "Fuerza de la tendencia",
    y = "Fuerza de la estacionalidad",
    colour = "Motivo"
  )
tourism |>
  features(Trips, feat_stl) |>
  filter(
    seasonal_strength_year == max(seasonal_strength_year)
  )|>
  left_join(tourism, by = c("State", "Region", "Purpose"), multiple = "all") |>
  ggplot(aes(x = Quarter, y = Trips)) +
  geom_line() +
  facet_grid(vars(State, Region, Purpose)) +
  labs(x = "Trimestre")
seasonal_peak_year<-tourism|>features(Trips,feat_stl)|>filter(seasonal_peak_year==max(seasonal_peak_year))
seasonal_trough_year<-tourism|>features(Trips,feat_stl)|>filter(seasonal_trough_year==min(seasonal_trough_year))
seasonal_trough_year
spikeness<- tourism|>features(Trips,feat_stl)|>filter(spikiness==max(spikiness))
spikeness
nospikiness<-tourism|>features(Trips,feat_stl)|>filter(spikiness==min(spikiness))
nospikiness
tourism|>features(Trips,feat_stl)|>mean(linearity)
tourism_max_lin<-tourism|>features(Trips,feat_stl)|>filter(linearity==max(linearity))
tourism_max_lin|>left_join(tourism,by=c("State","Region","Purpose"),multiple="all")|>
  ggplot(aes(x=Quarter,y=Trips))+
  geom_line()+
  facet_grid(vars(State,Region,Purpose))+
  labs(x="Trimestre")
tourism_max_curv<-tourism|>features(Trips,feat_stl)|>filter(curvature==max(curvature))

tourism_max_curv|>left_join(tourism,by=c("State","Region","Purpose"),multiple="all")|>
  ggplot(aes(x=Quarter,y=Trips))+
  geom_line()+
  facet_grid(vars(State,Region,Purpose))+
  labs(x="Trimestre")
tourism|>features(Trips,feat_stl)|>head()

autoplot(a10)
unitroot_kpss(a10)
a10|>features(Cost,unitroot_kpss)
a10|>features(Cost,unitroot_pp)
a10|>features(Cost,unitroot_nsdiffs)
