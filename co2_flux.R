source("libs_and_funcs.R")

#Estimate CO2 fluxes from predicted CO2 concentration and hydrological variables

#Derive relationship between air and water temperature
q_points_modeling <- read_parquet("data/q_points_modeling.parquet")

wtr_data <- q_points_modeling |> 
  select(airt, wtr, season) |> 
  na.omit()

wtr_model <- lm(wtr~airt, data = wtr_data)
summary(wtr_model)

fig_wtr_model <- wtr_data |> 
  ggplot(aes(x=airt, y=wtr))+
  geom_abline(slope=1, intercept = 0, linetype=3)+
  geom_point()+
  geom_smooth(method="lm", col="coral")+
  ylim(0, 22)+
  xlim(0, 22)+
  xlab(expression("Air temperature ("*degree*"C)"))+
  ylab(expression("Water temperature ("*degree*"C)"))

fig_wtr_model

#Derive slope
#For now based on entire stream network segments
network <- st_read("data/dk_model/dk_model_hip_2020.shp")
dk_border <- st_read("data/dk_border.sqlite")
network_border <- st_intersection(network, dk_border) |> 
  st_cast("LINESTRING")

network_begin <- st_startpoint(network_border)
network_end <- st_endpoint(network_border)

dem <- rast("data/dem/dhym.tif")

begin_elev <- as.numeric(extract(dem, vect(network_begin), raw=TRUE, ID=FALSE, method="bilinear"))
end_elev <- as.numeric(extract(dem, vect(network_end), raw=TRUE, ID=FALSE, method="bilinear"))
network_border$drop <- begin_elev - end_elev

network_slope <- network_border |> 
  mutate(length = as.numeric(st_length(geometry)),
         slope = abs(drop)/length) |>
  st_drop_geometry() |> 
  select(branch = BR_BrName, slope)

#Estimate CO2 flux
q_points_predictions <- read_parquet("data/q_points_predictions.parquet") |> 
  filter(snap_dist < 100,
         in_lake == 0,
         is.na(downstream))

#Calculate stream velocity from hydraulic geometry relationships
#raymond 2012
#q in m3/s, v in m/s
v_from_q <- function(q){
  v = 0.19 * q^0.29
  return(v)
}

#raymond 2012, table 2, eq 5
#v in m/s, slope unitless, temp in celcius, kgas in m/d
kgas <- function(v, slope, temp){
  sc <- 1742 - 91.24*temp + 2.208*temp^2 - 0.0219*temp^3
  k600 <- v*slope*2841 + 2.02
  kgas <- k600*(sc/600)^-0.5
  return(kgas)
}

#henrys constant from AquaEnv source
lnK <- function(A, B, C, D, E, T){
  lnK <- A + (B/T) + C*log(T) + D*T + E*T^2
  return(lnK)
}

#unit mol/(kg-soln*atm)
K0_CO2 <- function(S, t){
  T <- t + 273.15
  
  A <- 0.023517*S - 167.81077
  B <- 9345.17
  C <- 23.3585
  D <- -2.3656e-4*S
  E <- 4.7036e-7*S
  
  K0_CO2 <- exp(lnK(A, B, C, D, E, T))

  return (K0_CO2)
}

#determine dissolved co2 in uM from temperate
#temp in celcius, partial pressure in uatm, co2_eq in uM
co2_eq_from_temp <- function(temp, partial_pressure = 400){
  henry_constant <- K0_CO2(0, temp)
  
  co2_eq <- henry_constant*(400*10^-6)*10^6
  
  return(co2_eq)
}

#calculate flux 
#flux in unit mmol co2/m2/d
q_points_flux <- q_points_predictions |> 
  left_join(network_slope) |> 
  mutate(wtr_pred = predict(wtr_model, newdata=data.frame(airt=airt)),
         v = v_from_q(discharge),
         kgas = kgas(v, slope, wtr_pred),
         co2_eq = co2_eq_from_temp(wtr_pred),
         flux = kgas*(co2_pred-co2_eq))

write_parquet(q_points_flux, "data/q_points_flux.parquet")
