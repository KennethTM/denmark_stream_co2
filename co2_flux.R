source("libs_and_funcs.R")

#Estimate CO2 fluxes from predicted CO2 concentration and hydrological variables

#Derive relationship between air and water temperature
q_points_modeling <- read_parquet("data/q_points_modeling.parquet")

wtr_data <- q_points_modeling |> 
  select(airt, wtr, season) |> 
  na.omit()

wtr_model <- lm(wtr~airt*season, data = wtr_data)
anova(wtr_model)
summary(wtr_model)

#Derive slope
#Either sample points regularly or just derive slope per stream branch? Or streamnet rasterized from taudem?
# network <- st_read("rawdata/dk_model_hip_2020.shp")
# 
# dist <- 200
# 
# network_segment <- st_line_sample(network, density = 1/dist)
# 
# network$geometry <- network_segment
# 
# network_points <- network |> 
#   select(BR_BrName) |> 
#   st_cast("POINT")
# 
# dem <- rast("data/dem/dhym.tif")
# 
# network_points$z <- extract(dem, vect(network_points), ID = FALSE, raw=TRUE)
# 
# network_points_slope <- network_points |> 
#   group_by(BR_BrName) |> 
#   mutate(drop = c(NA, diff(z)),
#          slope = abs(drop)/dist)

#q_points_predictions <- read_parquet("data/q_points_predictions.parquet")

#Calculate stream velocity from hydraulic geometry relationships
#q in m3/s
#raymond 2012
v_from_q <- function(q){
  v = 0.19 * q^0.29
  return(v)
}

#raymond 2012
kgas <- function(v, slope, temp){
  sc <- 1742 - 91.24 * temp + 2.208 * temp^2 - 0.0219 * temp^3
  kgas <- (v * slope * 2841 + 2.02)*(sc/600)^-0.5
  return(kgas)
}



#henrys constant from aquaenv source
K0_CO2 <- function(S, t)           
{
  T <- t + 273.15
  
  A <- 0.023517*S - 167.81077
  B <- 9345.17
  C <- 23.3585
  D <- -2.3656e-4*S
  E <- 4.7036e-7*S
  
  K0_CO2 <- exp(lnK(A, B, C, D, E, T))
  #attr(K0_CO2, "unit") <- "mol/(kg-soln*atm)"
  
  return (K0_CO2)
}

#determine dissolved co2 in uM from temperate
co2_eq_from_temp <- function(temp, partial_pressure = 400){
  henry_constant <- K0_CO2(0, temp)
  
  co2_eq <- henry_constant*(400*10^-6)*10^6
  
  return(co2_eq)
}



q_points_predictions |> 
  mutate(wtr_pred = predict(wtr_model, newdata=data.frame(airt=airt, season=season)),
         v = v_from_q(discharge),
         kgas = kgas(v, slope, wtr_pred),
         co2_eq = co2_eq_from_temp(wtr_pred),
         flux = kgas*(co2_pred-co2_eq)*XXXX)



