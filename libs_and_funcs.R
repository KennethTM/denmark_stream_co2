library(data.table);library(tidyverse);library(AquaEnv);library(sf);
library(lubridate);library(patchwork);library(terra);library(lwgeom);
library(exactextractr);library(nngeo);library(stars)
library(foreign);library(gdalUtilities);library(whitebox)
library(arrow);library(readxl);library(ggExtra)

set.seed(9999)

#Aq. Sci.:For most journals the figures should be 39 mm, 84 mm, 129 mm, or 174 mm wide and not higher than 234 mm.
theme_pub <- theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text = element_text(colour = "black"), 
        strip.background = element_rect(fill = "white"))
theme_set(theme_pub)

dk_epsg <- 25832

mpi_settings <- "mpiexec -n 8 "
taudem_path <- "/home/kenneth/TauDEM-more-mpi-deprecations/src/build/"

dk_lakes_path <- "/media/kenneth/d6c13395-8492-49ee-9c0f-6a165e34c95c1/autoencoder-for-lake-bathymetry/rawdata/DK_StandingWater.gml"

season_map <- data.frame(month = 1:12, 
                         season = c(rep("winter", 2),
                                    rep("spring", 3),
                                    rep("summer", 3),
                                    rep("autumn", 3),
                                    "winter"))

#Calculate stream velocity from hydraulic geometry relationships
#Raymond 2012
#q in m3/s, v in m/s
v_from_q <- function(q){
  v = 0.19 * q^0.29
  return(v)
}

#Raymond 2012, table 2, eq 5
#v in m/s, slope unitless, temp in celcius, kgas in m/d
kgas <- function(v, slope, temp){
  sc <- 1742 - 91.24*temp + 2.208*temp^2 - 0.0219*temp^3
  k600 <- v*slope*2841 + 2.02
  kgas <- k600*(sc/600)^-0.5
  return(kgas)
}

#Henrys constant from AquaEnv source
#unit mol/(kg-soln*atm)
lnK <- function(A, B, C, D, E, T){
  lnK <- A + (B/T) + C*log(T) + D*T + E*T^2
  return(lnK)
}

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

#Determine dissolved co2 in uM from temperature
#temp in celcius, partial pressure in uatm, co2_eq in uM
co2_eq_from_temp <- function(temp, partial_pressure = 400){
  henry_constant <- K0_CO2(0, temp)
  
  co2_eq <- henry_constant*(400*10^-6)*10^6
  
  return(co2_eq)
}

#Determine stream width based on regional empirical relationships to catchment area (km2 in equations)
#Not available for region 7 - which re-use relationship for region 1
#Relationships for each region in dk-model
region_relationships <- list("DK1" = \(x) 2.85*x^0.28,
                             "DK2" = \(x) 3.07*x^0.38,
                             "DK3" = \(x) 2.01*x^0.44,
                             "DK4+5_east" = \(x) 3.47*x^0.29,
                             "DK4+5_west" = \(x) 4.97*x^0.32,
                             "DK6" = \(x) 1.5*x^0.39,
                             "DK7" = \(x) 2.85*x^0.28)

width_from_area <- function(region, area){
  region_relationships[[region]](area)
}

#variable name map
var_name_map <- list("site_elev" = "Site elevation", 
                    "discharge_specific" = "Specific discharge", 
                    "overland" = "Overland flow", 
                    "overland_drain" = "Overland drain flow", 
                    "sz" = "Groundwater flow", 
                    "sz_drain" = "Groundwater drain flow", 
                    "airt" = "Air temperature", 
                    "precip" = "Precipitation", 
                    "catchment_area" = "Catchment area", 
                    "mean.phraetic" = "Phraetic", 
                    "mean.dhym" = "Elevation", 
                    "mean.dhym_slope" = "Slope", 
                    "mean.dhym_hand" = "HAND", 
                    "mean.clay_a" = "Clay A", 
                    "mean.clay_b" = "Clay B", 
                    "mean.clay_c" = "Clay C", 
                    "mean.clay_d" = "Clay D", 
                    "mean.artificial" = "Artifical", 
                    "mean.agriculture" = "Agriculture", 
                    "mean.forest" = "Forest", 
                    "mean.nature_eks_agriculture" = "Nature/agriculture", 
                    "mean.stream" = "Stream", 
                    "mean.lake" = "Lake", 
                    "mean.artificial_200m" = "Artifical (200 m)", 
                    "mean.agriculture_200m" = "Agriculture (200 m)", 
                    "mean.forest_200m" = "Forest (200 m)", 
                    "mean.nature_eks_agriculture_200m" = "Nature/agriculture (200 m)", 
                    "mean.stream_200m" = "Stream (200 m)", 
                    "mean.lake_200m" = "Lake (200 m)", 
                    "autumn" = "Autumn", 
                    "spring" = "Spring", 
                    "summer" = "Summer", 
                    "winter" = "Winter")


