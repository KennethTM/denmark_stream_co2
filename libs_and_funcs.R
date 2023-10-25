library(data.table);library(tidyverse);library(AquaEnv);library(sf);
library(lubridate);library(patchwork);library(terra);library(lwgeom);
library(exactextractr);library(nngeo);library(stars)
library(foreign);library(gdalUtilities);library(whitebox)
library(arrow);library(readxl)

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
