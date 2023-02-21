library(data.table);library(tidyverse);library(AquaEnv);library(sf);
library(lubridate);library(patchwork);library(terra);library(lwgeom);
library(exactextractr);library(nngeo);library(stars);library(arrow)

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

coords_to_col <- function(df){
  cbind(st_drop_geometry(df), data.frame(st_coordinates(df)))
}

ncdf_to_dt <- function(path){
  read_ncdf(path) |> 
    st_as_sf(as_points = TRUE, long=TRUE) |> 
    coords_to_col() |> 
    data.table()
}