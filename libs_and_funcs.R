library(data.table);library(tidyverse);library(AquaEnv);library(sf);
library(lubridate);library(patchwork);library(terra);library(lwgeom);
library(exactextractr);library(nngeo);library(stars)
library(foreign);library(gdalUtilities);library(whitebox)
library(arrow)

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
