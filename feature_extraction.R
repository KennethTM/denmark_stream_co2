source("libs_and_funcs.R")

#Load catchments
catchments <- st_read("data/dk_model/q_points_catchments.sqlite")
upstream_200m <- st_read("data/dk_model/q_points_upstream_200m.sqlite")

#Feature extraction in qpoint catchments

#File paths
clay_files <- list.files("data/features", pattern="clay_*", full.names = TRUE)
dem_file <- "data/dem/dhym.tif"
slp_file <- "data/dem/dhym_slope.tif"
hand_file <- "data/dem/dhym_hand.tif"
basemap_features <- c("artificial", "agriculture", "forest",
                      "nature_eks_agriculture","stream", "lake")
basemap_files <- paste0("data/features/", basemap_features, ".tif")

#Load rasters
clay <- rast(clay_files)
chalk <- rast("data/features/chalkdepth_DKM.tif")
phraetic <- rast("data/features/dkm_2020_100m_phreatic_all_mean.tif")
morpho <- rast(c(dem_file, slp_file, hand_file))
basemap <- rast(basemap_files)
names(basemap) <- basemap_features

airtemp <- rast("data/features/airtemp.tif")
names(airtemp) <- paste0(names(airtemp), "_airt")
precip <- rast("data/features/precip.tif")
names(precip) <- paste0(names(precip), "_precip")

#Extract values
phraetic_vals <- exact_extract(phraetic, catchments, "mean")
chalk_vals <- exact_extract(chalk, catchments, "mean")
clay_vals <- exact_extract(clay, catchments, "mean", max_cells_in_memory=1e+09)
#morpho_vals <- exact_extract(morpho, catchments, "mean", max_cells_in_memory=1e+09)
morpho_vals <- readRDS("morpho_vals.rds")

#basemap_vals <- exact_extract(basemap, catchments, "mean", max_cells_in_memory=1e+9)
basemap_vals <- readRDS("basemap_vals.rds")

basemap_vals_200m <- exact_extract(basemap, upstream_200m, "mean", max_cells_in_memory=1e+9)
names(basemap_vals_200m) <- paste0(names(basemap_vals_200m), "_200m")

airtemp_vals <- exact_extract(airtemp, catchments, "mean")
precip_vals <- exact_extract(precip, catchments, "mean")

#Calculate catchment area
catchment_area <- catchments |> 
  mutate(catchment_area = as.numeric(st_area(GEOMETRY))) |> 
  st_drop_geometry() |> 
  as.data.frame()

#Combine extracted feature values and write to file 
static_features <- cbind(catchment_area,
                         "mean.phraetic" = phraetic_vals, 
                         "mean.chalk" = chalk_vals, 
                         morpho_vals,
                         clay_vals,
                         basemap_vals,
                         basemap_vals_200m,
                         airtemp_vals,
                         precip_vals)

write_csv(static_features, "data/features/static_features.csv")
