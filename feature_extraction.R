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

#Extract values
phraetic_vals <- exact_extract(phraetic, catchments, "mean")
chalk_vals <- exact_extract(chalk, catchments, "mean")
clay_vals <- exact_extract(clay, catchments, "mean", max_cells_in_memory=1e+09)
morpho_vals <- exact_extract(morpho, catchments, "mean", max_cells_in_memory=1e+09)

basemap_vals <- exact_extract(basemap, catchments, "mean", max_cells_in_memory=1e+9)

basemap_vals_200m <- exact_extract(basemap, upstream_200m, "mean", max_cells_in_memory=1e+9)
names(basemap_vals_200m) <- paste0(names(basemap_vals_200m), "_200m")

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
                         basemap_vals_200m)

write_csv(static_features, "data/features/static_features.csv")

#Extract dynamic climate variables and write to file
airtemp <- rast("data/features/airtemp.tif")
precip <- rast("data/features/precip.tif")

airtemp_vals <- exact_extract(airtemp, catchments, "mean")
precip_vals <- exact_extract(precip, catchments, "mean")

airtemp_df <- bind_cols(id = catchments$id, airtemp_vals) |> 
  gather(season, airt, -id) |> 
  mutate(season=sub("mean.", "", season))

precip_df <- bind_cols(id = catchments$id, precip_vals) |> 
  gather(season, precip, -id) |> 
  mutate(season=sub("mean.", "", season))

left_join(airtemp_df, precip_df) |> 
  write_csv("data/features/climate_features.csv")
