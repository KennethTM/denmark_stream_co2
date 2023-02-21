source("libs_and_funcs.R")

#Feature extraction in qpoint catchments

#Load catchments
catchments <- st_read("rawdata/q_points_watersheds.sqlite")

#File paths
clay_files <- list.files("rawdata/features", pattern="clay_*", full.names = TRUE)
slp_files <- "rawdata/features/slpdeg.tif"

chalk_file <- "rawdata/features/chalkdepth_DKM.tif"
phraetic_file <- "rawdata/features/dkm_2020_100m_phreatic_all_mean.tif"

#Load rasters
clay_slope <- rast(c(clay_files, slp_files))
chalk <- rast(chalk_file)
phraetic <- rast(phraetic_file)

#Extract values
phraetic_vals <- exact_extract(phraetic, catchments, "mean")
chalk_vals <- exact_extract(chalk, catchments, "mean")
clay_slope_vals <- exact_extract(clay_slope, catchments, "mean", max_cells_in_memory=1e+09)

df_vals <- cbind("mean.phraetic" = phraetic_vals$mean, "mean.chalk" = chalk_vals$mean, clay_slope_vals)

#Write to file
saveRDS(df_vals, "rawdata/static_features.rds")

#Join air temperature data and catchments
dk_ta <- read_ncdf("rawdata/features/DK_Ta_20km_1989-2023.nc") |> 
  st_as_sf(as_points = TRUE, long=TRUE) |> 
  st_set_crs(25832)

bh_ta <- read_ncdf("rawdata/features/BH_Ta_20km_1989-2023.nc") |> 
  st_as_sf(as_points = TRUE, long=TRUE) |> 
  st_set_crs(25832)

all_ta <- rbind(dk_ta, bh_ta)

all_ta_dt <- cbind(time = all_ta$time, airt = as.numeric(all_ta$Ta), data.frame(st_coordinates(all_ta))) |> 
  data.table()
setkeyv(all_ta_dt, c("X", "Y"))

grid_ta <- all_ta |> 
  filter(time == as_date("2000-01-01"))

grid_ta_ids <- st_join(grid_ta, catchments, left=FALSE)

grid_ta_ids_dt <- cbind(id = grid_ta_ids$id, data.frame(st_coordinates(grid_ta_ids))) |> 
  data.table()
setkeyv(grid_ta_ids_dt, c("X", "Y"))

catchment_ta <- merge(all_ta_dt, grid_ta_ids_dt, allow.cartesian=TRUE) #all_ta_dt[grid_ids_dt, allow.cartesian=TRUE]

catchment_ta_aggr <- catchment_ta[, .(mean.airt = mean(airt)), by= .(id, time)]

saveRDS(as.data.frame(catchment_ta_aggr), "rawdata/clim_features_airt.rds")

#Join precipitation data and catchments
dk_prec <- read_ncdf("rawdata/features/DK_DMI_Corr_Precip_10km_1989-2023.nc") |> 
  st_as_sf(as_points = TRUE, long=TRUE) |> 
  st_set_crs(25832)

bh_prec <- read_ncdf("rawdata/features/BH_DMI_Corr_Precip_10km_1989-2023.nc") |> 
  st_as_sf(as_points = TRUE, long=TRUE) |> 
  st_set_crs(25832)

all_prec <- rbind(dk_prec, bh_prec) |> 
  rename(precip = P.DMI.corr) |> 
  mutate(time = as.Date(time))

all_prec_dt <- cbind(time = all_prec$time, precip = as.numeric(all_prec$precip), data.frame(st_coordinates(all_prec))) |> 
  data.table()
setkeyv(all_prec_dt, c("X", "Y"))

grid_prec <- all_prec |> 
  filter(time == as_date("2000-01-01"))

grid_prec_ids <- st_join(grid_prec, catchments, left=FALSE)

grid_prec_ids_dt <- cbind(id = grid_prec_ids$id, data.frame(st_coordinates(grid_prec_ids))) |> 
  data.table()
setkeyv(grid_prec_ids_dt, c("X", "Y"))

catchment_prec <- merge(all_prec_dt, grid_prec_ids_dt, allow.cartesian=TRUE)

catchment_prec_aggr <- catchment_prec[, .(mean.precip = mean(precip)), by= .(id, time)]

saveRDS(as.data.frame(catchment_prec_aggr), "rawdata/clim_features_prec.rds")
