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
saveRDS(df_vals, "rawdata/static_features.rds") #WRITE TO PARQUET FILE

#Join air temperature data and catchments
all_ta <- rbind(ncdf_to_dt("rawdata/features/DK_Ta_20km_1989-2023.nc"), 
                ncdf_to_dt("rawdata/features/BH_Ta_20km_1989-2023.nc"))
all_ta <- all_ta[, Ta := as.numeric(Ta)][, time := as.Date(time)]
setkeyv(all_ta, c("X", "Y"))

all_ta_ids <- all_ta[time == as.Date("1989-01-02"), ] |> 
  as.data.frame() |> 
  st_as_sf(coords=c("X", "Y"), crs=25832) |> 
  select(-time, -Ta) |> 
  st_join(catchments, left=FALSE) |> 
  coords_to_col() |> 
  data.table()
setkeyv(all_ta_ids, c("X", "Y"))

catchment_ta_aggr <- merge(all_ta, all_ta_ids, allow.cartesian=TRUE)[, .(mean.airt = mean(Ta)), by= .(id, time)]

catchment_ta_aggr |> 
  as.data.frame() |> 
  write_parquet("rawdata/climate_airt.parquet")

#Join precipitation data and catchments
all_prec <- rbind(ncdf_to_dt("rawdata/features/DK_DMI_Corr_Precip_10km_1989-2023.nc"), 
                  ncdf_to_dt("rawdata/features/BH_DMI_Corr_Precip_10km_1989-2023.nc"))
all_prec <- all_prec[, P.DMI.corr := as.numeric(P.DMI.corr)][, time := as.Date(time)]
setkeyv(all_prec, c("X", "Y"))

all_prec_ids <- all_prec[time == as.Date("1989-01-02"), ] |> 
  as.data.frame() |> 
  st_as_sf(coords=c("X", "Y"), crs=25832) |> 
  select(-time, -P.DMI.corr) |> 
  st_join(catchments, left=FALSE) |> 
  coords_to_col() |> 
  data.table()
setkeyv(all_prec_ids, c("X", "Y"))

catchment_prec_aggr <- merge(all_prec, all_prec_ids, allow.cartesian=TRUE)[, .(mean.precip = mean(P.DMI.corr)), by= .(id, time)]

catchment_prec_aggr |> 
  as.data.frame() |> 
  write_parquet("rawdata/climate_precip.parquet")
