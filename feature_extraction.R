source("libs_and_funcs.R")

#Feature extraction in qpoint catchments

#Load catchments
catchments <- st_read("rawdata/q_points_watersheds.sqlite")

#Clay, slope, chalk and phraetic variables
#File paths
clay_files <- list.files("rawdata/features", pattern="clay_*", full.names = TRUE)
slp_files <- "rawdata/features/slpdeg.tif"

chalk_file <- "rawdata/features/chalkdepth_DKM.tif"
phraetic_file <- "rawdata/features/dkm_2020_100m_phreatic_all_mean.tif"

dem_file <- "/media/kenneth/d6c13395-8492-49ee-9c0f-6a165e34c95c1/watershed_diversity/data_raw/dhym_10m.tif"

#Load rasters
clay_slope <- rast(c(clay_files, slp_files))
chalk <- rast(chalk_file)
phraetic <- rast(phraetic_file)
dem <- rast(dem_file)

#Extract values
dem_vals <- exact_extract(dem, catchments, "mean", max_cells_in_memory=1e+09)
phraetic_vals <- exact_extract(phraetic, catchments, "mean")
chalk_vals <- exact_extract(chalk, catchments, "mean")
clay_slope_vals <- exact_extract(clay_slope, catchments, "mean", max_cells_in_memory=1e+09)

#Basemap04 variables
bsm_dbf <- read.dbf("rawdata/features/Basemap04_public_geotiff/Basemap04_2021/lu_agg_2021.tif.vat.dbf")

var_codes <- list("artificial" = bsm_dbf$VALUE[1:19],
                  "agriculture" = bsm_dbf$VALUE[21:24],
                  "forest" = 311000,
                  "nature_agriculture" = bsm_dbf$VALUE[26:30],
                  "stream" = 412000, 
                  "lake" = 411000)

bsm <- rast("rawdata/features/Basemap04_public_geotiff/Basemap04_2021/lu_agg_2021.tif")

for(i in 1:length(var_codes)){
  var_vals <- var_codes[[i]]
  var_name <- names(var_codes[i])
  
  bool_rast <- bsm %in% var_vals
  
  writeRaster(bool_rast, 
              paste0("rawdata/features/", var_name, ".tif"),
              datatype="INT1U")
}

#Extract basemap04 values
bsm_files <- paste0("rawdata/features/", names(var_codes), ".tif")

bsm_stack <- rast(bsm_files)
names(bsm_stack) <- names(var_codes)

bsm_vals <- exact_extract(bsm_stack, catchments, "mean", max_cells_in_memory=1e+9)

#Combine extracted feature values and write to file 
df_vals <- cbind("id" = catchments$id,
                 "mean.elev" = dem_vals,
                 "mean.phraetic" = phraetic_vals, 
                 "mean.chalk" = chalk_vals, 
                 clay_slope_vals,
                 bsm_vals)

write_parquet(df_vals, "rawdata/static_features.parquet") 

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

#Calculate catchment area
catchment_area <- catchments |> 
  mutate(catchment_area = as.numeric(st_area(GEOMETRY))) |> 
  st_drop_geometry()

catchment_area |> 
  as.data.frame() |> 
  write_parquet("rawdata/catchment_area.parquet")

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
