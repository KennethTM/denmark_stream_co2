source("libs_and_funcs.R")
setDTthreads(8)

#Feature extraction in qpoint catchments

#Load catchments
catchments <- st_read("rawdata/q_points_watersheds.sqlite")
q_points_snap <- st_read("rawdata/q_points_snap.sqlite")

#Cut lower part of catchment using 1km buffer around q_point
q_points_catchment_1km_list <- lapply(catchments$id, function(q){
  
  q_catch <- catchments[catchments$id == q, ]
  q_point <- q_points_snap[q_points_snap$id == q, "id"][1, ]
  
  q_catch_1km <- st_intersection(st_buffer(q_point, 1000), q_catch) |> 
    st_cast("MULTIPOLYGON") |> 
    select(id)
  
  return(q_catch_1km)
  
})

q_points_catchment_1km <- q_points_catchment_1km_list |> 
  rbindlist() |> 
  st_as_sf()

st_write("rawdata/q_points_catchment_1km.sqlite", delete_dsn=TRUE)

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

#bsm_vals_1km <- exact_extract(bsm_stack, q_points_catchment_1km, "mean", max_cells_in_memory=1e+9)
#names(bsm_vals_1km) <- paste0(names(bsm_vals_1km), "_1km")

#Combine extracted feature values and write to file 
df_vals <- cbind("id" = catchments$id,
                 "mean.elev" = dem_vals,
                 "mean.phraetic" = phraetic_vals, 
                 "mean.chalk" = chalk_vals, 
                 clay_slope_vals,
                 bsm_vals,
                 #bsm_vals_1km
                 )

write_parquet(df_vals, "rawdata/static_features.parquet") 

#Calculate catchment area
catchment_area <- catchments |> 
  mutate(catchment_area = as.numeric(st_area(GEOMETRY))) |> 
  st_drop_geometry()

catchment_area |> 
  as.data.frame() |> 
  write_parquet("rawdata/catchment_area.parquet")

#Aggregate daily air temperature data for catchments
all_ta <- rbind(ncdf_to_dt("rawdata/features/DK_Ta_20km_1989-2023.nc"), 
                ncdf_to_dt("rawdata/features/BH_Ta_20km_1989-2023.nc"))
all_ta <- all_ta[, Ta := as.numeric(Ta)][, time := as.IDate(time)]
setkeyv(all_ta, c("X", "Y"))

#Determine coverage fraction of climate cells for each catchment
one_day_ta <- all_ta[time == as.IDate("1989-01-02"), ] |> 
  as_tibble() |> 
  select(x=X, y=Y) |> 
  mutate(val=1)

one_day_ta_rast <- rast(one_day_ta)
coverage_ta_list <- exact_extract(one_day_ta_rast, catchments, include_xy=TRUE, include_cols="id")

#Aggregate per site and day in chunks
coverage_ta_list_split <- split(coverage_ta_list, ceiling(seq_along(coverage_ta_list)/5000))

agg_ta_list <- lapply(coverage_ta_list_split, function(list){
  
  coverage_ta_dt <- rbindlist(list)[, value := NULL]
  setnames(coverage_ta_dt, c("x", "y"), c("X", "Y"))
  setkeyv(coverage_ta_dt, c("X", "Y"))
  dt_agg <- merge(all_ta, coverage_ta_dt, allow.cartesian=TRUE)[, X := NULL][, Y := NULL][, .(mean.airt = sum(Ta*coverage_fraction)/sum(coverage_fraction)), by= .(id, time)]
  return(dt_agg)
  
})

#Write to file
rbindlist(agg_ta_list) |> 
  as.data.frame() |> 
  write_parquet("rawdata/climate_airt.parquet")

#Aggregate daily precipitation data for catchments
all_prec <- rbind(ncdf_to_dt("rawdata/features/DK_DMI_Corr_Precip_10km_1989-2023.nc"), 
                  ncdf_to_dt("rawdata/features/BH_DMI_Corr_Precip_10km_1989-2023.nc"))
all_prec <- all_prec[, P.DMI.corr := as.numeric(P.DMI.corr)][, time := as.IDate(time)]
setkeyv(all_prec, c("X", "Y"))

one_day_prec <- all_prec[time == as.IDate("1989-01-02"), ] |> 
  as_tibble() |> 
  select(x=X, y=Y) |> 
  mutate(val=1)

one_day_prec_rast <- rast(one_day_prec)
coverage_prec_list <- exact_extract(one_day_prec_rast, catchments, include_xy=TRUE, include_cols="id")

coverage_prec_list_split <- split(coverage_prec_list, ceiling(seq_along(coverage_prec_list)/5000))

agg_prec_list <- lapply(coverage_prec_list_split, function(list){
  
  coverage_prec_dt <- rbindlist(list)[, value := NULL]
  setnames(coverage_prec_dt, c("x", "y"), c("X", "Y"))
  setkeyv(coverage_prec_dt, c("X", "Y"))
  dt_agg <- merge(all_prec, coverage_prec_dt, allow.cartesian=TRUE)[, X := NULL][, Y := NULL][, .(mean.precip = sum(P.DMI.corr*coverage_fraction)/sum(coverage_fraction)), by= .(id, time)]
  return(dt_agg)
  
})

rbindlist(agg_prec_list) |> 
  as.data.frame() |> 
  write_parquet("rawdata/climate_precip.parquet")
