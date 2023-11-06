source("libs_and_funcs.R")

#Preparation of features

#Process additional features
wbt_d8_pointer(dem="data/dem/dhym_breach_fill.tif",
               output = "data/dem/dhym_dirs.tif")

wbt_d8_flow_accumulation(input = "data/dem/dhym_dirs.tif",
                         output = "data/dem/dhym_accum.tif",
                         pntr = TRUE)

wbt_extract_streams(flow_accum = "data/dem/dhym_accum.tif",
                    output = "data/dem/dhym_streams.tif",
                    threshold = 1000)

wbt_elevation_above_stream(dem = "data/dem/dhym_breach_fill.tif",
                           streams = "data/dem/dhym_streams.tif",
                           output = "data/dem/dhym_hand.tif")

wbt_slope(dem="data/dem/dhym_breach_fill.tif",
          output="data/dem/dhym_slope.tif",
          units="degrees")

#Basemap04 variables
bsm_dbf <- read.dbf("data/features/Basemap04_public_geotiff/Basemap04_2021/lu_agg_2021.tif.vat.dbf")

var_codes <- list("artificial" = bsm_dbf$VALUE[1:19],
                  "agriculture" = bsm_dbf$VALUE[21:24],
                  "forest" = bsm_dbf$VALUE[25:26],
                  "nature_eks_agriculture" = bsm_dbf$VALUE[27:30],
                  "stream" = bsm_dbf$VALUE[31], 
                  "lake" = bsm_dbf$VALUE[32])

bsm <- rast("data/features/Basemap04_public_geotiff/Basemap04_2021/lu_agg_2021.tif")

#Create boolean rasters of basemap variables
for(i in 1:length(var_codes)){
  var_vals <- var_codes[[i]]
  var_name <- names(var_codes[i])
  
  bool_rast <- bsm %in% var_vals
  
  writeRaster(bool_rast, 
              paste0("data/features/", var_name, ".tif"),
              datatype="INT1U")
}

#Create buffer zones 200 m upstream in catchment
#Load catchments
catchments <- st_read("data/dk_model/q_points_catchments.sqlite")
q_points_snap <- st_read("data/dk_model/q_points_snap.sqlite")

#Cut lower part of catchment using 200 m buffer around q_point
q_points_upstream_200m_list <- lapply(catchments$id, function(q){
  
  q_catch <- catchments[catchments$id == q, ]
  q_point <- q_points_snap[q_points_snap$id == q, "id"][1, ]
  
  q_catch_200m <- st_intersection(st_buffer(q_point, 200), q_catch) |> 
    st_cast("MULTIPOLYGON") |> 
    select(id)
  
  return(q_catch_200m)
  
})

q_points_upstream_200m <- q_points_upstream_200m_list |> 
  rbindlist() |> 
  st_as_sf()

st_write(q_points_upstream_200m, "data/dk_model/q_points_upstream_200m.sqlite", delete_dsn=TRUE)

#Create climate grid for each season

#Air temperature
ta_bh <- read_ncdf("data/features/climate/BH_Ta_20km_1989-2023.nc") |> 
  as.data.frame()

ta_dk <- read_ncdf("data/features/climate/DK_Ta_20km_1989-2023.nc") |> 
  as.data.frame()

ta <- rbind(ta_dk, ta_bh)

ta_season <- ta |> 
  na.omit() |> 
  filter(between(year(time), 2000, 2009)) |> 
  mutate(month = month(time)) |> 
  left_join(season_map) |> 
  group_by(XUTM, YUTM, season) |> 
  summarise(airt=mean(as.numeric(Ta))) |> 
  ungroup() |> 
  spread(season, airt)

ta_raster <- rast(ta_season, type="xyz", crs="EPSG:25832")
writeRaster(ta_raster, "data/features/airtemp.tif")

#Precipitation
precip_bh <- read_ncdf("data/features/climate/BH_DMI_Corr_Precip_10km_1989-2023.nc") |> 
  as.data.frame()

precip_dk <- read_ncdf("data/features/climate/DK_DMI_Corr_Precip_10km_1989-2023.nc") |> 
  as.data.frame()

precip <- rbind(precip_dk, precip_bh)

precip_season <- precip |> 
  na.omit() |> 
  filter(between(year(time), 2000, 2009)) |> 
  mutate(month = month(time)) |> 
  left_join(season_map) |> 
  group_by(XUTM, YUTM, season) |> 
  summarise(precip=mean(as.numeric(P.DMI.corr))) |> 
  ungroup() |> 
  spread(season, precip)

precip_raster <- rast(precip_season, type="xyz", crs="EPSG:25832")
writeRaster(precip_raster, "data/features/precip.tif")
