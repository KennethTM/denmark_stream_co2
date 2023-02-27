source("libs_and_funcs.R")

dk_mod <- st_read("rawdata/dk_model_hip_2020.shp")

dk_mod_comb <- dk_mod |> 
  st_combine()

dk_lakes <- st_read("/media/kenneth/d6c13395-8492-49ee-9c0f-6a165e34c95c1/autoencoder-for-lake-bathymetry/rawdata/DK_StandingWater.gml") |> 
  select(gml_id) |> 
  st_transform(dk_epsg)

q_points <- st_read("rawdata/DK_mh_2020_100m_QPoints.shp")

q_points_lake <- q_points |> 
  bind_cols(data.frame(st_coordinates(q_points))) |> 
  rename(q_point_x = X, q_point_y = Y) |> 
  mutate(within_lake = lengths(st_intersects(q_points, dk_lakes)))

st_write(q_points_lake, "rawdata/q_points.sqlite")

q_df <- q_points_id |> 
  st_drop_geometry()

co2_data <- st_read("data/co2_data.sqlite")

# #Snap points to nearest stream node
# nearest <- st_nearest_points(co2_data, dk_mod_comb) |> 
#   st_cast("POINT")
# 
# point_snapped <- nearest[seq(2, nrow(co2_data)*2, 2)]
# 
# co2_data_snapped <- co2_data
# co2_data_snapped$GEOMETRY <- point_snapped
# co2_data_snapped$snap_distance <- as.numeric(st_distance(co2_data, co2_data_snapped, by_element = TRUE))
# 
# st_write(co2_data_snapped, "data/co2_data_snap.sqlite", delete_dsn=TRUE)

# #Read VP2 stream network
# vp2 <- st_read("rawdata/vp2.shp")
# 
# vp2_start <- vp2 |> 
#   st_cast("LINESTRING") |> 
#   st_startpoint()
# 
# dhym_10m <- rast("rawdata/dhym_10m_crop_breach.tif")
# 
# vp2_start_rast <- rasterize(vect(vp2_start), rast(dhym_10m), 
#                             background=0, field = 1)
# 
# writeRaster(vp2_start_rast, "rawdata/vp2_start.tif", datatype="INT1U")


#Join co2 data with nearest q point
nearest_q_point <- st_nearest_feature(co2_data, q_points)

co2_data_q_point <- bind_cols(co2_data, q_df[nearest_q_point, ])

#Join co2 and q data
#Q data
q_point_sf <- co2_data_q_point |> 
  select(site_id, q_point_x, q_point_y) |> 
  st_drop_geometry() |> 
  st_as_sf(coords=c("q_point_x", "q_point_y"), crs=25832)

co2_data_q_point$snap_dist <- as.numeric(st_distance(co2_data_q_point, q_point_sf, by_element = TRUE))

#Read dk model outputs
dk_model_files <- list.files("rawdata/dk_model_flow/", full.names = TRUE, pattern="*.csv")

dk_model_list <- lapply(dk_model_files, function(x){
  
  df <- fread(x)
  df <- melt(df, id.vars = "V1", variable.name="Name")
  df <- df[, .(date = as_date(V1), Name, value)]
  
  return(df)
})
names(dk_model_list) <- sub("_DK_[1-9].csv", "", basename(dk_model_files))

dk_model_df <- rbindlist(dk_model_list, idcol="component")
dk_model_df <- dcast(dk_model_df, Name + date ~ component, value.var = "value")

#Add dk_model data to co2 data
co2_data_dt <- co2_data_q_point |> 
  mutate(date = ymd(date)) |> 
  data.table()
setkeyv(co2_data_dt, c("Name", "date"))

co2_q_merge_dt <- dk_model_df[co2_data_dt]

co2_q_merge <- co2_q_merge_dt |> 
  tibble()

co2_q_merge_sf <- co2_q_merge |> 
  st_as_sf()

co2_q_merge |>
  filter(snap_dist < 100) |> 
  select(co2, Discharge:SZ_drain) |> 
  gather(variable, value, -co2) |> 
  ggplot(aes(value, co2))+
  geom_point(shape=1, alpha=0.2)+
  facet_wrap(variable~., scales="free")

co2_q_merge |>
  filter(snap_dist < 100) |> 
  mutate(month=month(date, label=TRUE)) |> 
  select(co2, Discharge:SZ_drain, month) |> 
  gather(variable, value, -co2, -month) |> 
  filter(value >= 0) |> 
  ggplot(aes(value, co2, col=month))+
  geom_smooth(se=FALSE)+
  facet_grid(month~variable, scales="free")
