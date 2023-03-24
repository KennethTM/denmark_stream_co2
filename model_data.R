source("libs_and_funcs.R")

#Merge data for modelling

#Read data
q_points_snap <- st_read("rawdata/q_points_snap.sqlite") #qpoints snapped to src grid cells

co2_data <- st_read("data/co2_data.sqlite")

#Join co2 data with nearest q point
nearest_q_point <- st_nearest_feature(co2_data, q_points_snap)

co2_q_point <- bind_cols(co2_data, st_drop_geometry(q_points_snap)[nearest_q_point, ]) |> 
  rename(Name = name)

q_point_sf <- co2_q_point |> 
  select(site_id, q_point_x, q_point_y) |> 
  st_drop_geometry() |> 
  st_as_sf(coords=c("q_point_x", "q_point_y"), crs=25832)

co2_q_point$snap_dist <- as.numeric(st_distance(co2_q_point, q_point_sf, by_element = TRUE))

st_write(co2_q_point, "rawdata/q_points_co2.sqlite", delete_dsn=TRUE)

#Climate features
airt <- read_parquet("rawdata/climate_airt.parquet") |> 
  rename(date = time) |> 
  data.table()

precip <- read_parquet("rawdata/climate_precip.parquet") |> 
  rename(date = time) |> 
  data.table()

#Catchment (static features)
static <- read_parquet("rawdata/static_features.parquet") |> 
  data.table()

#Catchment area
catchment_area <- read_parquet("rawdata/catchment_area.parquet") |> 
  data.table()

#Read DK-model outputs
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

#Read DK-model 1km UPSTREAM INTEGRATED
dk_model_1km_files <- list.files("rawdata/dk_model_flow_1km_upstream_v2/", full.names = TRUE, pattern="*.csv")

dk_model_1km_list <- lapply(dk_model_1km_files, function(x){
  
  df <- fread(x)
  df <- melt(df, id.vars = "V1", variable.name="Name")
  df <- df[, .(date = as_date(V1), Name, value)]
  
  return(df)
})
names(dk_model_1km_list) <- paste0(sub("_DK_[1-9].csv", "", basename(dk_model_1km_files)), "_1km")

dk_model_1km_df <- rbindlist(dk_model_1km_list, idcol="component")
dk_model_1km_df <- dcast(dk_model_1km_df, Name + date ~ component, value.var = "value")

#Merge CO2 data and features based on id and date
co2_q_point_dt <- co2_q_point |> 
  st_drop_geometry() |> 
  mutate(date = ymd(date)) |> 
  data.table()

co2_data_merge <- merge(co2_q_point_dt, dk_model_df, by=c("Name", "date"), all.x=TRUE)
co2_data_merge <- merge(co2_data_merge, dk_model_1km_df, by=c("Name", "date"), all.x=TRUE)
co2_data_merge <- merge(co2_data_merge, airt, by=c("id", "date"), all.x=TRUE)
co2_data_merge <- merge(co2_data_merge, precip, by=c("id", "date"), all.x=TRUE)
co2_data_merge <- merge(co2_data_merge, static, by=c("id"), all.x=TRUE)
co2_data_merge <- merge(co2_data_merge, catchment_area, by=c("id"), all.x=TRUE)
co2_data_merge <- co2_data_merge[, doy := yday(date)][, year := year(date)]

#Write to file
co2_data_merge |> 
  as.data.frame() |>
  write_parquet("data/model_data_raw.parquet")
