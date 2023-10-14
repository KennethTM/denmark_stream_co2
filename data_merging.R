source("libs_and_funcs.R")

#Merge data for modelling

#Read data
q_points_snap <- st_read("data/dk_model/q_points_snap.sqlite")

q_point_snap_coords <- as.data.frame(st_coordinates(q_points_snap))
names(q_point_snap_coords) <- c("q_point_snap_x", "q_point_snap_y")

q_points_df <- bind_cols(st_drop_geometry(q_points_snap), q_point_snap_coords)

q_points_orig_coords <- data.frame(x = q_points_snap$q_point_x, 
                                   y = q_points_snap$q_point_y) |> 
  st_as_sf(crs=st_crs(q_points_snap), coords=c("x", "y"))

#Read and filter co2 data
co2_data <- st_read("data/stream_data/co2_data.sqlite")

co2_season <- co2_data |> 
  mutate(date = ymd(date),
         month = month(date)) |> 
  filter(between(year(date), 2000, 2009)) |> 
  left_join(season_map) |> 
  group_by(site_id, season) |> 
  summarise(alk = mean(alk), ph=mean(ph), wtr=mean(wtr), 
            co2=mean(co2), co2_sat=mean(co2_sat),
            fco2=mean(fco2), fco2_sat=mean(fco2_sat), n=n()) |> 
  ungroup() |> 
  filter(n >= 4)

co2_coords <- as.data.frame(st_coordinates(co2_season))
names(co2_coords) <- c("co2_coords_x", "co2_coords_y")

co2_season_df <- bind_cols(st_drop_geometry(co2_season), co2_coords) |> 
  rename(co2_site_id = site_id)

#Join co2 data with nearest q point
nearest_co2_site <- st_nearest_feature(q_points_orig_coords, co2_season)

co2_site_dist <- as.numeric(st_distance(q_points_orig_coords, co2_season[nearest_co2_site, ], by_element = TRUE))

q_points_co2_df <- bind_cols(q_points_df, 
                             co2_site_id = co2_season$site_id[nearest_co2_site],
                             co2_site_id_dist = co2_site_dist)

#Read DK-model outputs
flow_files <- list.files("data/dk_model/flow_components", full.names = TRUE, pattern="*.csv")

flow_season_map <- list("DJF"="winter", "JJA" = "summer", "MAM" = "spring", "SON" = "autumn")

flow_list <- lapply(flow_files, function(x){
  
  component <- sub(".csv", "", basename(x))
  df <- fread(x)
  df <- melt(df, id.vars = "V1", variable.name="name")
  df <- df[, .(component = component, season = unlist(flow_season_map[V1]), name, value)]
  
  return(df)
})

flow_df <- rbindlist(flow_list)
flow_df <- dcast(flow_df, name + season ~ component, value.var = "value")
flow_df <- as.data.frame(flow_df)
names(flow_df) <- tolower(names(flow_df))

#Read static features
#Perform median imputation (only chalk has NAs in in modeling data)
static_features <- read_csv("data/features/static_features.csv")


#Read climate features
climate_features <- read_csv("data/features/climate_features.csv")

#Merge initial table
q_points_features <- q_points_co2_df |> 
  left_join(flow_df, multiple = "all") |> 
  left_join(climate_features) |> 
  left_join(static_features) |> 
  left_join(co2_season_df)

saveRDS(q_points_features, "data/q_points_features.rds")

#Filter observations for modeling
#Some sites share snap to the same virtual stream cell (same catchment), keep only the one with highest n
q_points_modeling <- q_points_features |> 
  filter(co2_site_id_dist < 100,
         snap_dist < 100,
         !is.na(co2),
         in_lake == 0) |> 
  group_by(co2_site_id, season) |> 
  slice_min(co2_site_id_dist)

#do some co2 sites have same coordinates but different ids?