source("libs_and_funcs.R")

#Snap CO2 data to qpoints (original coordinates)
#join features to qpoints data
#write csv

#Merge data for modelling

#Read data
q_points_snap <- st_read("data/dk_model/q_points_snap.sqlite")

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
  ungroup()

#Join co2 data with nearest q point
nearest_q_point <- st_nearest_feature(q_points_orig_coords, co2_season)

q_points_snap_co2 <- bind_cols(q_points_snap, 
                               co2_site_id = co2_season$site_id[nearest_q_point],
                               co2_site_id_dist = as.numeric(st_distance(q_points_orig_coords, co2_season[nearest_q_point, ], by_element = TRUE)))

#Read DK-model outputs
flow_files <- list.files("data/dk_model/flow_components", full.names = TRUE, pattern="*.csv")

flow_season_map <- list("DJF"="winter", "JJA" = "summer", "MAM" = "spring", "SON" = "autumn")

flow_list <- lapply(flow_files, function(x){
  
  component <- sub(".csv", "", basename(x))
  df <- fread(x)
  df <- melt(df, id.vars = "V1", variable.name="name")
  df <- df[, .(component = component, month = unlist(flow_season_map[V1]), name, value)]
  
  return(df)
})

flow_df <- rbindlist(flow_list)
flow_df <- dcast(flow_df, name + month ~ component, value.var = "value")

#Read static features
static_features <- read_csv("data/features/static_features.csv")


#create long df with qpoints and seasons for rows
#and export with catchment features as csv