source("libs_and_funcs.R")

#Derive relationship between air and water temperature
q_points_modeling <- read_parquet("data/q_points_modeling.parquet")

wtr_data <- q_points_modeling |> 
  select(airt, wtr, season) |> 
  na.omit()

wtr_model <- lm(wtr~airt, data = wtr_data)
summary(wtr_model)

#Estimate CO2 fluxes from predicted CO2 concentration and hydrological variables
q_points_predictions <- read_parquet("data/q_points_predictions.parquet") |> 
  filter(#snap_dist < 100,
         #in_lake == 0,
         is.na(downstream))

#Extract slope values using virtual stream cell ids
dhym_net_slope <- rast("data/dem/dhym_net_slope.tif")

q_points_predictions$virtual_stream_slope <- extract(dhym_net_slope, q_points_predictions$virtual_stream_id)[[1]]

#Calculate flux 
#Flux in unit mmol co2/m2/d
q_points_flux <- q_points_predictions |> 
  mutate(wtr_pred = predict(wtr_model, newdata=data.frame(airt=airt)),
         v = v_from_q(discharge),
         kgas = kgas(v, virtual_stream_slope, wtr_pred),
         co2_eq = co2_eq_from_temp(wtr_pred),
         co2_flux = kgas*(co2_pred-co2_eq))

write_parquet(q_points_flux, "data/q_points_flux.parquet")

#TODO refactor considering static stream area??

#Estimate national flux from the stream network
network <- st_read("data/dk_model/dk_model_hip_2020.shp")

dk_border <- st_read("data/dk_border.sqlite")

dk_lakes <- st_read(dk_lakes_path) |> 
  select(gml_id) |> 
  st_transform(dk_epsg)

#Maximum extent of ice during most recent glaciation
#https://frisbee.geus.dk/geuswebshop/index.xhtml
ice_poly <- st_read("data/iceage/Isrand_poly.shp") |> 
  slice(1) |> 
  st_transform(dk_epsg) |>
  st_crop(dk_border)

#Determine region for qpoints from id string
q_points_region <- q_points_flux |> 
  mutate(region = case_when(grepl("DK1", branch) ~ "DK1",
                            grepl("DK2", branch) ~ "DK2",
                            grepl("DK3", branch) ~ "DK3",
                            grepl("DK4", branch) ~ "DK4",
                            grepl("DK5", branch) ~ "DK5",
                            grepl("DK6", branch) ~ "DK6",
                            grepl("DK7", branch) ~ "DK7",
                            TRUE ~ "other")) |> 
  select(x=q_point_x, y=q_point_y, index, season, region, co2_flux, catchment_area) |> 
  st_as_sf(crs=st_crs(dk_epsg), coords=c("x", "y"))

#Sample network and mask by dk_border and lake
#Add id of nearest qpoint site, expand by season and join flux
resolution <- 100
point_network <- st_line_sample(network, density = 1/resolution) |> 
  st_cast("POINT") |> 
  st_as_sf() |> 
  mutate(in_lake = lengths(st_intersects(x, dk_lakes)),
         in_dk = lengths(st_intersects(x, dk_border)),
         iceage_east = lengths(st_intersects(x, ice_poly))) |> 
  filter(in_lake == 0 & in_dk == 1) |> 
  mutate(sample_id = 1:n())

point_network_flux <- point_network |> 
  st_join(q_points_region["index"], join = st_nearest_feature) |> 
  st_drop_geometry() |> 
  expand_grid(data.frame(season = c("winter", "summer", "spring", "autumn"))) |> 
  left_join(st_drop_geometry(q_points_region))

#Calculate width and multiply by width*resolution
point_network_agg <- point_network_flux |> 
  mutate(region_iceage = case_when(region %in% c("DK4", "DK5") & iceage_east == 1 ~ "DK4+5_east",
                                   region %in% c("DK4", "DK5") & iceage_east == 0 ~ "DK4+5_west",
                                   TRUE ~ region),
         stream_width = map2_dbl(region_iceage, catchment_area, ~width_from_area(.x, .y/10^6)),
         stream_area = stream_width*resolution,
         stream_flux = stream_area*(co2_flux/1000*44)) |> #mmol co2/m2/day --> g co2/day
  group_by(season) |> 
  summarise(stream_flux = sum(stream_flux, na.rm=TRUE)*365, #g co2/year 
            stream_area = sum(stream_area),
            stream_width = mean(stream_width))

point_network_agg$stream_flux |> mean() * 10^-9 #gigagram co2/year

#stream area does not vary per season
#calc flux per stream area and country area
