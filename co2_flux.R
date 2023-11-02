source("libs_and_funcs.R")

#Estimate CO2 fluxes from predicted CO2 concentration and hydrological variables
q_points_predictions <- read_parquet("data/q_points_predictions.parquet") |> 
  filter(#snap_dist < 100,
         #in_lake == 0,
         is.na(downstream))

#Derive relationship between air and water temperature
q_points_modeling <- read_parquet("data/q_points_modeling.parquet")

wtr_data <- q_points_modeling |> 
  select(airt, wtr, season) |> 
  na.omit()

wtr_model <- lm(wtr~airt, data = wtr_data)
summary(wtr_model)

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
