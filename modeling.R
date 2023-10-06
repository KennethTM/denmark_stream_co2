source("libs_and_funcs.R")

model_data_raw <- read_parquet("data/model_data_raw.parquet")

preds <- names(model_data_raw)[26:60][-(6:10)]

model_data_sub <- model_data_raw |> 
  filter(between(year, 2000, 2009)) |> 
  filter(snap_dist < 100 & within_lake == 0) |> #remove sites snapping to same src_idx?
  mutate(month = month(date)) |> 
  select(site_id, month, reachno, co2, all_of(preds)) |> 
  mutate(season=case_when(month %in% c(6, 7, 8) ~ "summer",
                          month %in% c(9, 10, 11) ~ "autumn",
                          month %in% c(12, 1, 2) ~ "winter",
                          month %in% c(3, 4, 5) ~ "spring")) |> 
  select(-year, -doy, -month) |> 
  group_by(site_id, reachno, season) |> 
  add_tally() |> 
  summarise_all(list(mean), na.rm=TRUE) |> 
  ungroup()

summary(model_data_sub) #many na's in dk model components

model_data_na <- model_data_sub |> 
  na.omit() |> 
  filter(n >= 4) |> 
  select(-contains("_1km"), -n, -reachno)

model_data_na |> 
  select(-reachno, -site_id, -season) |> 
  gather(variable, value) |> 
  ggplot(aes(value))+
  geom_histogram()+
  facet_wrap(variable~., scales="free")

write_parquet(model_data_na, "data/model_data.parquet")
