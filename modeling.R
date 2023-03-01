source("libs_and_funcs.R")

model_data_raw <- read_parquet("data/model_data_raw.parquet")

preds <- names(model_data_raw)[26:49]

model_data_sub <- model_data_raw |> 
  filter(snap_dist < 150 & within_lake == 0) |> #remove sites snapping to same src_idx?
  select(reachno, co2, all_of(preds))

summary(model_data_sub) #many na's in dk model components

model_data_na <- model_data_sub |> 
  na.omit()

model_data_na |> 
  select(-reachno) |> 
  gather(variable, value) |> 
  ggplot(aes(value))+
  geom_histogram()+
  facet_wrap(variable~., scales="free")

write_parquet(model_data_na, "data/model_data.parquet")
