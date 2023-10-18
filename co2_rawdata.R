source("libs_and_funcs.R")

#Process and merge chemistry and field observations in streams
chem_raw <- read.csv2("data/stream_data/stream_chem.csv")
chem <- data.table(chem_raw)

field_raw <- read.csv2("data/stream_data/stream_field.csv")
field <- data.table(field_raw)

chem_clean <- chem[,.(site_id = ObservationsStedNr, site_name = ObservationsStedNavn,
                      x_coord = Xutm_Euref89_Zone32, y_coord = Yutm_Euref89_Zone32,
                      date = as.Date(as.character(Startdato), format = "%Y%m%d"), var_unit = paste0("chem_", Parameter, "_", Enhed),
                      value = Resultat)]

field_clean <- field[,.(site_id = ObservationsStedNr, site_name = ObservationsStedNavn, 
                        x_coord = Xutm_Euref89_Zone32, y_coord = Yutm_Euref89_Zone32,
                        date = as.Date(as.character(Startdato), format = "%Y%m%d"), var_unit = paste0("field_", Parameter, "_", Enhed), 
                        value = Resultat)]

all <- rbindlist(list(chem_clean, field_clean))

all_wide <- dcast(all, site_id + site_name + x_coord + y_coord + date ~ var_unit, value.var = "value", fun = mean, fill = NA)

all_clean <- all_wide[, .(site_id, site_name, date,
                          x_coord, y_coord,
                          alk = `chem_Alkalinitet,total TA_mmol/l`,
                          ph = fcoalesce(field_pH_pH, chem_pH_pH),
                          wtr = `field_Vandtemperatur_grader C`)]

all_na_data_frame <- all_clean |> 
  na.omit() |> 
  data.frame()

#Calculate pco2 from alkalinity, ph and water temperature
#units: co2 umol/liter, fco2 uatm
co2_data <- all_na_data_frame |> 
  filter(alk < 10, ph > 5.4, wtr > 0, wtr < 30) |> 
  mutate(aquaenv = pmap(list(wtr, ph, alk), ~aquaenv(S=0, t=..1, SumCO2 = NULL, pH = ..2, TA = ..3/1000)),
         co2 = map_dbl(aquaenv, ~ .$CO2)*10^6, 
         co2_sat = map_dbl(aquaenv, ~ .$CO2_sat)*10^6,
         fco2 = map_dbl(aquaenv, ~ .$fCO2)*10^6,
         fco2_sat = map_dbl(aquaenv, ~ .$fCO2atm)*10^6) |> 
  filter(fco2 < 40000)

co2_data_raw <- co2_data |> 
  select(-aquaenv) |> 
  rename(co2_site_id = site_id,
         co2_site_x = x_coord, 
         co2_site_y = y_coord)

saveRDS(co2_data_raw, "data/stream_data/co2_data_raw.rds")

#aggregate co2 data by season
co2_data_agg <- co2_data_raw |> 
  mutate(month = month(date)) |> 
  filter(between(year(date), 2000, 2009)) |> 
  left_join(season_map) |> 
  group_by(co2_site_id, season) |> 
  summarise(co2_site_x = mean(co2_site_x),
            co2_site_y = mean(co2_site_y),
            alk = mean(alk), ph=mean(ph), wtr=mean(wtr), 
            co2=mean(co2), co2_sat=mean(co2_sat),
            fco2=mean(fco2), fco2_sat=mean(fco2_sat), n=n()) |> 
  ungroup() |> 
  filter(n >= 4)

saveRDS(co2_data_agg, "data/stream_data/co2_data_agg.rds")
