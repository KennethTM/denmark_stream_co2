source("libs_and_funcs.R")

#Process and merge rawdata of chemistry and field observations in streams
chem_raw <- read.csv2("rawdata/stream_chem.csv")
chem <- data.table(chem_raw)

field_raw <- read.csv2("rawdata/stream_field.csv")
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

all_na <- na.omit(all_clean)

all_na_data_frame <- data.frame(all_na)

write.csv(all_na_data_frame, "data/chemistry_data.csv", row.names = FALSE)

#RAWDATA NEEDS SOME CLEANING

#Calculate pco2 from alkalinity, ph and water temperature
#Create point vector file and save
chemistry_data <- read.csv( "data/chemistry_data.csv") |> 
  tibble()

#co2 umol liter, fco2 uatm
co2_data <- chemistry_data |> 
  filter(alk < 10, ph > 5.4, wtr > 0, wtr < 30) |> 
  mutate(aquaenv = pmap(list(wtr, ph, alk), ~aquaenv(S=0, t=..1, SumCO2 = NULL, pH = ..2, TA = ..3/1000)),
         co2 = map_dbl(aquaenv, ~.$CO2)*10^6, 
         co2_sat = map_dbl(aquaenv, ~.$CO2_sat)*10^6,
         fco2 = map_dbl(aquaenv, ~.$fCO2)*10^6,
         fco2_sat = map_dbl(aquaenv, ~.$fCO2atm)*10^6) |> 
  filter(fco2 < 40000)

co2_data_vector <- co2_data |> 
  select(-aquaenv) |> 
  st_as_sf(coords=c("x_coord","y_coord"), crs=25832)

st_write(co2_data_vector, "data/co2_data.sqlite", delete_dsn=TRUE)
