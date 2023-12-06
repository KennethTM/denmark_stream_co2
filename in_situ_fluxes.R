source("libs_and_funcs.R")

#Processing in-situ flux measurements
#Aggregate and match measurements with dk-model qpoints manually using qgis
#Collect all data in "insitu_flux_qpoints.xlsx"

#Write shapefile with coords and create new column with nearest qpoint
rewet_flux <- read_excel("data/insitu_flux/co2_rewet_sites.xlsx")

rewet_flux |>
  st_as_sf(coords = c("longitude", "latitude"), crs=4326) |>
  st_transform(dk_epsg) |>
  group_by(site) |>
  slice(1) |>
  st_write("data/insitu_flux/rewet_sites.sqlite")

#River Tude data
tude_flux <- read_excel("data/insitu_flux/Tude Å datasamling.xlsx")

tude_flux_agg <- tude_flux |> 
  mutate(datetime_0 = ymd_hms(sub("1899-12-31 ", "2023-03-08 ", as.character(start))),
         datetime_1 = ymd_hms(sub("1899-12-31 ", "2023-03-08 ", as.character(slut)))) |> 
  select(datetime_0, datetime_1, site = Site, lat = bredde, long=længde, 
         co2_flux = Flux, ph, alk = Alkalinitet, temp=`Temp (c )`) |> 
  group_by(datetime_0, datetime_1, site) |> 
  summarise(co2_flux = mean(co2_flux),
            ph = mean(ph),
            alk = mean(alk),
            temp = mean(temp),
            n = n(),
            lat = mean(lat),
            long=mean(long)) |> 
  ungroup()

write_csv(tude_flux_agg, "data/insitu_flux/tude_flux_agg.csv")

tude_flux_agg |> 
  st_as_sf(coords = c("long", "lat"), crs=4326) |>
  st_write("data/insitu_flux/tude_sites.sqlite")
