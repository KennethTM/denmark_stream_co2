source("libs_and_funcs.R")

#Download Denmark border and write to file
dk_border_raw <- raster::getData("GADM", country = "DNK", level = 0, path = "data")

dk_border <- dk_border_raw |>
  st_as_sf() |>
  st_transform(dk_epsg)

st_write(dk_border, "data/dk_border.sqlite", delete_layer = TRUE)

#Create vrt for hydro DEM tiles (1.6 meter resolution)
dem_files <- list.files("/media/kenneth/d6c13395-8492-49ee-9c0f-6a165e34c95c1/dnk_lake_catchments/rawdata/DHYM_RAIN", pattern = "*.ZIP", full.names = TRUE)

dem_asc_files <- sapply(dem_files, function(x){
  zip_files <- unzip(x, list = TRUE)
  asc_file <- zip_files$Name[grepl("*.asc", zip_files$Name)]
  asc_path <- paste0(x, "/", asc_file)
  return(asc_path)
})

gdalbuildvrt(paste0("/vsizip/", dem_asc_files), 
             "data/dem/dhym.vrt",
             allow_projection_difference = TRUE,
             a_srs = paste0("EPSG:", dk_epsg))

#Create national 10 m dem for drainage basin delineation (minimum function used for resampling)
gdalwarp(srcfile = "data/dem/dhym.vrt",
         dstfile = "data/dem/dhym.tif",
         cutline = "data/dk_border.sqlite",
         crop_to_cutline = TRUE,
         overwrite = TRUE,
         dstnodata = -9999,
         r = "min",
         co = c("COMPRESS=LZW"),
         wo = c("NUM_THREADS=ALL_CPUS"),
         tr = c(10, 10),
         multi = TRUE,
         wm = 8000)

#Add lake attribute and site elevation
dk_lakes <- st_read(dk_lakes_path) |> 
  select(gml_id) |> 
  st_transform(dk_epsg)

q_points <- st_read("data/dk_model/DK_mh_2020_100m_QPoints.shp")

q_points_coords <- data.frame(st_coordinates(q_points))

dhym <- rast("data/dem/dhym.tif")

q_point_elev <- extract(dhym, vect(q_points))

#TODO remove downstream qpoints

q_points_lake <- q_points |> 
  bind_cols(q_points_coords) |> 
  rename(q_point_x = X, q_point_y = Y) |> 
  mutate(in_lake = lengths(st_intersects(q_points, dk_lakes)),
         site_elev = q_point_elev$dhym)

st_write(q_points_lake, "data/dk_model/q_points.shp", delete_dsn=TRUE)
