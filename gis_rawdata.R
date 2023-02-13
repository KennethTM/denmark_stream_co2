source("libs_and_funcs.R")

#Download Denmark border and write to file
dk_border_raw <- raster::getData("GADM", country = "DNK", level = 0, path = "rawdata")

dk_border <- dk_border_raw |>
  st_as_sf() |>
  st_transform(dk_epsg)

st_write(dk_border, "rawdata/dk_border.sqlite", delete_layer = TRUE)

#Create vrt for hydro DEM tiles (1.6 meter resolution)
dem_files <- list.files("/media/kenneth/d6c13395-8492-49ee-9c0f-6a165e34c95c1/dnk_lake_catchments/rawdata/DHYM_RAIN", pattern = "*.ZIP", full.names = TRUE)

dem_asc_files <- sapply(dem_files, function(x){
  zip_files <- unzip(x, list = TRUE)
  asc_file <- zip_files$Name[grepl("*.asc", zip_files$Name)]
  asc_path <- paste0(x, "/", asc_file)
  return(asc_path)
})

gdalbuildvrt(paste0("/vsizip/", dem_asc_files), 
             "rawdata/dhym.vrt",
             allow_projection_difference = TRUE,
             a_srs = paste0("EPSG:", dk_epsg))

#Create national 20 m dem for drainage basin delineation (minimum function used for resampling)
gdalwarp(srcfile = "rawdata/dhym.vrt",
         dstfile = "rawdata/dhym_20m.tif",
         cutline = "rawdata/dk_border.sqlite",
         crop_to_cutline = TRUE,
         overwrite = TRUE,
         dstnodata = -9999,
         r = "min",
         co = c("COMPRESS=LZW"),
         wo = c("NUM_THREADS=ALL_CPUS"),
         tr = c(20, 20),
         multi = TRUE,
         wm = 8000)
