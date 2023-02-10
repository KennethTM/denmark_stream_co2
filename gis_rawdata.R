source("libs_and_funcs.R")
library(raster)
#library(gdalUtils) #not available anymore, alternatives exists, e.g. the gdalUtilities package

#Download Denmark border and write to file
dk_border_raw <- getData("GADM", country = "DNK", level = 0, path = "rawdata")

dk_border <- dk_border_raw %>%
  st_as_sf() %>%
  st_crop(xmin = 8, ymin = 54.56, xmax = 14, ymax = 57.76) %>%
  st_transform(dk_epsg) %>%
  st_write("rawdata/dk_border.sqlite")

#Create vrt for hydro DEM tiles (1.6 meter resolution)
dem_files <- list.files("rawdata/DHYM_RAIN", pattern = "*.ZIP", full.names = TRUE)

dem_asc_files <- sapply(dem_files, function(x){
  zip_files <- unzip(x, list = TRUE)
  asc_file <- zip_files$Name[grepl("*.asc", zip_files$Name)]
  asc_path <- paste0(x, "/", asc_file)
  return(asc_path)
})

gdalbuildvrt(paste0("/vsizip/", dem_asc_files), 
             "rawdata/dhym_10m.vrt",
             allow_projection_difference = TRUE,
             a_srs = paste0("EPSG:", dk_epsg))

#Create national 10 m dem for drainage basin delineation (minimum function for resampling)
gdalwarp(srcfile = "rawdata/dhym_10m.vrt",
         dstfile = "rawdata/dhym_10m.tif",
         cutline = "rawdata/dk_border.sqlite",
         crop_to_cutline = TRUE,
         overwrite = TRUE,
         dstnodata = -9999,
         r = "min",
         co = c("COMPRESS=LZW", "BIGTIFF=YES"),
         tr = c(10, 10),
         multi = TRUE,
         wm = 4000)
