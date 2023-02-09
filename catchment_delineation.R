source("libs_and_funcs.R")
#library(gdalUtils) #not available anymore, alternatives exists, e.g. the gdalUtilities package

#https://hydrology.usu.edu/taudem/taudem5/TauDEM53CommandLineGuide.pdf

#Crop DEM
gdalwarp(srcfile = "rawdata/dhym_10m.tif",
         dstfile = "rawdata/dhym_10m_crop.tif",
         cutline = "rawdata/dk_border.sqlite",
         crop_to_cutline = TRUE,
         co = "COMPRESS=LZW")

#Compute DEM slope
gdaldem(mode = "slope",
        input_dem = "rawdata/dhym_10m_crop.tif",
        output = "rawdata/dhym_10m_slope.tif",
        p = TRUE,
        co = "COMPRESS=LZW")

#Preprocess DEM using using RichDEM Python library ("dem_preproc.py" script)

# #Read VP2 stream network
# vp2 <- st_read("rawdata/vp2.shp")
# 
# vp2_start <- vp2 |> 
#   st_cast("LINESTRING") |> 
#   st_startpoint()
# 
# dhym_10m <- rast("rawdata/dhym_10m_crop_breach.tif")
# 
# vp2_start_rast <- rasterize(vect(vp2_start), rast(dhym_10m), 
#                             background=0, field = 1)
# 
# writeRaster(vp2_start_rast, "rawdata/vp2_start.tif", datatype="INT1U")

#Flowdirs
taudem_flowdir <- paste0(mpi_settings, taudem_path, "d8flowdir ",
                         " -p ", "rawdata/dhym_10m_breach_p.tif",
                         " -sd8 ", "rawdata/dhym_10m_breach_sd8.tif",
                         " -fel ", "rawdata/dhym_10m_crop_breach.tif")
system(taudem_flowdir)

#Flow accumulation weighted by stream start points
taudem_acc <- paste0(mpi_settings, taudem_path, "aread8",
                     " -p ", "rawdata/dhym_10m_breach_p.tif",
                     " -ad8 ", "rawdata/dhym_10m_breach_ad8.tif",
                     #" -wg ", "rawdata/vp2_start.tif",
                     " -nc")
system(taudem_acc)

#Threshold stream network
taudem_threshold <- paste0(mpi_settings, taudem_path, "threshold",
                           " -src ", "rawdata/dhym_10m_breach_src.tif",
                           " -ssa ", "rawdata/dhym_10m_breach_ad8.tif",
                           " -thresh 1000")
system(taudem_threshold)

#Snap points to stream network along flow directions
taudem_snap <- paste0(taudem_path, "moveoutletstostrm",
                      " -p ", "rawdata/dhym_10m_breach_p.tif",
                      " -src ", "rawdata/dhym_10m_breach_src.tif",
                      " -o ", "rawdata/DK_mh_2020_100m_QPoints.shp",
                      " -om ", "rawdata/qpoints_snap.shp")
system(taudem_snap)


#Test om det er nødvendigt at seede med stream start points??



stream_sites_snap <- st_read("rawdata/qpoints_snap.shp")

#NESTED WATERSHED DELINEATION
#Delineate watershed draining to stream outlets
taudem_gage <- paste0(mpi_settings, taudem_path, "gagewatershed",
                      " -p ", "rawdata/dhym_10m_breach_p.tif",
                      " -o ", "rawdata/DK_mh_2020_100m_QPoints.shp",
                      " -gw ", "rawdata/dhym_watersheds.tif",
                      " -id ", "rawdata/dhym_watersheds.txt")
system(taudem_gage)

#Raster to polygon
polygonize <- paste0("pkpolygonize ",
                     " -i ", "rawdata/dhym_watersheds.tif",
                     " -m ", "rawdata/dhym_watersheds.tif",
                     " -o ", "rawdata/dhym_watersheds.sqlite")
system(polygonize)

#Clean polygons
gw_clean <- st_read("rawdata/dhym_watersheds.sqlite") %>% 
  st_transform(dk_epsg) %>% 
  st_make_valid() %>% 
  group_by(dn) %>% 
  summarise() %>% 
  st_cast("MULTIPOLYGON") %>% 
  st_remove_holes() %>% 
  rename(id = dn) %>% 
  st_join(stream_sites_snap) %>% 
  mutate(nested_area = as.numeric(st_area(geometry)))

#Calculate total non-overlapping area covered by catchments
st_write(gw_clean, "data/qpoint_watersheds.sqlite", delete_dsn = TRUE)

# #NON-NESTED WATERSHED DELINEATION
# for(i in 1:nrow(stream_sites_snap)){
#   
#   print(paste0("Delineating stream watershed ", i))
#   
#   st_write(stream_sites_snap[i, ], paste0(getwd(), "/data/watershed_tmp/outlet_", i, ".sqlite"))
#   
#   #Delineate watershed draining to lake boundary points
#   taudem_gage <- paste0(mpi_settings, taudem_path, "gagewatershed",
#                         " -p ", p_raster,
#                         " -o ", paste0(getwd(), "/data/watershed_tmp/outlet_", i, ".sqlite"),
#                         " -gw ", paste0(getwd(), "/data/watershed_tmp/outlet_", i, ".tif"))
#   system(taudem_gage)
#   
#   polygonize <- paste0("pkpolygonize ",
#                        " -i ", paste0(getwd(), "/data/watershed_tmp/outlet_", i, ".tif"),
#                        " -m ", paste0(getwd(), "/data/watershed_tmp/outlet_", i, ".tif"),
#                        " -o ", paste0(getwd(), "/data/watershed_tmp/gw_", i, ".sqlite"))
#   system(polygonize)
#   
#   file.remove(paste0(getwd(), "/data/watershed_tmp/outlet_", i, ".sqlite"))
#   file.remove(paste0(getwd(), "/data/watershed_tmp/outlet_", i, ".tif"))
# }
# 
# #Load and clean all non-nested watershed files
# gw_nonnest_list <- lapply(1:nrow(stream_sites_snap), function(i){
#   st_read(paste0(getwd(), "/data/watershed_tmp/gw_", i, ".sqlite")) %>% 
#     st_transform(25832) %>% 
#     st_make_valid() %>% 
#     st_union() %>% 
#     st_as_sf() %>% 
#     st_cast("MULTIPOLYGON") %>% 
#     st_remove_holes() %>% 
#     mutate(id = i,
#            total_area = as.numeric(st_area(geometry)))
# })
# 
# gw_nonnest_clean <- do.call(what = rbind, args = gw_nonnest_list) %>% 
#   left_join(st_drop_geometry(gw_clean[, c("id", "name")]))
# 
# #Extract mean slope and elevation for each catchment
# dem_stack <- stack(c(paste0(getwd(), "/data/dhym_10m_crop.tif"),
#                      paste0(getwd(), "/data/dhym_10m_slope.tif")))
# 
# catchment_attr <- exact_extract(dem_stack, gw_nonnest_clean, "mean")
# names(catchment_attr) <- c("mean_elev", "mean_slope")
# 
# st_write(bind_cols(gw_nonnest_clean, catchment_attr) , paste0(getwd(), "/data/gw_nonnest_clean.sqlite"), delete_dsn=TRUE)
