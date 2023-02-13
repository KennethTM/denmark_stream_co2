source("libs_and_funcs.R")

#https://hydrology.usu.edu/taudem/taudem5/TauDEM53CommandLineGuide.pdf

#Flowdirs
taudem_flowdir <- paste0(mpi_settings, taudem_path, "d8flowdir ",
                         " -p ", "rawdata/dhym_20m_p.tif",
                         " -sd8 ", "rawdata/dhym_20m_sd8.tif",
                         " -fel ", "rawdata/dhym_breach_flats.tif")
system(taudem_flowdir)

#Flow accumulation
taudem_acc <- paste0(mpi_settings, taudem_path, "aread8",
                     " -p ", "rawdata/dhym_20m_p.tif",
                     " -ad8 ", "rawdata/dhym_20m_ad8.tif",
                     " -nc")
system(taudem_acc)

####

#Threshold stream network
taudem_threshold <- paste0(mpi_settings, taudem_path, "threshold",
                           " -src ", "rawdata/dhym_20m_src.tif",
                           " -ssa ", "dhym_20m_ad8.tif",
                           " -thresh 1000")
system(taudem_threshold)

#Snap q_points to nearest src raster cell before watershed delineation
src <- rast("rawdata/dhym_20m_src.tif")
src[src == 0] <- NA
src_df <- as.data.frame(src, na.rm=TRUE, xy=TRUE)

#Snap qpoints to nearest stream cell
q_points <- st_read("rawdata/q_points_id.sqlite")

src_vect <- st_as_sf(src_df, coords=c("x", "y"), crs=dk_epsg)

nearest_src <- st_nearest_feature(q_points, src_vect)

q_point_snap <- bind_cols(st_drop_geometry(q_points), src_df[nearest_src, ])

q_point_snap |> 
  st_as_sf(coords=c("x", "y"), crs=dk_epsg) |> 
  st_write("rawdata/qpoints_snap.shp")

# #Snap points to stream network along flow directions
# taudem_snap <- paste0(taudem_path, "moveoutletstostrm",
#                       " -p ", "/media/kenneth/d6c13395-8492-49ee-9c0f-6a165e34c95c1/co2_lowland_streams/data/dhym_10m_breach_p.tif",
#                       " -src ", "rawdata/dhym_10m_breach_src.tif",
#                       " -md 100",
#                       " -o ", "rawdata/DK_mh_2020_100m_QPoints.shp",
#                       " -om ", "rawdata/qpoints_snap.shp")
# system(taudem_snap)

stream_sites_snap |> 
  filter(Dist_moved >= 0) |> 
  mapview::mapview()

#Delineate watershed draining to stream outlets
taudem_gage <- paste0(mpi_settings, taudem_path, "gagewatershed",
                      " -p ", "rawdata/dhym_20m_p.tif",
                      " -o ", "rawdata/qpoints_snap.shp",
                      " -gw ", "rawdata/dhym_watersheds.tif",
                      " -id ", "rawdata/dhym_watersheds.txt")
system(taudem_gage)

#as.polygons

#Raster to polygon
polygonize <- paste0("pkpolygonize ",
                     " -i ", "rawdata/dhym_watersheds.tif",
                     " -m ", "rawdata/dhym_watersheds.tif",
                     " -o ", "rawdata/dhym_watersheds.sqlite")
system(polygonize)

#Clean polygons
gw_clean <- st_read("rawdata/dhym_watersheds.sqlite") %>% 
  st_make_valid() %>% 
  #group_by(dn) %>% 
  #summarise() %>% 
  #st_cast("MULTIPOLYGON") %>% 
  #st_remove_holes() %>% 
  rename(id = dn) #%>% 
  #st_join(stream_sites_snap) %>% 
  #mutate(nested_area = as.numeric(st_area(geometry)))

conn <- read.table("rawdata/dhym_watersheds.txt", header = TRUE)


upstream <- function(id){
  
  id_list <- c()
  upstream_exist <- TRUE
  
  while(upstream_exist){
    ids_up <- conn[conn$iddown == id, ]$id
    
    if(length(ids_up) == 0){
      upstream_exist <- FALSE
    }else{
      id_list <- c(id_list, ids_up)
    }
    
  }
  
  return(id_list)
  
}


test <- gw_clean[1:10, ]

idx_list <- list()

for(i in 1:nrow(test)){
  watershed_id <- test[i, ]$id
  
  next_id <- watershed_id
  id_list <- c()
  
  while(next_id != -1){
    next_id <- conn[conn$id == next_id, ]$iddown
    id_list <- c(id_list, next_id)
    
  }
  
  watershed_id_chr <- as.character(watershed_id)
  
  id_list[id_list == -1] <- watershed_id
  
  idx_list[[watershed_id_chr]] <- id_list
  
}

gw_unioned <- lapply(idx_list, function(idxs){
  gw_clean[gw_clean$id %in% idxs, ] |> 
    st_union()
})

test_2 <- st_set_geometry(test, do.call(rbind, gw_unioned) |> st_sfc(crs=dk_epsg))

#compare qpoints og catchments
