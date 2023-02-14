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

#Threshold stream network
taudem_threshold <- paste0(mpi_settings, taudem_path, "threshold",
                           " -src ", "rawdata/dhym_20m_src.tif",
                           " -ssa ", "rawdata/dhym_20m_ad8.tif",
                           " -thresh 1250")
system(taudem_threshold)

#Snap q_points to nearest src raster cell before watershed delineation
src <- rast("rawdata/dhym_20m_src.tif")
src[src == 0] <- NA

src_df <- as.data.frame(src, na.rm=TRUE, xy=TRUE) |> 
  rownames_to_column(var = "src_idx")

#Snap qpoints to nearest stream cell
q_points <- st_read("rawdata/q_points.sqlite")

src_vect <- st_as_sf(src_df, coords=c("x", "y"), crs=dk_epsg) 

nearest_src <- st_nearest_feature(q_points, src_vect)

q_point_snap <- bind_cols(st_drop_geometry(q_points), src_df[nearest_src, ])

q_point_snap |> 
  mutate(id = as.numeric(factor(src_idx))) |> 
  st_as_sf(coords=c("x", "y"), crs=dk_epsg) |> 
  st_write("rawdata/q_points_snap.sqlite", delete_dsn=TRUE)

# #Snap points to stream network along flow directions
# taudem_snap <- paste0(taudem_path, "moveoutletstostrm",
#                       " -p ", "/media/kenneth/d6c13395-8492-49ee-9c0f-6a165e34c95c1/co2_lowland_streams/data/dhym_10m_breach_p.tif",
#                       " -src ", "rawdata/dhym_10m_breach_src.tif",
#                       " -md 100",
#                       " -o ", "rawdata/DK_mh_2020_100m_QPoints.shp",
#                       " -om ", "rawdata/qpoints_snap.shp")
# system(taudem_snap)
#
# stream_sites_snap |> 
#   filter(Dist_moved >= 0) |> 
#   mapview::mapview()

#Delineate watershed draining to stream outlets
taudem_gage <- paste0(mpi_settings, taudem_path, "gagewatershed",
                      " -p ", "rawdata/dhym_20m_p.tif",
                      " -o ", "rawdata/q_points_snap.sqlite",
                      " -gw ", "rawdata/dhym_watersheds.tif",
                      " -id ", "rawdata/dhym_watersheds.txt")
system(taudem_gage)

# #Raster to polygon
# polygonize <- paste0("pkpolygonize ",
#                      " -i ", "rawdata/dhym_watersheds.tif",
#                      " -m ", "rawdata/dhym_watersheds.tif",
#                      " -o ", "rawdata/dhym_watersheds.sqlite")
# system(polygonize)

# #Clean polygons
# gw_clean <- st_read("rawdata/dhym_watersheds.sqlite") %>% 
#   st_make_valid() %>% 
#   group_by(dn) %>% 
#   summarise() %>% 
#   st_cast("MULTIPOLYGON") %>% 
#   st_remove_holes() %>% 
#   rename(id = dn)
# 
# st_write(gw_clean, "rawdata/dhym_watersheds_clean.sqlite")

watersheds <- rast("rawdata/dhym_watersheds.tif")

watersheds_vec <- as.polygons(watersheds) |> 
  st_as_sf() |> 
  rename(id = dhym_watersheds) |> 
  st_cast("MULTIPOLYGON")

#Resolve watershed connectivity
conn <- read.table("rawdata/dhym_watersheds.txt", header = TRUE)

upstream_watersheds <- function(id){
  
  all_ids <- c(id)
  current_ids <- c(id)
  upstream_exist <- TRUE
  
  while(upstream_exist){
    
    upstream_ids <- conn[conn$iddown %in% current_ids, ]$id
    
    if(length(upstream_ids) == 0){
      upstream_exist <- FALSE
    }else{
      all_ids <- c(all_ids, upstream_ids)
      current_ids <- upstream_ids
    }
    
  }
  
  return(all_ids)
  
}

conn_list <- lapply(watersheds_vec$id, upstream_watersheds)

union_list <- lapply(conn_list, function(x){
  union_vec <- watersheds_vec |>
    filter(id %in% x) |> 
    st_union() |> 
    st_cast("MULTIPOLYGON")
  
  return(st_sf(id = x[1], geometry=union_vec))
})

watershed_union <- union_list |> 
  rbindlist() |> 
  st_as_sf()

st_write(watershed_union, "rawdata/q_points_watersheds.sqlite")

# #Use mapshaper cmd line functions to simplify polygons
# simplify_cmd <- paste("mapshaper-xl 16gb", "rawdata/q_points_watersheds.shp", "-simplify", "10%", "keep-shapes", "-o", "rawdata/q_points_watersheds_simple.shp")
# system(simplify_cmd)
