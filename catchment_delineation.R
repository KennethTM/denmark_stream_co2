source("libs_and_funcs.R")
library(whitebox)

#Use whiteboxtools to preprocess DEM
# wbt_fill_single_cell_pits(dem="data/dem/dhym_20m.tif",
#                           output="data/dem/dhym_single.tif")

wbt_fill_single_cell_pits(dem="data/dem/dtm_10m.tif",
                          output="data/dem/dhym_single.tif")

wbt_breach_depressions_least_cost(dem = "data/dem/dhym_single.tif",
                                  output = "data/dem/dhym_breach.tif",
                                  dist=10,
                                  fill = FALSE)

wbt_fill_depressions(dem = "data/dem/dhym_breach.tif",
                     output = "data/dem/dhym_breach_fill.tif",
                     fix_flats = TRUE)

#https://hydrology.usu.edu/taudem/taudem5/TauDEM53CommandLineGuide.pdf

#Flowdirs
taudem_flowdir <- paste0(mpi_settings, taudem_path, "d8flowdir",
                         " -p ", "data/dem/dhym_20m_p.tif",
                         " -sd8 ", "data/dem/dhym_20m_sd8.tif",
                         " -fel ", "data/dem/dhym_breach_fill.tif")
system(taudem_flowdir)

#Flow accumulation
taudem_acc <- paste0(mpi_settings, taudem_path, "aread8",
                     " -p ", "data/dem/dhym_20m_p.tif",
                     " -ad8 ", "data/dem/dhym_20m_ad8.tif",
                     " -nc")
system(taudem_acc)

#Threshold stream network
taudem_threshold <- paste0(mpi_settings, taudem_path, "threshold",
                           " -src ", "data/dem/dhym_20m_src.tif",
                           " -ssa ", "data/dem/dhym_20m_ad8.tif",
                           " -thresh 250")
system(taudem_threshold)

# #Snap q_points to nearest src raster cell before watershed delineation
# src <- rast("data/dem/dhym_20m_src.tif")
# src[src == 0] <- NA
# 
# src_df <- as.data.frame(src, na.rm=TRUE, xy=TRUE) |> 
#   rownames_to_column(var = "src_idx")
# 
# #Snap qpoints to nearest stream cell
# q_points <- st_read("data/dem/q_points.sqlite")
# 
# src_vect <- st_as_sf(src_df, coords=c("x", "y"), crs=dk_epsg) 
# 
# nearest_src <- st_nearest_feature(q_points, src_vect)
# 
# q_point_snap <- bind_cols(st_drop_geometry(q_points), src_df[nearest_src, ])
# 
# q_point_snap |> 
#   mutate(id = as.numeric(factor(src_idx))) |> 
#   st_as_sf(coords=c("x", "y"), crs=dk_epsg) |> 
#   st_write("data/dem/q_points_snap.sqlite", delete_dsn=TRUE)

# #Snap points to stream network along flow directions
# taudem_snap <- paste0(taudem_path, "moveoutletstostrm",
#                       " -p ", "data/dem/dhym_20m_p.tif",
#                       " -src ", "rawdata/dhym_20m_src.tif",
#                       " -md 100",
#                       " -o ", "data/q_points.sqlite",
#                       " -om ", "rawdata/q_points_snap.shp")
# system(taudem_snap)

wbt_jenson_snap_pour_points(pour_pts = "data/dk_model/q_points.shp",
                            streams = "data/dem/dhym_20m_src.tif",
                            output = "data/dk_model/q_points_snap.shp",
                            snap_dist = 500)

#Delineate watershed draining to stream outlets
taudem_gage <- paste0(mpi_settings, taudem_path, "gagewatershed",
                      " -p ", "data/dem/dhym_20m_p.tif",
                      " -o ", "data/dk_model/q_points_snap.shp",
                      " -gw ", "data/dem/dhym_watersheds.tif",
                      " -id ", "data/dem/dhym_watersheds.txt")
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

watersheds <- rast("data/dem/dhym_watersheds.tif")

watersheds_vec <- as.polygons(watersheds) |> 
  st_as_sf() |> 
  rename(id = dhym_watersheds) |> 
  st_cast("MULTIPOLYGON")

#Resolve watershed connectivity
conn <- read.table("data/dem/dhym_watersheds.txt", header = TRUE)

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



# library(igraph)
# conn[conn == -1] <- NA
# x <- graph_from_data_frame(conn[,c(2, 1)])


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

st_write(watershed_union, "data/dem/q_points_watersheds.sqlite")

# #Use mapshaper cmd line functions to simplify polygons
# simplify_cmd <- paste("mapshaper-xl 16gb", "rawdata/q_points_watersheds.shp", "-simplify", "10%", "keep-shapes", "-o", "rawdata/q_points_watersheds_simple.shp")
# system(simplify_cmd)
