source("libs_and_funcs.R")

#Use whiteboxtools to preprocess DEM
wbt_fill_single_cell_pits(dem = "data/dem/dhym.tif",
                          output = "data/dem/dhym_single.tif")

wbt_breach_depressions_least_cost(dem = "data/dem/dhym_single.tif",
                                  output = "data/dem/dhym_breach.tif",
                                  dist = 10)

wbt_fill_depressions_wang_and_liu(dem = "data/dem/dhym_breach.tif",
                                  output = "data/dem/dhym_breach_fill.tif",
                                  fix_flats = TRUE)

#Use taudem to delineate watersheds
#https://hydrology.usu.edu/taudem/taudem5/TauDEM53CommandLineGuide.pdf

#Flowdirs
taudem_flowdir <- paste0(mpi_settings, taudem_path, "d8flowdir",
                         " -p ", "data/dem/dhym_p.tif",
                         " -sd8 ", "data/dem/dhym_sd8.tif",
                         " -fel ", "data/dem/dhym_breach_fill.tif")
system(taudem_flowdir)

#Flow accumulation
taudem_acc <- paste0(mpi_settings, taudem_path, "aread8",
                     " -p ", "data/dem/dhym_p.tif",
                     " -ad8 ", "data/dem/dhym_ad8.tif",
                     " -nc")
system(taudem_acc)

#Threshold stream network
taudem_threshold <- paste0(mpi_settings, taudem_path, "threshold",
                           " -src ", "data/dem/dhym_src.tif",
                           " -ssa ", "data/dem/dhym_ad8.tif",
                           " -thresh 1000")
system(taudem_threshold)

#Streamnet processing to get slope of stream links
taudem_streamnet <- paste0("mpiexec -n 2 ", taudem_path, "streamnet",
                           " -p ", "data/dem/dhym_p.tif",
                           " -src ", "data/dem/dhym_src.tif",
                           " -ord ", "data/dem/dhym_ord.tif",
                           " -ad8 ", "data/dem/dhym_ad8.tif",
                           " -fel ", "data/dem/dhym_breach_fill.tif",
                           " -tree ", "data/dem/dhym_tree.txt",
                           " -coord ", "data/dem/dhym_coord.txt",
                           " -net ", "data/dem/dhym_net.sqlite",
                           " -netlyr ", "dhym_net",
                           " -w ", "data/dem/dhym_w.tif")
system(taudem_streamnet)

#Rasterize streamnet slope attribute
src_rast <- rast(rast("data/dem/dhym_src.tif"))
template_rast <- rast(src_rast)
streamnet_vec <- vect("data/dem/dhym_net.sqlite")
rasterize(streamnet_vec, template_rast, filename="data/dem/dhym_net_slope.tif", 
          field="slope", overwrite=TRUE)

#Snap qpoints to virtual stream network
#TODO - increase to make sure all sites snap to src?? Filter by snap_dist later, 1000?
wbt_jenson_snap_pour_points(pour_pts = "data/dk_model/q_points.shp",
                            streams = "data/dem/dhym_src.tif",
                            output = "data/dk_model/q_points_snap_raw.shp",
                            snap_dist = 500)

#Add snapping info to qpoints
q_points_snap_raw <- st_read("data/dk_model/q_points_snap_raw.shp")

q_points_orig_coords <- data.frame(x = q_points_snap_raw$q_point_x, 
                                   y = q_points_snap_raw$q_point_y) |> 
  st_as_sf(crs=st_crs(q_points_snap_raw), coords=c("x", "y"))

q_points_snap_raw$snap_dist <- as.numeric(st_distance(q_points_snap_raw, q_points_orig_coords, by_element = TRUE))

#Check if stream site is snapped onto virtual stream network
#Add an id to relate q points to watersheds (some q points are snapped to the same virtual stream cell)
dhym_src <- rast("data/dem/dhym_src.tif")

site_dhym_src <- extract(dhym_src, vect(q_points_snap_raw), cells=TRUE)

q_points_snap <- q_points_snap_raw |> 
  mutate(on_virtual_stream = site_dhym_src$dhym_src,
         virtual_stream_id = site_dhym_src$cell) |> 
  group_by(virtual_stream_id) |> 
  mutate(id = cur_group_id())
  
st_write(q_points_snap, "data/dk_model/q_points_snap.sqlite", delete_dsn = TRUE)

#Delineate watershed draining to stream outlets
taudem_gage <- paste0(mpi_settings, taudem_path, "gagewatershed",
                      " -p ", "data/dem/dhym_p.tif",
                      " -o ", "data/dk_model/q_points_snap.sqlite",
                      " -gw ", "data/dem/dhym_watersheds.tif",
                      " -id ", "data/dem/dhym_watersheds.txt")
system(taudem_gage)

#Load nested watersheds
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

conn_list <- lapply(watersheds_vec$id, upstream_watersheds)

#Union watersheds
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

st_write(watershed_union, "data/dk_model/q_points_catchments.sqlite")

# #Use mapshaper cmd line functions to simplify polygons
# simplify_cmd <- paste("mapshaper-xl 16gb", "rawdata/q_points_watersheds.shp", "-simplify", "10%", "keep-shapes", "-o", "rawdata/q_points_watersheds_simple.shp")
# system(simplify_cmd)
