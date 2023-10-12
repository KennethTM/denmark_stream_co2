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
                     " -ad8 ", "data/dem/dhymm_ad8.tif",
                     " -nc")
system(taudem_acc)

#Threshold stream network
taudem_threshold <- paste0(mpi_settings, taudem_path, "threshold",
                           " -src ", "data/dem/dhym_src.tif",
                           " -ssa ", "data/dem/dhymm_ad8.tif",
                           " -thresh 1000")
system(taudem_threshold)

wbt_jenson_snap_pour_points(pour_pts = "data/dk_model/q_points.shp",
                            streams = "data/dem/dhym_src.tif",
                            output = "data/dk_model/q_points_snap.shp",
                            snap_dist = 1000)

#Delineate watershed draining to stream outlets
taudem_gage <- paste0(mpi_settings, taudem_path, "gagewatershed",
                      " -p ", "data/dem/dhym_p.tif",
                      " -o ", "data/dk_model/q_points_snap.shp",
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

st_write(watershed_union, "data/dk_model/q_points_watersheds.sqlite")

# #Use mapshaper cmd line functions to simplify polygons
# simplify_cmd <- paste("mapshaper-xl 16gb", "rawdata/q_points_watersheds.shp", "-simplify", "10%", "keep-shapes", "-o", "rawdata/q_points_watersheds_simple.shp")
# system(simplify_cmd)
