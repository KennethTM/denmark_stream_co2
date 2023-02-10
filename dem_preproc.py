import richdem as rd

#Breach depression in DEM and resolve flats
dem_path = "rawdata/dhym_10m_crop.tif"

dem = rd.LoadGDAL(dem_path)

rd.BreachDepressions(dem, in_place=True)

rd.ResolveFlats(dem, in_place=True)

rd.SaveGDAL(dem_path.split(".")[0]+"_breach.tif", dem)
