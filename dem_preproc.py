import richdem as rd
import subprocess
import os

#Breach depression in DEM and resolve flats
dem_path = "rawdata/dhym_20m.tif"

dem = rd.LoadGDAL(dem_path)

rd.BreachDepressions(dem, in_place=True)

rd.ResolveFlats(dem, in_place=True)

rd.SaveGDAL("rawdata/dhym_breach_flats_raw.tif", dem)

subprocess.run(["gdal_translate", "-co", "COMPRESS=LZW", "rawdata/dhym_breach_flats_raw.tif", "rawdata/dhym_breach_flats.tif"])

os.remove("rawdata/dhym_breach_flats_raw.tif")
