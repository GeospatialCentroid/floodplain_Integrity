###
# Generate summary datasets for areas of overlap between the Konrad and IFI datsets
# 20230223
# carverd@colostate.edu
###

pacman::p_load("terra", "dplyr", "sf", "tmap", "raster")

# read in data  -----------------------------------------------------------
templateCRS <- raster("data/Konrad Ecological Functions/ecological_functions/fn1/w001000.adf")
# EPSG:6339
## IFI flood plains 
ifiFloodPlain <- sf::read_sf("data/CONUS_IFI_/CONUS_IFI_.shp")%>%
  st_transform(crs = st_crs(templateCRS))

ifi_fpCrop <- ifiFloodPlain %>% sf::st_crop(templateCRS) 
sf::st_write(obj = ifi_fpCrop, dsn = "data/preppedValidataionData/ifi_fpCrop.geojson", delete_dsn = TRUE)

## IFI results
ifiResults <- sf::read_sf("data/ifi/FP_stats_huc12.shp")%>%
  sf::st_transform(st_crs(ifiFloodPlain))

ifi_rCrop <- ifiResults %>% sf::st_crop(ext(templateCRS))
sf::st_write(obj = ifi_rCrop, dsn = "data/preppedValidataionData/ifiResults.geojson", delete_dsn = TRUE)



## Konrad FN1 objects - rasters
### list all features
kfiles <- list.files(path = "data/Konrad Ecological Functions",
                     full.names = TRUE,
                     recursive = TRUE)
kImages <- kfiles[grepl(pattern = "w001000.adf", x = kfiles)]

### read in object of interest for cropping 

#fn1
library(tictoc)
# ESPG number pulled from the st_crs(ifiFloodPlain)
k_fn1<-raster::raster(kImages[grepl(pattern = "fn1", x = kImages)])
writeRaster(k_fn1, filename = "data/preppedValidataionData/fn1.tif",overwrite = TRUE )

#fn2
k_fn2<-raster::raster(kImages[grepl(pattern = "fn2", x = kImages)])
writeRaster(k_fn2, filename = "data/preppedValidataionData/fn2.tif",overwrite = TRUE )

#fn3
k_fn3<-raster::raster(kImages[grepl(pattern = "fn3", x = kImages)])
writeRaster(k_fn1, filename = "data/preppedValidataionData/fn3.tif",overwrite = TRUE )

#fn4
k_fn4<-raster::raster(kImages[grepl(pattern = "fn4", x = kImages)])
writeRaster(k_fn1, filename = "data/preppedValidataionData/fn4.tif",overwrite = TRUE)



