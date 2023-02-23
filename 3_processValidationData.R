###
# Generate summary datasets for areas of overlap between the Konrad and IFI datsets
# 20230223
# carverd@colostate.edu
###

pacman::p_load("terra", "dplyr", "sf", "tmap")

# read in data  -----------------------------------------------------------
## IFI flood plains 
ifiFloodPlain <- sf::read_sf("data/CONUS_IFI_/CONUS_IFI_.shp")
coreCRS <- crs(ifiFloodPlain)

## IFI results
ifiResults <- sf::read_sf("data/ifi/FP_stats_huc12.shp")%>%
  sf::st_transform(crs = coreCRS)

## Konrad FN1 objects - rasters
### list all features
kfiles <- list.files(path = "data/Konrad Ecological Functions",
                     full.names = TRUE,
                     recursive = TRUE)
### read in object of interest 
k_fn1 <- terra::rast(kfiles[7])%>%
  terra::project(coreCRS)


# comparison of flood plain areas  ----------------------------------------
## this will be a difficult thing to do

# process rasters per flood plain feature ---------------------------------

## crop other spatial features 
ifi_fpCrop <- ifiFloodPlain %>% sf::st_crop(y=k_fn1)
ifi_rCrop <- ifiResults %>% sf::st_crop(y=k_fn1)
### I think I want to work with the results feature 


# function to calculate summary statistic of raster values  ---------------
## total cells, number of cell in each category


# apply to all huc 12  ----------------------------------------------------
## apply across all feautres 
## compile and summarize results to percent of total area and percent coverages 
## of each category 





