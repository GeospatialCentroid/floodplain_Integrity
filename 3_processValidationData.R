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
kImages <- kfiles[grepl(pattern = "w001000.adf", x = kfiles)]
### read in object of interest for cropping 

#fn1
k_fn1<-terra::rast(kImages[grepl(pattern = "fn1", x = kImages)])%>%
  terra::project(coreCRS)
writeRaster(k_fn1, filename = "data/preppedValidataionData/fn1.tif" )

#fn2
k_fn2<-terra::rast(kImages[grepl(pattern = "fn2", x = kImages)])%>%
  terra::project(coreCRS)
writeRaster(k_fn1, filename = "data/preppedValidataionData/fn2.tif" )

#fn3
k_fn3<-terra::rast(kImages[grepl(pattern = "fn3", x = kImages)])%>%
  terra::project(coreCRS)
writeRaster(k_fn1, filename = "data/preppedValidataionData/fn3.tif" )

#fn4
k_fn4<-terra::rast(kImages[grepl(pattern = "fn4", x = kImages)])%>%
  terra::project(coreCRS)
writeRaster(k_fn1, filename = "data/preppedValidataionData/fn4.tif" )


# comparison of flood plain areas  ----------------------------------------
## this will be a difficult thing to do

# process rasters per flood plain feature ---------------------------------

## crop other spatial features 
ifi_fpCrop <- ifiFloodPlain %>% sf::st_crop(ext(k_fn1))
sf::st_write(obj = ifi_fpCrop, dsn = "data/preppedValidataionData/ifi_fpCrop.geojson")
ifi_rCrop <- ifiResults %>% sf::st_crop(ext(k_fn1))
sf::st_write(obj = ifi_rCrop, dsn = "data/preppedValidataionData/ifiResults.geojson", delete_dsn = TRUE)



# generate storage data frame  --------------------------------------------
cellAreas <- data.frame(matrix(nrow = nrow(ifi_rCrop), ncol = 11))
names(cellAreas) <- c("huc12_id", "calculatedArea", "measuredArea", "fp_area", "nCells", "3","2","1","0","-1","-2")

# assign values from the ifi data
cellAreas$huc12_id <- ifi_rCrop$huc12
cellAreas$calculatedArea <- sf::st_area(ifi_rCrop)
cellAreas$measuredArea <- ifi_rCrop$IFI_geomea
cellAreas$fp_area <- ifi_rCrop$FP_Areakm2

# processing function to calculate input values   ----------------------------------------------------
for(i in seq_along(ifi_rCrop$huc12)){
  # grad the huc features 
  huc <- vect(ifi_rCrop[i,])
  # crop 
  tic()
  crop <- terra::crop(k_fn1, huc )%>%
    terra::mask(huc)
  toc()
  # pull out values 
  vals <- values(crop)[,1]
  cellAreas[i, "nCells"] <- length(vals)
  # gather all unique values 
  u_vals <- unique(vals)
  # condition to test for the presents of any unique measure 
  condition <- u_vals %in% c(3,2,1,0,-1,-2)
  # validate condition, are any of the values present. 
  if(TRUE %in% condition){
    print(i)
    cellAreas[i, "3"] <- length(grep(pattern = "3", x = vals))
    cellAreas[i, "2"] <- length(grep(pattern = "2", x = vals))
    cellAreas[i, "1"] <- length(grep(pattern = "1", x = vals))
    cellAreas[i, "0"] <- length(grep(pattern = "0", x = vals))
    cellAreas[i, "-1"] <- length(grep(pattern = "-1", x = vals))
    cellAreas[i, "-2"] <- length(grep(pattern = "-2", x = vals))
  }
}

write.csv(x = cellAreas, file = paste0("outputs/cellAreaClassification_",Sys.Date(),".csv"))
