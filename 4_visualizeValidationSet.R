### 
# set of functions to help generate specific comparison results between resources
# 20230207
# carverd@colostate.edu 
###

pacman::p_load(sf,terra,dplyr)

# general method 
## determine the total number of huc12s with IFI but no Konrad score 
## total floodplain areas for both IFI and Konrad 
## total floodplain areas shared at each huc12 betweem the datasets
## calculate the total flood plain area define by (IFI, Konrad) within each huc12
## gather the IFI value for each huc 12 
## compute the percent area of summed values against the total area 



# read in datasets --------------------------------------------------------
## IFI data
# flood plain areas 
ifi_fp <- sf::st_read("data/preppedValidataionData/ifi_fpCrop.geojson")
### used for specific information about flood plain area and measures 
# huc12 results 
ifi_results <- sf::st_read("data/preppedValidataionData/ifiResults.geojson")
### used for testing intersection with the konrad data 

## Konrad data
k_fn1 <- rast("data/preppedValidataionData/fn1.tif" )
k_fn2 <- rast("data/preppedValidataionData/fn2.tif" )
k_fn3 <- rast("data/preppedValidataionData/fn3.tif" )
k_fn4 <- rast("data/preppedValidataionData/fn4.tif" )





# Flood reduction ---------------------------------------------------------
## IFI : Flood reduction
## konrad : FN1 --- sum(1,2,3)

# generate storage data frame  --------------------------------------------
cellAreas <- data.frame(matrix(nrow = nrow(ifi_results), ncol = 7))
names(cellAreas) <- c("huc12_id", "ifi_measure_Flood","ifi_fp_AreaKM2", "konrad_fp_area", "k_totalCells", "k_sum_1_2_3", "k_percentArea")

# assign values from the ifi data
cellAreas$huc12_id <- ifi_results$huc12_num
cellAreas$ifi_measure_Flood <- ifi_results$Floods
cellAreas$ifi_fp_AreaKM2 <- ifi_results$FP_Areakm2


# processing function to calculate input values   ----------------------------------------------------
for(i in seq_along(ifi_results$huc12)){
  print(i)
  # grab the huc ID, used as an index for the reset of the analysis 
  hucID <- ifi_results[i, "huc12"] %>% st_drop_geometry() %>% pull()
  # grab huc object and convert to a terra vect 
  huc <- ifi_results[i,] %>% vect()

  # crop the konrad data to area of the huc
  crop <- terra::crop(k_fn1, huc )%>%
    terra::mask(huc)
  

  # pull out all values 
  vals <- values(crop)[,1]
  
  # gather all unique values 
  u_vals <- unique(vals)
  # condition to test for the presents of any unique measure 
  condition <- u_vals %in% c(3,2,1,0,-1,-2)
  # validate condition, are any of the values present. 
  if(TRUE %in% condition){
    trueVals <- vals[!is.na(vals)]
    cellAreas$k_totalCells[i] <- length(trueVals)
    cellAreas$k_sum_1_2_3[i] <- length(trueVals[trueVals >= 1])
  }else{
    cellAreas$k_totalCells[i] <- 0 
    cellAreas$k_sum_1_2_3[i] <- NA
  }
  cellAreas$k_percentArea[i] <- (cellAreas$k_sum_1_2_3[i]/cellAreas$k_totalCells[i])*100
  
}

write.csv(x = cellAreas, file = paste0("outputs/cellAreaClassification_",Sys.Date(),".csv"))



# Sediment regulation -----------------------------------------------------
## IFI : Sediment regulation
## konrad fn2 --- sum(1,2,3,4)


# organic and solute regulation -------------------------------------------
## IFI : Organic & solute regulation
## konrad fn3 --- sum(>0)


# habitat provisioning ----------------------------------------------------
## IFI : Habitat provisioning
## konrad fn4 --- sum(1,2)

