


getLeveeArea <- function(floodPlain, leeves){
  
  # calculate the area of leeve features 
  
  ## this might be slow... but it should 
  crop <- st_crop(x = leeves_all, y = floodPlain)
  if(nrow(crop) >0){# some test to see if any features are in th ecrop area 
    # generate the overlapping areas between the features 
    intersection <- st_intersection(crop, floodPlain) 
    # calculate area 
    if(nrow(intersection) >0 ){
      area <- st_area(intersection)*0.000001
    }else{
      area <- 0
    }
  }else{
    area <- 0
  }
  return(area)
}