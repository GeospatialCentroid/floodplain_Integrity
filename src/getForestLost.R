
# trying to use the purrr map function. We want to test 
getForestLost <- function(floodPlain, rasterList, multiple){
  # test for intersection between features 
  f1 <- floodPlain %>%
    st_transform(crs = terra::crs(rasterList[[1]]))
  bbox <- st_bbox(f1)
  
  if(isTRUE(multiple)){
    r1 <- NA 
    for(i in seq_along(rasterList)){
      r2 <- terra::ext(rasterList[i][[1]])
      # condition for testing overlap between features 
      r3 <- rasterList[i][[1]] 
      t <- NA
      try(t <- crop(r3,f1))
      if(class(t) == "SpatRaster"){
        if(class(r1) != "SpatRaster"){
          r1 <- t
        }else{
          r1 <- terra::merge(r1,t)
        }
      }
    }
    r1 <- r1 %>% terra::mask(vect(f1))
    # all values  
    v1 <- values(r1)
    vals <- v1[!is.na(v1)]
    total <- length(vals)
    # assuming negitive implies forest loss
    decrease <- length(vals[vals > 0])
    # calculate percent 
    if(isTRUE(total == 0)){
      value <- 0
    }else{
      value <- (decrease/total)*100
      
    }
    }else{
    for(i in seq_along(rasterList)){
      r2 <- terra::ext(rasterList[i][[1]])
      # condition for testing overlap between features 
      if( r2[1] < bbox[1] & r2[2] > bbox[3] & r2[3] < bbox[2] & r2[4] > bbox[4]){
        r1 <- rasterList[i][[1]] 
        print(i)
        # percentage of cells that reported forest lost 
        fp2 <- vect(f1)
        # sort to extent of the feature of interest 
        r3 <- r1 %>%
          terra::crop(fp2)%>%
          terra::mask(fp2)
        # all values  
        v1 <- values(r3)
        vals <- v1[!is.na(v1)]
        total <- length(vals)
        # assuming negitive implies forest loss
        decrease <- length(vals[vals > 0])
        # calculate percent 
        if(isTRUE(total == 0)){
          value <- 0
        }else{
          value <- (decrease/total)*100
          
        }
        break
      }else{
        # this should capture all features that overlap between boundaries. These will have to be 
        # calculated elsewhere 
        value <- NA 
      }
    }
  }
  
  return(value)
}