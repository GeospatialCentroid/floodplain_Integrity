
#' getLandfire
#'
#' @param polygons : flood plain area feature as an sf object 
#' @param EVT : landfire existing vegetation type as a terra rast object 
#' @param outputFolder : location where intermediate steps will be saved 
#' @param startIndex : where in the feature generation should the process start. Implemented because getting a full run is unlikely. 
#'
#' @return dataframe of percent area for landfire existing vegetation type,EVT, Groups 701-709,731 classes for all flood plain areas. 
#'

getlandfire <- function(polygons,EVT,outputFolder, startIndex){
  # https://landfire.gov/evt.php
  l1 <- c(9327,9301,9302,9307,9308,9309,9310,9311,9316,9317,9318,9319,9320,9321
          ,9323,9324,9325,9326,9328,9329,9332,9336,9337,9810,9811,9816,9817,9823
          ,9825,9826,9827,9828,9829,9312,9322) # which represent various types of invasive, non-agricultural plant species 
  
  # construct dataframe to hold all records 
  df <- data.frame(matrix(nrow = nrow(polygons), ncol = 3 + length(l1)))
  names(df) <- c("FIRST_huc1", l1, "total", "area")
  df$area <- polygons$FIRST_area
  
  time <- Sys.time()
  # run through each polygon 
  for(i in startIndex:length(polygons$FIRST_huc1)){
    
    try({
    # select feature
    t1 <- vect(polygons[i,])
    # assgin Id 
    df$FIRST_huc1[i] <- t1$FIRST_huc1  #again change when we have data
    # crop and mask image 
    r2 <- EVT %>% terra::crop(t1)%>%terra::mask(t1)
    
    # determine values 
    vals <- values(r2)
    #drop NA and 0 
    vals[vals == 0] <- NA
    vals <- vals[!is.na(vals)]
    # total number of land based cells 
    total <- length(vals)
    df[i,"total"] <- total
    # unique land cover class in area 
    uniqueVals <- sort(unique(vals))
    for(j in seq_along(l1)){
      code <- as.numeric(l1[j])
      # test if the specific land cover class is present in subset area
      if(code %in% uniqueVals){
        index <- code
        val2 <- vals[vals == index ]
        sum1 <- length(val2)
        percentCover <- (sum1/total)*100
        df[i,as.character(index)] <- percentCover
      }
    }
    })
    # counter for progress
    if(i %% 10 == 0){
      print(paste0(i ," out of ", nrow(df)))
    }
    if(i %% 100 ==0){
      print(paste0((Sys.time() - time), " writing file"))
      write_csv(x = df[(i-100):i, ],
                file = paste0(outputFolder, "/landfire_",i - 100, "_",i,".csv")
      )
    }
    if(i == 82511){
      print(paste0((Sys.time() - time), " writing file"))
      write_csv(x = df[(i-11):i, ],
                file = paste0(outputFolder, "/landfire_",i - 11, "_",i,".csv")
      )
    }
    
    
    }
  return(df)
}
