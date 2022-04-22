
#' getNLCD_lc
#'
#' @param polygons : flood plain area feature as an sf object 
#' @param landcover : nlcd as a terra rast object 
#' @param outputFolder : location where intermediate steps will be saved 
#' @param startIndex : where in the feature generation should the process start. Implimented because getting a full run is unlikley. 
#'

getNLCD_lc <- function(polygons,landcover, outputFolder, startIndex){
  # https://www.mrlc.gov/data/legends/national-land-cover-database-2019-nlcd2019-legend
  l1 <- c(22:24,81:82) # development low, medium, high intensity and pasture/hay or cultivated crops 
  
  # construct dataframe to hold all records 
  df <- data.frame(matrix(nrow = nrow(polygons), ncol = 3 + length(l1)))
  names(df) <- c("FIRST_huc1", l1, "total", "area")
  df$area <- polygons$FIRST_area
  
  time <- Sys.time()
  # run through each polygon 
  for(i in startIndex:length(polygons$FIRST_huc1)){
    # select feature
    t1 <- vect(polygons[i,])
    # assgin Id 
    df$FIRST_huc1[i] <- t1$FIRST_huc1  #again change when we have data
    # crop and mask image 
    r2 <- landcover %>% terra::crop(t1)%>%terra::mask(t1)
    
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
    # counter for progress
    if(i %% 10 == 0){
      print(paste0(i ," out of ", nrow(df)))
    }
    if(i %% 100 ==0){
      print(paste0((Sys.time() - time), " writing file"))
      write_csv(x = df[(i-100):i, ],
                file = paste0(outputFolder, "/nlcd_lc_",i - 100, "_",i,".csv")
                )
    }
    if(i == 82511){
      print(paste0((Sys.time() - time), " writing file"))
      write_csv(x = df[(i-11):i, ],
                file = paste0(outputFolder, "/nlcd_lc",i - 11, "_",i,".csv")
      )
    }
  }
}
