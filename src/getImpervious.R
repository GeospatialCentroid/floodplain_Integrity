
#' get impervious surface 
#' @description : generate the average percent impervious surface for all features in the floodplain
#' @param polygons : floodplain polygon features
#' @param impervious : NLCD 2019 impervious surface layer as a terra::rast
#' @param outputFolder : location for storing outputs
#' @param startIndex : where to start the itorative process from. numeric value betwee 1 and nrow(floodplain)

getNLCD_impervious <- function(polygons,impervious, outputFolder, startIndex){
  # https://www.mrlc.gov/data?f%5B0%5D=category%3Aurban%20imperviousness
  # raster values store % of area considered impervious. 
  # Percent imperviousness (average percent imperviousness values reported for each 30 m cell for all cells in the floodplain)
  
  # construct dataframe to hold all records 
  df <- data.frame(matrix(nrow = nrow(polygons), ncol = 3))
  names(df) <- c("FIRST_huc1", "aveImpervious", "area")
  df$area <- polygons$FIRST_area
  
  time <- Sys.time()
  # run through each polygon 
  for(i in startIndex:length(polygons$FIRST_huc1)){
    # select feature
    t1 <- vect(polygons[i,])
    # assign Id 
    df$FIRST_huc1[i] <- t1$FIRST_huc1  #again change when we have data
    # crop and mask image 
    try(
      # calculate the average impervious values across all features in floodplain
      r1 <- impervious %>% 
        terra::crop(t1)%>%
        terra::mask(t1),
    ) 
    if(class(r1)=="SpatRaster"){
      vals <- values(r1)
      vals[vals == 127] <- NA
      df$aveImpervious[i] <- vals %>% na.omit() %>% mean()
    }else{
      df$aveImpervious[i] <- NA
    }
    rm(r1)
    
    
    # counter for progress
    if(i %% 10 == 0){
      print(paste0(i ," out of ", nrow(df)))
    }
    if(i %% 100 ==0){
      print(paste0((Sys.time() - time), " writing file"))
      write_csv(x = df[(i-100):i, ],
                file = paste0(outputFolder, "/impervious_",i - 100, "_",i,".csv")
      )
    }
    if(i == 82511){
      print(paste0((Sys.time() - time), " writing file"))
      write_csv(x = df[(i-11):i, ],
                file = paste0(outputFolder, "/impervious_",i - 11, "_",i,".csv")
      )
    } 
  }
}
