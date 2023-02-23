


getGW_Wells <- function(polygons, wells, outputFolder, startIndex){
  
  # construct dataframe to hold all records 
  df <- data.frame(matrix(nrow = nrow(polygons), ncol = 3))
  names(df) <- c("FIRST_huc1", "numberWells", "area")
  df$area <- polygons$FIRST_area
  
  
  # reproject wells 
  wells <- wells %>%
    st_transform(crs =  st_crs(floodAreas))
  ### mutate was occurring before transformation Split out to dely 
  w1 <- wells %>%
    # we want lat lon values as rows in df to filter on. 
      mutate(lon = unlist(map(wells$geometry,1)),
             lat = unlist(map(wells$geometry,2)))
  
  # rather then spatial operation use the boundary box to filter wells 
  # than test for intersection 
  time <- Sys.time()
  for(i in startIndex:nrow(polygons)){
    ## generate count of well per floodplain area 
    # subset flood area 
    t1 <- polygons[i, ]
    df$FIRST_huc1[i] <- t1$FIRST_huc1

    # get square area containing flood plain for first filter
    bb <- sf::st_bbox(t1)
    # filter wells based on bbox 
    w2 <- w1 %>%
      dplyr::filter(lon >= bb[1], lon <= bb[3], lat >= bb[2], lat <= bb[4])
    # test for presences on tabular filter 
    if(nrow(w2) == 0){
      df$numberWells[i] <- 0 
    }else{
      # test for intersection between well and floodplain 
      inter <- st_intersects(x = t1, y = w2) %>% unlist()
      if(length(inter)== 0){
        # Not intersecting 
        df$numberWells[i] <- 0 
      }else{
        # number of intersecting wells 
        df$numberWells[i] <- length(inter) 
      }
    }
    # counter for progress
    if(i %% 100 == 0){
      print(paste0(i ," out of ", nrow(df)))
    }
    # write every 1000 iterations 
    if(i %% 1000 ==0){
      print(paste0((Sys.time() - time), " writing file"))
      write_csv(x = df[(i-1000):i, ],
                file = paste0(outputFolder, "/wells_",i - 1000, "_",i,".csv")
      )
    }
    # write every 1000 iterations 
    if(i == nrow(df)){
      print(paste0((Sys.time() - time), " writing file"))
      write_csv(x = df[(i-nrow(df)):i, ],
                file = paste0(outputFolder, "/wells_",i - nrow(df), "_",i,".csv")
      )
    }
  }
}
