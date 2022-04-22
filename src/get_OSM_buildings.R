

getOSM_buildings <- function(floodPlain, stateAbbrev,outputFolder){
  # for each state 
  abbrev <- stateAbbrev

    for(i in abbrev){
    features <- floodPlain[grep(pattern =i, x = floodPlain$postal1), ]
    # grab state 
    st2 <- i 
    # 
    building <- st_read(paste0("National Data Downloads/SP 22/Open Street Map/US Buildings/", st2,"_buildings.shp"))%>%
      st_transform(crs = st_crs(features))
    
    # create a dataframe for outputs 
    # construct dataframe to hold all records 
    df <- data.frame(matrix(nrow = nrow(features), ncol = 3))
    names(df) <- c("FIRST_huc1", "buildingArea", "area")
    df$area <- features$FIRST_area
 
    time <- Sys.time()
    # iterate over features 
    for(j in seq_along(features$FIRST_huc1)){
      # select features of interest 
      f1 <- features[j, ]
      df$FIRST_huc1[j] <- f1$FIRST_huc1
      # crop each element 
      b1 <- sf::st_crop(building, st_bbox(f1))
      
      print(paste0(j, ": of ", nrow(df), " for ", st2))
      # test for area of buildings 
      if(nrow(b1)>0){
        b2 <- st_intersection(x = b1, y = f1)
        # test for intersection 
        if(length(b2[[1]]) > 0 ){
          print(f1$FIRST_huc1)
          val <- st_area(b2) %>% sum() 
          # save to dataframe and convert to kmsq 
          df$buildingArea[j] <- val*0.000001
        }else{
          df$buildingArea[j] <- 0
        }
      }else{
        df$buildingArea[j] <- 0
      }
      # counter for progress
      if(j %% 10 == 0){
        print(paste0(i ," out of ", nrow(df)))
      }
      if(j %% 100 ==0){
        print(paste0((Sys.time() - time), " writing file"))
        write_csv(x = df[(j-100):j, ],
                  file = paste0(outputFolder, "/",i,"_osm_building_",j - 100, "_",j,".csv")
        )
      }
      if(j == nrow(df)){
        print(paste0((Sys.time() - time), " writing file"))
          write_csv(x = df[(j- nrow(df)):j, ], # this will create some overlap but we can remove at compile step
                    file = paste0(outputFolder, "/",i,"_osm_building_",j - nrow(df), "_",j,".csv"))

      }
    }
  }
}
