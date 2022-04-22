

getOSM_r_r <- function(floodPlain, stateAbbrev,outputFolder){
  # for each state 
  abbrev <- stateAbbrev 
  
  for(i in abbrev){
    features <- floodPlain[grep(pattern =i, x = floodPlain$postal1), ]
    # grab state 
    st2 <- i 
    # 
    roads <- st_read(paste0("National Data Downloads/SP 22/Open Street Map/US Roads/", st2,"_roads.shp"))%>%
      st_transform(crs = st_crs(features))
    rails <- st_read(paste0("National Data Downloads/SP 22/Open Street Map/US Railroads/", st2,"_rail.shp"))%>%
      st_transform(crs = st_crs(features))
    
    
    # create a dataframe for outputs 
    # construct dataframe to hold all records 
    df <- data.frame(matrix(nrow = nrow(features), ncol = 5))
    names(df) <- c("FIRST_huc1", "roadLength", "railLength", "totalLength", "area")
    df$area <- features$FIRST_area

    time <- Sys.time()
    # iterate over features 
    for(j in seq_along(features$FIRST_huc1)){
      # select features of interest 
      f1 <- features[j, ]
      
      df$FIRST_huc1[j] <- f1$FIRST_huc1
      
      # crop each element 
      road1 <- sf::st_crop(roads, st_bbox(f1))
      rails1 <- sf::st_crop(rails, st_bbox(f1))
      print(paste0(j, ": of ", nrow(features), " for ", st2))
    
      # test for area of roads 
      if(nrow(road1)>0){
        r2 <- st_intersection(x = road1, y = f1)
        # test for intersection 
        if(nrow(r2) > 0 ){
          val <- st_length(r2) %>% sum() 
          # save to dataframe and convert to kmsq 
          df$roadLength[j] <- val*0.001
        }else{
          df$roadLength[j] <- 0
        }
      }else{
        df$roadLength[j] <- 0
      }
      # test for length of railroads  
      if(nrow(rails1)>0){
        rl2 <- st_intersection(x = rails1, y = f1)
        # test for intersection 
        if(nrow(rl2) > 0 ){
          val <- st_length(rl2) %>% sum() 
          # save to dataframe and convert to kmsq 
          df$railLength[j] <- val*0.001
        }else{
          df$railLength[j] <- 0
        }
      }else{
        df$railLength[j] <- 0
      }
      df$totalLength[j] <- sum(df$railLength[j],df$roadLength[j])
      # counter for progress
      if(j %% 10 == 0){
        #print(paste0(i ," out of ", nrow(df)))
      }
      if(j %% 100 ==0){
        print(paste0((Sys.time() - time), " writing file"))
        write_csv(x = df[(j-100):j, ],
                  file = paste0(outputFolder, "/",i,"_osm_r_r_",j - 100, "_",j,".csv")
        )
      }
      if(j == nrow(df)){
        print(paste0((Sys.time() - time), " writing file"))
        write_csv(x = df[(j- nrow(df)):j, ], # this will create some overlap but we can remove at compile step
                  file = paste0(outputFolder, "/",i,"_osm_r_r_",j - nrow(df), "_",j,".csv"))
        
      }
    }

  }
}