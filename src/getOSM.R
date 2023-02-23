

getOSM <- function(floodPlain, buildings, railroad, roads){
  # for each state 
  abbrev <- c("AL","AZ","AR","CA","CO","CT","DE","FL","GA","ID","IL","IN","IA"
              ,"KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV"
              ,"NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC"
              ,"SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")
  for(i in abbrev){
    features <- floodPlain[grep(pattern =i, x = floodPlain$postal1), ]
    # grab state 
    st2 <- i 
    # 
    building <- st_read(paste0("National Data Downloads/SP 22/Open Street Map/US Buildings/", st2,"_buildings.shp"))%>%
      st_transform(crs = st_crs(features))
    roads <- st_read(paste0("National Data Downloads/SP 22/Open Street Map/US Roads/", st2,"_roads.shp"))%>%
      st_transform(crs = st_crs(features))
    rails <- st_read(paste0("National Data Downloads/SP 22/Open Street Map/US Railroads/", st2,"_rail.shp"))%>%
      st_transform(crs = st_crs(features))
    
    features$buildingArea <- NA
    features$roadLength <- NA
    features$railLength <- NA
    
    # iterate over features 
    for(j in seq_along(features$FIRST_huc1)){
      # select features of interest 
      f1 <- features[j, ]
      # crop each element 
      b1 <- sf::st_crop(building, st_bbox(f1))
      road1 <- sf::st_crop(roads, st_bbox(f1))
      rails1 <- sf::st_crop(rails, st_bbox(f1))
      print(paste0(j, ": of ", nrow(features), " for ", st2))
      # test for area of buildings 
      if(nrow(b1)>0){
        b2 <- st_intersection(x = f1, y = b1)
        # test for intersection 
        if(length(b2[[1]]) > 0 ){
          val <- st_area(b1[b2[[1]], ]) %>% sum() 
          # save to dataframe and convert to kmsq 
          features$buildingArea[j] <- val*0.000001
        }else{
          features$buildingArea[j] <- 0
        }
      }else{
        features$buildingArea[j] <- 0
      }
      # test for area of roads 
      if(nrow(road1)>0){
        r2 <- st_intersection(x = f1, y = road1)
        # test for intersection 
        if(nrow(r2) > 0 ){
          val <- st_length(r2) %>% sum() 
          # save to dataframe and convert to kmsq 
          features$roadLength[j] <- val*0.001
        }else{
          features$roadLength[j] <- 0
        }
      }else{
        features$roadLength[j] <- 0
      }
      # test for length of railroads  
      if(nrow(rails1)>0){
        rl2 <- st_intersection(x = f1, y = road1)
        # test for intersection 
        if(nrow(rl2) > 0 ){
          val <- st_length(rl2) %>% sum() 
          # save to dataframe and convert to kmsq 
          features$railLength[j] <- val*0.001
        }else{
          features$railLength[j] <- 0
        }
      }else{
        features$railLength[j] <- 0
      }
      
    }
    # export feature 
    f2 <- features %>% 
      rowwise() %>%
      mutate(roadAndRailLength = sum(railLength, roadLength))%>%
      dplyr::select("FIRST_huc1", "buildingArea", "roadLength",   "railLength")%>% 
      st_drop_geometry()
    
    write_csv(x = f2, file = paste0("outputs/osm/",st2,".csv"))
  }
}