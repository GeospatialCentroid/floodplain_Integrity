#' Compile OSM data
#'
#' @param buildings: TRUE/FALSE for if you are compiling the building or road and rail layers  
#' @param path : folder location for all OSM outputs 
#'
#' @return : df of compiled and summaries input across all states 

compileOSM <- function(buildings, path){
  
  if(isTRUE(buildings)){
    loc <- paste0(path, "/buildings")
    }else{
      loc <- paste0(path, "/roadsRails")
  }
  files <- list.files(loc, pattern = ".csv", full.names = TRUE)
  # loop over state
  for(i in seq_along(abbrev)){
    #grab all state features 
    f2 <- files[grepl(pattern = abbrev[i],x = files, ignore.case = FALSE)]
    for(j in seq_along(f2)){
      f1 <- read_csv(f2[j]) %>%
        dplyr::mutate("FIRST_huc1" = as.character(FIRST_huc1))
      if(j == 1){
        df <- f1
      }else{
        df <- bind_rows(df, f1)
      }
    }
    # remove any duplicated features 
    lc2 <- df[!duplicated(df$FIRST_huc1), ]
    if(i == 1){
      df2 <- df
    }else{
      df2 <- bind_rows(df2,df)
    }
  }
  if(isTRUE(buildings)){
    df3 <- df2 %>%
      group_by(FIRST_huc1)%>%
      dplyr::summarise(
        buildingArea = sum(buildingArea),
        count = n())
    }else{
      df3 <- df2 %>%
        group_by(FIRST_huc1)%>%
        dplyr::summarise(
          roadLength = sum(roadLength),
          railLength = sum(railLength ),
          totalLength = sum(totalLength),
          count = n())
    }
  return(df3)
}