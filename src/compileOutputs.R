#' Compile outsputs 
#' @description : grabs a series of CSV and combines them into a single file. 
#' @param filePath : folder location of csv files to compile 
#' @param fileName : name of the output file containing complied information
#'


compileOutputs <- function(filePath, fileName){
  files <- list.files(filePath, pattern = ".csv", full.names = TRUE)
  for(i in seq_along(files)){
    f1 <- read_csv(files[i],progress = FALSE) %>%
      dplyr::mutate("FIRST_huc1" = as.character(FIRST_huc1))
    if(i == 1){
      df <- f1
    }else{
      df <- bind_rows(df, f1)
    }
  }
  ## remove for dupicated features 
  lc2 <- df[!duplicated(df$FIRST_huc1), ]
  lc2 <- lc2[!is.na(lc2$FIRST_huc1), ]
  # write out feature. 
  write_csv(x = lc2, file = paste0(filePath,"/",fileName,".csv"))
}