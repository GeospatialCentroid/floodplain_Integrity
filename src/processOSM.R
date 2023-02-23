#' Combine shapefiles into one features
#'
#' @param path : location of store shapefiles 
#' @param output : full path with output file name 
#'

mergeFiles <- function(path, output){
 f1 <- list.files(path = path,  pattern = ".shp", full.names = TRUE) 
 # loop over features 
 ### this would be a great place to impliment purrr::map
 s1 <- st_read(f1[1])
 for(i in 2:length(f1)){
   print(i)
   s1a <- st_read(f1[i])
   s1 <- bind_rows(s1, s1a)
 }
 
 st_write(s1, output)
}