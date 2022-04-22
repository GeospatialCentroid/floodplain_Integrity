#' Combine shapefiles into one features
#'
#' @param path : location of store shapefiles 
#' @param output : full path with output file name 
#'

mergeFiles <- function(path, output, overwrite = FALSE){
  if(file.exists(output) & overwrite == FALSE){
    return("The files allready exists, please set overwrite==TRUE to rerender the content. ")
  }else{
    # loop over features 
    ### this would be a great place to impliment purrr::map
    s1 <- st_read(f1[1])
    for(i in 2:length(f1)){
      print(i)
      s1a <- st_read(f1[i])
      s1 <- rbind(s1, s1a)
    }
    
    st_write(s1, output)
  }
}