###
# generate summary statistics for the results 
# 20220921
# carverd@colostate.edu 
### 

# products: total floodplain area and percentage of total area that are less than specified IFI values.
# For instance, the floodplain area and percentage that have IFI values less than 0.5, less than 0.6, less than 0.7, etc. 

pacman::p_load("dplyr","tidyr", "sf", "tmap", "future","future.apply", "microbenchmark")

# read in data 
d1 <- sf::st_read("data/CONUS_IFI_/CONUS_IFI_.shp") %>% st_drop_geometry()

# function for generating summary data 
summarizeData <- function(index, data){
  allArea <- sum(data$FP_Areakm2)
  allFlood <- nrow(data)
  
  df <- data %>% 
    dplyr::filter(IFI_geomea <= index)%>%
    dplyr::summarise(totalArea = sum(FP_Areakm2),
                     totalFloodPlains = n())%>% 
    dplyr::mutate(percentFloodPlains = (totalFloodPlains/allFlood)*100,
                  percentArea = (totalArea/allArea)*100)
  return(df)
}
# sequence for values 
seq1 <- seq(from = 0.05, to  = 1, by =0.05)


# sequential 
df1 <- lapply(seq1, FUN = summarizeData, data = d1)%>%
  bind_rows()

df2 <- df1 %>%
  dplyr::mutate(IFI_index = seq1)%>%
  dplyr::select(IFI_index, percentArea, totalArea, percentFloodPlains, totalFloodPlains)
# export
write.csv(x = df2, file = paste0("outputs/summaryStats",Sys.Date(),".csv"))

# distributive 
plan(multicore, workers = 6)
y <- future_lapply(X = seq1, FUN = summarizeData, data = d1)%>%bind_rows()


# test between the two 
microbenchmark(
  #  this is a lot faster 
  lapply(seq1, FUN = summarizeData, data = d1)%>%
    bind_rows(),
  # 
  future_lapply(X = seq1, FUN = summarizeData, data = d1)%>%bind_rows(),
  times = 1
)
