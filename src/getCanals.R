


getCanals <- function(floodPlain, canals){
  # input dataset has already been joined to the flood plains 
  # need to reproject and recalculate lengths in measured units 
  df <- canals %>%
    dplyr::select(FIRST_huc1)%>%
    sf::st_transform(crs = st_crs(floodPlain))%>%
    dplyr::mutate(
      # length is in meters so translate to km 
      length = as.numeric(sf::st_length(canals)*0.001)
    )%>%
    sf::st_drop_geometry()%>%
    group_by(FIRST_huc1) %>%
    summarise(totalLength = sum(length), totalDitches = n())
  # join this floodplain featues and assign true zero values 
  fp2 <- floodPlain %>%
    st_drop_geometry()%>%
    dplyr::left_join(df)%>%
    dplyr::mutate(
      totalLength = 
        dplyr::case_when(
          is.na(totalLength) ~ 0,
          TRUE ~ totalLength
        )
    )%>%
    dplyr::select(FIRST_huc1, totalLength, totalDitches)
  
  return(fp2)

}