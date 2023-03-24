# generalized function for percent area -----------------------------------
#' calculatePercentages
#'
#' @param df : hold content for the 
#' @param hucSF : ifi_results -- huc area
#' @param fpSF : ifi_fp -- flood plain feature 
#' @param K_rast : konrad raster layer 
#' @param K_index : vector of measures in the konrad values to included
#'
#' @return
#' 
calculatePercentages <- function(df, hucSF, fpSF, K_rast, K_index){
  for(i in seq_along(hucSF$huc12)){
    # grab the huc ID, used as an index for the reset of the analysis 
    hucID <- hucSF[i, "huc12"] %>% st_drop_geometry() %>% pull()
    # grab huc object and convert to a terra vect 
    huc <- hucSF[i,] %>% vect()
    # grab the flood plain feature
    ifi_fp <- fpSF[grepl(pattern = hucID, x = fpSF$HUC12), ]
    if(nrow(ifi_fp) == 1){
      # crop to FP
      crop_fp <- terra::crop(K_rast, ifi_fp)%>%
        terra::mask(ifi_fp)
      # test if any values are in the data
      val_crop <- values(crop_fp)[,1]
      u_vals_crop <- unique(val_crop)
      if(length(K_index)==1){
        condition_crop <- u_vals_crop[u_vals_crop > K_index]
      }else{
        condition_crop <- u_vals_crop %in% K_index
      }
      if(TRUE %in% condition_crop){
        df$cell_In_IFI_FP[i] <- TRUE
      }else{
        df$cell_In_IFI_FP[i] <- FALSE
      }
    }
    
    # crop the konrad data to area of the huc
    crop <- terra::crop(K_rast, huc )%>%
      terra::mask(huc)
    # pull out all values
    vals <- values(crop)[,1]
    # gather all unique values
    u_vals <- unique(vals)
    # condition to test for the presents of any unique measure
    if(length(K_index)==1){
      condition <- u_vals_crop[u_vals_crop > K_index]
    }else{
      condition <- u_vals_crop %in% K_index
    }
    # validate condition, are any of the values present.
    if(TRUE %in% condition){
      trueVals <- vals[!is.na(vals)]
      df$k_totalCells[i] <- length(trueVals)
      df$k_sum[i] <- length(trueVals[trueVals >= 1])
    }else{
      df$k_totalCells[i] <- 0
      df$k_sum[i] <- NA
    }
    print(paste0("end", i))
  }
  return(df)
}


# generalized function for generating figure  -----------------------------
#' getFigure
#'
#' @param data : filtered dataframe of comparison values 
#'
#' @return plotly scatter plot object 
getFigure <- function(data){
  p1 <- ggplot(data, aes(x=ifi_measure_Flood, y=k_percentArea)) +
    geom_abline(size = 1.5, intercept = 0, slope = 1, color = "#6F7378" ,
                linetype = 'dotted',)+
    annotate(geom = "text", x = 0.15, y = 0.2, label = "1:1 line",
             color = "#6F7378",
             angle = 45)+
    geom_point(color="#E69F00",  size=1.5)+
    theme_pander()+
    scale_x_continuous(expand = c(0, 0), limits = c(-0.05, 1.05))+
    theme(plot.margin=unit(c(1,1,1,1), 'cm'))
    

  #return(plotly::ggplotly(p1))
  ## keeping this 
  return(p1)
} 


# generalized function for assigning values to df --------------------------------------------------
#' assignDFValues
#'
#' @param data : storage dataframe for results. 
#' @param results : output of the ifi konrad comparison
#' @param rowIndex ; what row of the dataframe to use
#'
#' @return dataframe with assinged comparison values 
assignDFValues <- function(data, results, rowIndex){
  data[rowIndex, "no IFI fp"] <- results %>% dplyr::filter(ifi_measure_Flood  ==0) %>%nrow()
  data[rowIndex, "compared hucs"] <- results %>% dplyr::filter(konrad_fp_area > 0) %>%nrow()
  data[rowIndex, "no k_fp"] <- results %>% dplyr::filter(konrad_fp_area ==0) %>%nrow()
  data[rowIndex, "no k_fp overlap"] <- results %>% dplyr::filter(konrad_fp_area > 0 & cell_In_IFI_FP == FALSE) %>% nrow()
  return(data)
}

