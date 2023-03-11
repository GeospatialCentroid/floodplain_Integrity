### 
# set of functions to help generate specific comparison results between resources
# 20230207
# carverd@colostate.edu 
###

pacman::p_load(sf,terra,dplyr, plotly, tmap, readr)
tmap::tmap_mode("view")

# general method 
## determine the total number of huc12s with IFI but no Konrad score 
## total floodplain areas for both IFI and Konrad 
## total floodplain areas shared at each huc12 betweem the datasets
## calculate the total flood plain area define by (IFI, Konrad) within each huc12
## gather the IFI value for each huc 12 
## compute the percent area of summed values against the total area 



# read in datasets --------------------------------------------------------
## IFI data
# flood plain areas 
ifi_fp <- sf::st_read("data/preppedValidataionData/ifi_fpCrop.geojson")
### used for specific information about flood plain area and measures 
# huc12 results 
ifi_results <- sf::st_read("data/preppedValidataionData/ifiResults.geojson")
### used for testing intersection with the konrad data 

## Konrad data
k_fn1 <- rast("data/preppedValidataionData/fn1.tif" )
k_fn2 <- rast("data/preppedValidataionData/fn2.tif" )
k_fn3 <- rast("data/preppedValidataionData/fn3.tif" )
k_fn4 <- rast("data/preppedValidataionData/fn4.tif" )



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
      condition_crop <- u_vals_crop %in% K_index
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
    condition <- u_vals %in% K_index
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



# template df for location; require name structure ------------------------

df_template <- data.frame(matrix(nrow = nrow(ifi_results), ncol = 9))

names(df_template) <- c("huc12_id", "ifi_measure_Flood","ifi_fp_AreaKM2", 
                      "konrad_fp_area", "k_totalCells", "k_sum", 
                      "k_percentArea", "floodPlainAreaRatio", "cell_In_IFI_FP")
# assign values from the ifi data
df_template$huc12_id <- ifi_results$huc12_num
df_template$ifi_measure_Flood <- ifi_results$Floods
df_template$ifi_fp_AreaKM2 <- ifi_results$FP_Areakm2

# Flood reduction ---------------------------------------------------------
## IFI : Flood reduction
## konrad : FN1 --- sum(1,2,3)

kn1 <- calculatePercentages(df = df_template,
                            hucSF = ifi_results,
                            fpSF = ifi_fp,
                            K_rast = k_fn1,
                            K_index = c(1,2,3))

kn1$k_percentArea <- (kn1$k_sum/kn1$k_totalCells)*100
kn1$konrad_fp_area <- kn1$k_totalCells * 0.0001
kn1$floodPlainAreaRatio <- kn1$ifi_fp_AreaKM2 / kn1$konrad_fp_area

write_csv()
#filter steps 


fig_fn1 <- plot_ly(data = kn1, 
               x = ~ifi_measure_Flood, 
               y = ~k_percentArea,
               color = ~floodPlainAreaRatio,
               text = ~paste("huc12: ", huc12_id, "<br>",
                             "flood plain area ratio:", floodPlainAreaRatio)
)
fig_fn1


# processing function to calculate input values   ----------------------------------------------------
for(i in seq_along(ifi_results$huc12)){
  print(i)
  # grab the huc ID, used as an index for the reset of the analysis 
  hucID <- ifi_results[i, "huc12"] %>% st_drop_geometry() %>% pull()
  # grab huc object and convert to a terra vect 
  huc <- ifi_results[i,] %>% vect()

  # crop the konrad data to area of the huc
  crop <- terra::crop(k_fn1, huc )%>%
    terra::mask(huc)
  

  # pull out all values 
  vals <- values(crop)[,1]
  
  # gather all unique values 
  u_vals <- unique(vals)
  # condition to test for the presents of any unique measure 
  condition <- u_vals %in% c(3,2,1,0,-1,-2)
  # validate condition, are any of the values present. 
  if(TRUE %in% condition){
    trueVals <- vals[!is.na(vals)]
    cellAreas$k_totalCells[i] <- length(trueVals)
    cellAreas$k_sum_1_2_3[i] <- length(trueVals[trueVals >= 1])
  }else{
    cellAreas$k_totalCells[i] <- 0 
    cellAreas$k_sum_1_2_3[i] <- NA
  }
}
cellAreas$k_percentArea <- (cellAreas$k_sum_1_2_3/cellAreas$k_totalCells)*100
cellAreas$konrad_fp_area <- cellAreas$k_totalCells * 0.0001
cellAreas$floodPlainAreaRatio <- cellAreas$ifi_fp_AreaKM2 / cellAreas$konrad_fp_area



fig_fn1 <- plot_ly(data = cellAreas, 
               x = ~ifi_measure_Flood, 
               y = ~k_percentArea,
               color = ~floodPlainAreaRatio,
               text = ~paste("huc12: ", huc12_id, "<br>",
                             "flood plain area ratio:", floodPlainAreaRatio)
              )
fig

## example image of the two features 
j <- 770
## 59 bad over lap and score  
## 934 --- Great overlap with area and scores 
## 696 --- good areas alignment but discrepency between score
## 770 --- no overlap between floodplain classes 

# grab the huc ID, used as an index for the reset of the analysis 
hucID <- ifi_results[j, "huc12"] %>% st_drop_geometry() %>% pull()
# grab huc object and convert to a terra vect 
huc <- ifi_results[j,] %>% vect()

# crop the konrad data to area of the huc
crop <- terra::crop(k_fn1, huc )%>%
  terra::mask(huc)
#ifi flood plain 

f1 <- ifi_fp %>% filter(HUC12 == hucID)

tm_shape(ifi_results[j,], "huc12")+
  tm_borders()+
  tm_shape(f1, "ifi_floodplain")+
    tm_borders()+
    tm_shape(crop, "konrad_floodplain")+
      tm_raster()+
  tm_layout(legend.position = c("right", "top"), title= paste(f1$HUC12, ": index", j))

View()
write.csv(x = cellAreas, file = paste0("outputs/validationFloods_fn1",Sys.Date(),".csv"))



  # Sediment regulation -----------------------------------------------------
## IFI : Sediment regulation
## konrad fn2 --- sum(1,2,3,4)

kn2 <- calculatePercentages(df = df_template,
                            hucSF = ifi_results,
                            fpSF = ifi_fp,
                            K_rast = k_fn2,
                            K_index = c(1,2,3,4))

kn2$k_percentArea <- (kn2$k_sum/kn2$k_totalCells)*100
kn2$konrad_fp_area <- kn2$k_totalCells * 0.0001
kn2$floodPlainAreaRatio <- kn2$ifi_fp_AreaKM2 / kn2$konrad_fp_area






# organic and solute regulation -------------------------------------------
## IFI : Organic & solute regulation
## konrad fn3 --- sum(>0)

kn3 <- calculatePercentages(df = df_template,
                            hucSF = ifi_results,
                            fpSF = ifi_fp,
                            K_rast = k_fn3,
                            K_index = c(1,2,3))

kn3$k_percentArea <- (kn3$k_sum/kn3$k_totalCells)*100
kn3$konrad_fp_area <- kn3$k_totalCells * 0.0001
kn3$floodPlainAreaRatio <- kn3$ifi_fp_AreaKM2 / kn3$konrad_fp_area


# habitat provisioning ----------------------------------------------------
## IFI : Habitat provisioning
## konrad fn4 --- sum(1,2)


kn4 <- calculatePercentages(df = df_template,
                            hucSF = ifi_results,
                            fpSF = ifi_fp,
                            K_rast = k_fn4,
                            K_index = c(1,2,3,4)) 
                            
                            kn4$k_percentArea <- (kn4$k_sum/kn4$k_totalCells)*100
                            kn4$konrad_fp_area <- kn4$k_totalCells * 0.0001
                            kn4$floodPlainAreaRatio <- kn4$ifi_fp_AreaKM2 / kn4$konrad_fp_area
                            
                            