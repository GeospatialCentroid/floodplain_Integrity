### 
# set of functions to help generate specific comparison results between resources
# 20230207
# carverd@colostate.edu 
###

pacman::p_load(sf,terra,dplyr, plotly, tmap, readr, RColorBrewer,ggthemes)
tmap::tmap_mode("view")
# source functions 
source("src/validationVisualizationFunctions.R")

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
# ### used for specific information about flood plain area and measures 
# # huc12 results 
ifi_results <- sf::st_read("data/preppedValidataionData/ifiResults.geojson")
### used for testing intersection with the konrad data 

## Konrad data
k_fn1 <- rast("data/preppedValidataionData/fn1.tif" )
k_fn2 <- rast("data/preppedValidataionData/fn2.tif" )
# k_fn3 <- rast("data/preppedValidataionData/fn3.tif" )
k_fn4 <- rast("data/preppedValidataionData/fn4.tif" )




# template df for location; require name structure ------------------------

df_template <- data.frame(matrix(nrow = nrow(ifi_results), ncol = 9))

names(df_template) <- c("huc12_id", "ifi_measure_Flood","ifi_fp_AreaKM2", 
                      "konrad_fp_area", "k_totalCells", "k_sum", 
                      "k_percentArea", "floodPlainAreaRatio", "cell_In_IFI_FP")
# assign values from the ifi data
df_template$huc12_id <- ifi_results$huc12_num
df_template$ifi_measure_Flood <- ifi_results$Floods
df_template$ifi_fp_AreaKM2 <- ifi_results$FP_Areakm2



# template df for features not included -----------------------------------

df2 <- data.frame(matrix(nrow = 4, ncol = 6))
colnames(df2) <- c("k_category", "total hucs","compared hucs", "no IFI fp", 
                    "no k_fp","no k_fp overlap")
df2$k_category <- c("FN1_flood_reduction", 
                    "FN2_sediment_reduction",
                    "FN3_organic_solute_reduction",
                    "FN4_habitat_provisioning")
df2$`total hucs` <- nrow(ifi_results)


# Flood reduction ---------------------------------------------------------
  ## IFI : Flood reduction
  ## konrad : FN1 --- sum(1,2,3)
file1 <- "outputs/validationResults/allResults_kn1.csv"
if(file.exists(file1)){
  kn1 <- read.csv(file1)
}else{
  kn1 <- calculatePercentages(df = df_template,
                              hucSF = ifi_results,
                              fpSF = ifi_fp,
                              K_rast = k_fn1,
                              K_index = c(1,2,3))
}


kn1$k_percentArea <- (kn1$k_sum/kn1$k_totalCells)
kn1$konrad_fp_area <- kn1$k_totalCells * 0.0001
kn1$floodPlainAreaRatio <- kn1$ifi_fp_AreaKM2 / kn1$konrad_fp_area
#write_csv(x = kn1, file ="outputs/validationResults/allResults_kn1.csv")


# generate comparison summary 
df2 <- assignDFValues(data = df2, results = kn1, rowIndex = 1)


#filter results to where comparison was possible  
kn1_a <- kn1 %>%
  dplyr::filter(konrad_fp_area > 0)

#write_csv(x = kn1_a, file ="outputs/validationResults/filteredResults_kn1.csv")

# generate plot 
f1 <- getFigure(data = kn1_a)+
  xlab("Flood Reduction IFI")+
  ylab("Fraction of Floodplain from Konrad 2015")
f1

ggplot2::ggsave(filename = paste0("outputs/validationResults/fn1_",Sys.Date(),".png"), plot = f1,
                width =6 ,height = 6, units = "in", dpi = 300)
ggplot2::ggsave(filename = paste0("outputs/validationResults/fn1_",Sys.Date(),".pdf"), plot = f1,
                width =6 ,height = 6, units = "in", dpi = 300)

 
# Sediment regulation -----------------------------------------------------
## IFI : Sediment regulation
## konrad fn2 --- sum(1,2,3,4)
file2 <- "outputs/validationResults/allResults_kn2.csv"
if(file.exists(file2)){
  kn2 <- read.csv(file2)
}else{
  kn2 <- calculatePercentages(df = df_template,
                              hucSF = ifi_results,
                              fpSF = ifi_fp,
                              K_rast = k_fn2,
                              K_index = c(1,2,3,4))
}



kn2$k_percentArea <- (kn2$k_sum/kn2$k_totalCells)
kn2$konrad_fp_area <- kn2$k_totalCells * 0.0001
kn2$floodPlainAreaRatio <- kn2$ifi_fp_AreaKM2 / kn2$konrad_fp_area

#write_csv(x = kn2, file ="outputs/validationResults/allResults_kn2.csv")


# generate comparison summary 
df2 <- assignDFValues(data = df2, results = kn2,
                      rowIndex = 2)


#filter results to where comparison was possible  
kn2_a <- kn2 %>%
  dplyr::filter(konrad_fp_area > 0)

#write_csv(x = kn2_a, file ="outputs/validationResults/filteredResults_kn2.csv")

# generate plot 
f2 <- getFigure(data = kn2_a)+
  xlab("Sediment Regulation IFI")+
  ylab("Fraction of Floodplain from Konrad 2015")
f2
ggplot2::ggsave(filename = paste0("outputs/validationResults/fn2_",Sys.Date(),".png"),
                plot = f2,
                width =6 ,height = 6, units = "in", dpi = 300)
ggplot2::ggsave(filename = paste0("outputs/validationResults/fn2_",Sys.Date(),".pdf"), 
                plot = f2,
                width =6 ,height = 6, units = "in", dpi = 300)




# organic and solute regulation -------------------------------------------
## IFI : Organic & solute regulation
## konrad fn3 --- sum(>0)
# file3 <- "outputs/validationResults/allResults_kn3.csv"
# if(file.exists(file3)){
#   kn3 <- read.csv(file3)
# }else{
#   kn3 <- calculatePercentages(df = df_template,
#                               hucSF = ifi_results,
#                               fpSF = ifi_fp,
#                               K_rast = k_fn3,
#                               K_index = c(1,2,3))
# }
# 
# 
# kn3$k_percentArea <- (kn3$k_sum/kn3$k_totalCells)
# kn3$konrad_fp_area <- kn3$k_totalCells * 0.0001
# kn3$floodPlainAreaRatio <- kn3$ifi_fp_AreaKM2 / kn3$konrad_fp_area
# 
# #write_csv(x = kn3, file ="outputs/validationResults/allResults_kn3.csv")
# 
# 
# # generate comparison summary 
# df2 <- assignDFValues(data = df2, results = kn3, rowIndex = 3)
# 
# 
# #filter results to where comparison was possible  
# kn3_a <- kn3 %>%
#   dplyr::filter(konrad_fp_area > 0)
# 
# #write_csv(x = kn3_a, file ="outputs/validationResults/filteredResults_kn3.csv")
# 
# # generate plot 
# f3 <- getFigure(data = kn1_a)
# ggplot2::ggsave(filename = "outputs/validationResults/fn3.png", plot = f3)
# 

# habitat provisioning ----------------------------------------------------
## IFI : Habitat provisioning
## konrad fn4 --- sum(1,2)

file4 <- "outputs/validationResults/allResults_kn4.csv"
if(file.exists(file4)){
  kn4 <- read.csv(file4)
}else{
  kn4 <- calculatePercentages(df = df_template,
                              hucSF = ifi_results,
                              fpSF = ifi_fp,
                              K_rast = k_fn4,
                              K_index = c(1,2)) 
}

                            
kn4$k_percentArea <- (kn4$k_sum/kn4$k_totalCells)
kn4$konrad_fp_area <- kn4$k_totalCells * 0.0001
kn4$floodPlainAreaRatio <- kn4$ifi_fp_AreaKM2 / kn4$konrad_fp_area
    
#write_csv(x = kn4, file ="outputs/validationResults/allResults_kn4.csv")


# generate comparison summary 
df2 <- assignDFValues(data = df2, results = kn4, rowIndex = 4)
#write_csv(x = df2, file ="outputs/validationResults/comparisonSummary.csv")


#filter results to where comparison was possible  
kn4_a <- kn4 %>%
  dplyr::filter(konrad_fp_area > 0)

#write_csv(x = kn4_a, file ="outputs/validationResults/filteredResults_kn4.csv")

# generate plot 
f4 <- getFigure(data = kn4_a)+
  xlab("Habitat Provisioning IFI")+
  ylab("Fraction of Floodplain from Konrad 2015")
f4
ggplot2::ggsave(filename = paste0("outputs/validationResults/fn4_",Sys.Date(),".png"), 
                plot = f4,
                width =6 ,height = 6, units = "in", dpi = 300)
    
ggplot2::ggsave(filename = paste0("outputs/validationResults/fn4_",Sys.Date(),".pdf"),
                plot = f4,
                width =6 ,height = 6, units = "in", dpi = 300)  
                              