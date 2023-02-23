###
# primary processing script for running content 
# carverd@colostate.edu
# 20210203
###
 
# install.packages("pacman")
pacman::p_load(sf,terra,dplyr, tictoc, readr, purrr)

# source scripts ----------------------------------------------------------
for(feature in list.files(path = "src/" ,full.names = TRUE)){
  source(feature)
}


# Calculate Stressor Dataset Density --------------------------------------
## reading from a local file, taking a long time to load pulling from 
floodAreas <- sf::st_read("US_HU12/US_HU12.shp")

# OpenStreetMap -----------------------------------------------------------
states <- st_read("Z:/National Data Downloads/ne_10m_admin_1_states_provinces/ne_10m_admin_1_states_provinces.shp")%>%
  dplyr::filter(adm0_a3 == "USA")%>%
  st_transform(crs = st_crs(floodAreas))%>%
  dplyr::select("postal")
# this takes a long time so I should write it out 
s2 <- st_intersection(x = floodAreas, y = states)
# Join results to flood area file
st_postal <- s2 %>%
  st_drop_geometry()%>%
  group_by(FIRST_huc1)%>%
  dplyr::mutate(postal1 =paste0(postal, collapse = ", "))%>%
  dplyr::select(FIRST_huc1, postal1)
write_csv(x = st_postal, file = "US_HU12/US_HU12)stateIntersection.csv")

st_postal <- read_csv("US_HU12/US_HU12)stateIntersection.csv")

floodAreas2 <- dplyr::left_join(floodAreas, st_postal)
floodAreas2 <- floodAreas2[!duplicated(floodAreas2$FIRST_huc1), ]


### spilt out states to run on specific PCS 
abbrev <- c("AL",
            "AZ","AR","CA","CO","CT","DE","FL","GA","ID",
            "IL","IN","IA","KS","KY","LA","ME","MD","MA","MI",
            "MN","MS","MO","MT","NE","NV","NH","NJ","NM",
            "NY","NC","ND","OH","OK","OR","PA","RI","SC","SD",
            "TN","TX","UT","VT","VA","WA","WV","WI","WY")

# search for completed files 
sort(list.files(path = "outputs/", pattern = "TX", recursive = TRUE ))

completed <- c("AL",
            "AZ","AR",
            "ID",
            "IL","IN","IA","KS","KY","LA","ME","MD","MA","MI",
            "MN","MS","MO","MT","NE","NV","NH","NJ","NM",
            "NY","NC","ND","OH","OK","OR","PA"
)
# list of content to rerun, 
abbrev2 <- abbrev[!abbrev %in% completed]
abbrev3 <- abbrev2[!abbrev2 %in% c("CA","TX")]
# spilt for building and rr completed 
build3 <- abbrev3[abbrev3 !="TN"]
rr3 <- abbrev3

for(i in abbrev3){
  print(i)
  print(sort(list.files(path = "outputs/", pattern = i, recursive = TRUE )))
}

geo3 <- build3[1:5]
geo4 <- build3[6:10]
geo5 <- build3[11:15]

## render process for each pc
# geo8 - testing 
get_buildings(floodPlain =floodAreas2, stateAbbrev = geo8, outputFolder = "outputs/osm/buildings" )
get_r_r(floodPlain =floodAreas2, stateAbbrev = geo8, outputFolder = "outputs/osm/roadsRails")
# geo2 -- completed 20220304
get_buildings(floodPlain =floodAreas2, stateAbbrev = geo2, outputFolder = "outputs/osm/buildings" )
get_r_r(floodPlain =floodAreas2, stateAbbrev = geo2, outputFolder = "outputs/osm/roadsRails" )
# geo3
get_buildings(floodPlain =floodAreas2, stateAbbrev = geo3, outputFolder = "outputs/osm/buildings" )
  get_r_r(floodPlain =floodAreas2, stateAbbrev = geo3, outputFolder = "outputs/osm/roadsRails" )
# geo4
get_buildings(floodPlain =floodAreas2, stateAbbrev = geo4, outputFolder = "outputs/osm/buildings" )
get_r_r(floodPlain =floodAreas2, stateAbbrev = geo4, outputFolder = "outputs/osm/roadsRails" )
# geo5
get_buildings(floodPlain =floodAreas2, stateAbbrev = geo5, outputFolder = "outputs/osm/buildings" )
get_r_r(floodPlain =floodAreas2, stateAbbrev = geo5, outputFolder = "outputs/osm/roadsRails" )
# geo6
get_buildings(floodPlain =floodAreas2, stateAbbrev = geo6, outputFolder = "outputs/osm/buildings" )
get_r_r(floodPlain =floodAreas2, stateAbbrev = geo6, outputFolder = "outputs/osm/roadsRails" )


build1 <- compile(buildings = TRUE, path = "outputs/")
write_csv(x = build1, file = "outputs/compiledOutputs/buildings.csv")
rr_1 <- compile(buildings = FALSE, path = "outputs/")
write_csv(x = rr_1, file = "outputs/compiledOutputs/roads_rails.csv")

### test for missing records 
build1 <- read_csv("outputs/osm/compiledOutputs/buildings.csv")
fp2 <- floodAreas[!floodAreas$FIRST_huc1 %in% build1$FIRST_huc1, ]
# are unclass hucs in state intersection? No, so edit OSM code to run intersection 
states <- st_read("National Data Downloads/cb_2018_us_state_500k/cb_2018_us_state_500k.shp")%>%
  st_transform(crs = st_crs(floodAreas))%>%
  dplyr::select("STUSPS")

# this takes a long time so I should write it out 
s2 <- st_intersection(x = fp2, y = states)

fp4 <- fp2[!fp2$FIRST_huc1 %in% s2$FIRST_huc1, ]
write_csv(x = fp4, file = "US_HU12/US_HU12_noStates_rr.csv")

# Join results to flood area file
st_postal <- s2 %>%
  st_drop_geometry()%>%
  group_by(FIRST_huc1)%>%
  dplyr::select(FIRST_huc1, postal1 = STUSPS)

# write_csv(x = st_postal, file = "US_HU12/US_HU12_stateIntersection_building.csv")

floodAreas2 <- dplyr::left_join(floodAreas, st_postal) %>% dplyr::filter(!is.na(postal1))
floodAreas2 <- floodAreas2[!duplicated(floodAreas2$FIRST_huc1), ]

getOSM_buildings(floodPlain =floodAreas2, stateAbbrev = abbrev, outputFolder = "outputs/osm/rerun/buildings" )




## RR 
rr1 <- read_csv("outputs/osm/compiledOutputs/roads_rails.csv")
fp3 <- floodAreas[!floodAreas$FIRST_huc1 %in% rr1$FIRST_huc1, ]

# this takes a long time so I should write it out 
s2 <- st_intersection(x = fp3, y = states)

fp4 <- fp3[!fp3$FIRST_huc1 %in% s2$FIRST_huc1, ]
write_csv(x = fp4, file = "US_HU12/US_HU12_noStates_rr.csv")

# Join results to flood area file
st_postal <- s2 %>%
  st_drop_geometry()%>%
  group_by(FIRST_huc1)%>%
  dplyr::select(FIRST_huc1,postal1 =  STUSPS)

write_csv(x = st_postal, file = "US_HU12/US_HU12_stateIntersection_rr.csv")

floodAreas2 <- dplyr::left_join(floodAreas, st_postal)%>% dplyr::filter(!is.na(postal1))
floodAreas2 <- floodAreas2[!duplicated(floodAreas2$FIRST_huc1), ]

getOSM_r_r(floodPlain =floodAreas2, stateAbbrev = abbrev, outputFolder = "outputs/osm/rerun/roadsRails")


### combine reran content with existing material 
# full files 
b1 <- read_csv("outputs/osm/compiledOutputs/buildings.csv")
r1 <- read_csv("outputs/osm/compiledOutputs/roads_rails.csv")
# reruns 
b2<- list.files(path = "outputs/osm/rerun/buildings", full.names = TRUE)
for(i in seq_along(b2)){
  b3 <- read_csv(b2[i])%>%
    dplyr::mutate("FIRST_huc1" = as.character(FIRST_huc1))
  if(i == 1){
    b4 <- b3
  }else{
    b4 <- bind_rows(b4, b3)
  }
}
write_csv(x = b4, file ="outputs/osm/rerun/buildings/compiled/buildingRerun.csv")

r2<- list.files(path = "outputs/osm/rerun/roadsRails", full.names = TRUE, pattern = ".csv")
for(i in seq_along(r2)){
  r3 <- read_csv(r2[i])%>%
    dplyr::mutate("FIRST_huc1" = as.character(FIRST_huc1))
  if(i == 1){
    r4 <- r3
  }else{
    r4 <- bind_rows(r4, r3)
  }
}

write_csv(x = r4, file ="outputs/osm/rerun/roadsRails/compiled/roadsRailsRerun.csv")



# National Levee Database --------------------------------------------
leeves_all <- st_read("National Data Downloads/SP 22/NLD/nld.shp") %>% # export features from geodatabase.
  st_transform(crs = st_crs(floodAreas))

# construct dataframe to hold all records 
df <- data.frame(matrix(nrow = nrow(floodAreas), ncol = 3))
names(df) <- c("FIRST_huc1", "Leeve_area", "area")
df$area <- floodAreas$FIRST_area

time <- Sys.time()
nrow(floodAreas)
for(i in 1:nrow(floodAreas)){
  df$FIRST_huc1[i] <- floodAreas$FIRST_huc1[i]
  df$Leeve_area[i] <- getLeveeArea(floodPlain = floodAreas[i,],leeves = leeves_all) 
  # counter for progress
  if(i %% 10 == 0){
    print(paste0(i ," out of ", nrow(df)))
  }
  if(i %% 100 ==0){
    print(paste0((Sys.time() - time), " writing file"))
    write_csv(x = df[(i-100):i, ],
              file = paste0("outputs/leeveArea/leeveArea",i - 100, "_",i,".csv")
    )
  }
  if(i == 82511){
    print(paste0((Sys.time() - time), " writing file"))
    write_csv(x = df[(i-11):i, ],
              file = paste0(outputFolder, "outputs/leeveArea/leeveArea",i - 11, "_",i,".csv")
    )
  }
}
# compile outputs 
compileOutputs(filePath = "outputs/levveArea/compiledLeeveArea",
               fileName = paste0("compiledLeeveArea_",Sys.Date()))
# -------------------------------------------------------------------------

# Global Forest Loss Dataset ----------------------------------------------
rasters <- list.files("National Data Downloads/SP 22/Forest Loss", pattern = ".tif", full.names = TRUE)
rasters <- lapply(rasters[stringr::str_ends(rasters, pattern = ".tif")], FUN = rast)

# construct dataframe to hold all records 
df <- data.frame(matrix(nrow = nrow(floodAreas), ncol = 3))
names(df) <- c("FIRST_huc1", "Forest_Lost", "area")
df$area <- floodAreas$FIRST_area
time <- Sys.time()

for(i in 82501:nrow(floodAreas)){
  df$Forest_Lost[i] <- getForestLost(floodPlain = floodAreas[i,], rasterList = rasters,multiple = FALSE) 
  df$FIRST_huc1[i] <- floodAreas$FIRST_huc1[i]
  # counter for progress
  if(i %% 10 == 0){
    print(paste0(i ," out of ", nrow(df)))
  }
  if(i %% 100 ==0){
    print(paste0((Sys.time() - time), " writing file"))
    write_csv(x = df[(i-100):i, ],
              file = paste0("outputs/forestLost/forestLost",i - 100, "_",i,".csv")
    )
  }
  if(i == 82511){
    print(paste0((Sys.time() - time), " writing file"))
    write_csv(x = df[(i-11):i, ],
              file = paste0("outputs/forestLost/forestLost",i - 11, "_",i,".csv")
    )
  }
}
compileOutputs(filePath = "outputs/forestLost",
               fileName = paste0("/compiledForestLost/compiledForestLost_",Sys.Date()))
### rerun features with multiple states 
mStates <- read_csv("outputs/forestLost/compiledForestLost/compiledForestLost_2022-03-07.csv")%>%
  dplyr::filter(is.na(Forest_Lost))

mFlood <- floodAreas[floodAreas$FIRST_huc1 %in% mStates$FIRST_huc1, ]


### Rerun classification process 
# construct dataframe to hold all records 
df <- data.frame(matrix(nrow = nrow(mFlood), ncol = 3))
names(df) <- c("FIRST_huc1", "Forest_Lost", "area")
df$area <- mFlood$FIRST_area
time <- Sys.time()

for(i in 1:nrow(mFlood)){
  df$Forest_Lost[i] <- getForestLost(floodPlain = mFlood[i,], rasterList = rasters,
                                     multiple = TRUE) 
  df$FIRST_huc1[i] <- mFlood$FIRST_huc1[i]
  # counter for progress
  if(i %% 10 == 0){
    print(paste0(i ," out of ", nrow(df)))
  }
  if(i %% 100 ==0){
    print(paste0((Sys.time() - time), " writing file"))
    write_csv(x = df[(i-100):i, ],
              file = paste0("outputs/forestLost/mutliple/forestLost_multiple",i - 100, "_",i,".csv")
    )
  }
  if(i == nrow(mFlood)){
    print(paste0((Sys.time() - time), " writing file"))
    write_csv(x = df[(i-100):i, ],
              file = paste0("outputs/forestLost/mutliple/forestLost_multiple",i - 100, "_",i,".csv")
    )
  }
}
### replace values with previous NA 
df <- read_csv("outputs/forestLost/compiledForestLost/compiledForestLost_2022-03-09.csv", col_types = cols(.default = "c"))

files <- list.files("outputs/forestLost/mutliple", pattern = ".csv", full.names = TRUE)
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

## read in previous compiled data 
allFeatures <- read_csv("outputs/forestLost/compiledForestLost/compiledForestLost_2022-03-09.csv")

allFeatures <- allFeatures[!allFeatures$FIRST_huc1 %in% lc2$FIRST_huc1, ]

df3 <- bind_rows(allFeatures,lc2)
write_csv(x= df3, file = "outputs/forestLost/compiledForestLost/combinedWithMultiples_20220311.csv")

### evaluate the Current NA values 
d1 <- read_csv("outputs/forestLost/compiledForestLost/combinedWithMultiples_20220311.csv")
d2 <- d1[is.na(d1$Forest_Lost),] 
View(d2)
# two features are not listed in numerical format 
missingHucs <- floodAreas[!floodAreas$FIRST_huc1 %in% df3$FIRST_huc1,]
missingHucs2 <- c(missingHucs$FIRST_huc1, d2$FIRST_huc1[3:4])

# rerun content

fp2 <- floodAreas[floodAreas$FIRST_huc1 %in% missingHucs$FIRST_huc1, ]


df <- data.frame(matrix(nrow = nrow(fp2), ncol = 3))
names(df) <- c("FIRST_huc1", "Forest_Lost", "area")
df$area <- fp2$FIRST_area
time <- Sys.time()



for(i in 1:nrow(fp2)){
  df$Forest_Lost[i] <- getForestLost(floodPlain = fp2[i,], rasterList = rasters,
                                     multiple = TRUE) 
  df$FIRST_huc1[i] <- fp2$FIRST_huc1[i]
  # counter for progress
  if(i == nrow(fp2)){
    print(paste0((Sys.time() - time), " writing file"))
    write_csv(x = df[(i-nrow(fp2)):i, ],
              file = paste0("outputs/forestLost/mutliple/forestLost_na_0311",i - nrow(fp2), "_",i,".csv")
    )
  }
}
# append NA checks to the compiled data 
d1 <- read_csv("outputs/forestLost/compiledForestLost/combinedWithMultiples_20220311.csv")
d2 <- d1[!is.na(d1$Forest_Lost),] 
d2a <- d1[is.na(d1$Forest_Lost),]
# drop 
df1 <- df[df$FIRST_huc1 %in% d2a$FIRST_huc1, ]

df2 <- bind_rows(d2, df[2:3, ])
df2 <- df2[!duplicated(df2$FIRST_huc1), ]
write_csv(x = df2, "outputs/forestLost/compiledForestLost/combinedWithMultiples_20220311_NAcheck.csv")

# double check 131000000000 and rerun 100 features to test for systematic errors 
fp3 <- floodAreas[floodAreas$FIRST_huc1 ==  "131000000000", ]
val <- getForestLost(floodPlain = fp3, rasterList = rasters,
                                   multiple = FALSE) 
# rerun content
fp4 <- floodAreas[1:100,]
df <- data.frame(matrix(nrow = nrow(fp4), ncol = 3))
names(df) <- c("FIRST_huc1", "Forest_Lost", "area")
df$area <- fp4$FIRST_area
time <- Sys.time()

for(i in 1:nrow(fp4)){
  df$Forest_Lost[i] <- getForestLost(floodPlain = fp4[i,], rasterList = rasters,
                                     multiple = TRUE) 
  df$FIRST_huc1[i] <- fp4$FIRST_huc1[i]
}



# -------------------------------------------------------------------------

# National Landcover Database ---------------------------------------------
## land cover 
nlcd_lc <-terra::rast("L:/Projects_active/EnviroScreen/data/NLCD/Land Cover/nlcd_2019_land_cover_l48_20210604.img")
## render landcover 
lc <- getNLCD_lc(polygons = floodAreas,landcover = nlcd_lc, outputFolder = "F:/geoSpatialCentroid/floodplainIntagrety/outputs/nlcd_lc", startIndex = 69441)

## impervious 
nlcd_impervious <- terra::rast("National Data Downloads/SP 22/NLCD/2019 percent imperviousness/nlcd_2019_impervious_l48_20210604.img")
## render impervious 
getNLCD_impervious(polygons = floodAreas, impervious = nlcd_impervious,
                   outputFolder = "outputs/impervious", startIndex = 1)
compileOutputs(filePath = "outputs/impervious", fileName = "compiled/impervious_20220301")
# -------------------------------------------------------------------------

# NHDPLUS  ----------------------------------------------------------------
### need to determine how to index features from the full element. should be able to follow 
### methods from the leeve dataset, with a length rather then area measure 

canals <- st_read("National Data Downloads/NHD_Canal_ditch/canal_floodplain_join.shp")
canalData <- getCanals(floodPlain = floodAreas , canals = canals)
write_csv(x = canalData, file = "outputs/canals/canals.csv")

# -------------------------------------------------------------------------

# Active Groundwater Level Network  ---------------------------------------
wells <- st_read("National Data Downloads/SP 22/Groundwater Wells/awl_wells/awl_wells.shp")
getGW_Wells(polygons = floodAreas,wells = wells, outputFolder ="outputs/wells", startIndex = 1)
compileOutputs(filePath = "outputs/wells", fileName = "summary/wells_20220218")

### rerun the 511 that did not successfully run 
errorWells <- read.csv("National Data Downloads/SP 22/Groundwater Wells/missing wells.csv")
eFA <- floodAreas[floodAreas$FIRST_huc1 %in% errorWells$missing.HUC12, ]
getGW_Wells(polygons = eFA,wells = wells, outputFolder ="outputs/wells/rerun", startIndex = 1)


# add the rerun features back into the full compiled files
allWells <- read_csv("outputs/wells/summary/wells_20220218.csv")
reruns <- read_csv("outputs/wells/rerun/wells_0_511.csv")%>%
  dplyr::mutate(FIRST_huc1 = as.character(FIRST_huc1))
wells <- bind_rows(allWells, reruns)
write_csv(x = wells, file = "outputs/wells/summary/wells_20220309.csv")
# -------------------------------------------------------------------------

# Landfire Existing Vegetation type  --------------------------------------

landfire <- terra::rast("K:/Kira Simonson/National Data Downloads/SP 22/Landfire (vegetation type)/LF2016_EVT_200_CONUS/Tif/LC16_EVT_200.tif")
terra::activeCat(landfire)<-"EVT_GP"

#render vegetation types
veg <- getlandfire(polygons = floodAreas,EVT = landfire, outputFolder = "K:/Kira Simonson/Stressor Densities/veg_csv", startIndex =71301)

