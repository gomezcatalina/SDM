#Species Distribution Models in the Northwest Atlantic Ocean
#By Catalina Gomez, Paul Regular, Amy-Lee Kouwenberg
#Last updated: August 2016
setwd("D:/RProjects/SDM_notShared")
library(plyr)
library(raster)
library(rgdal)
options(java.parameters = "-Xmx1g" )
#system.file('java', package='dismo') #"C:/Users/gomezc/Documents/R/win-library/3.1/dismo/java"
library(dismo)
library(rJava)
library(maptools)
library(RColorBrewer) 
require(maps)
require(sp)
require(rgdal)
library(lubridate)

###**************************************************************************************************##
##------------------------------------PART I: Open CSV file with sightings of your species of interest ---
## The minimum infomration that you need is species, latitude and longitude
# head(MarMammSightings)
#     
#               Unknown Dolphin -43.71667 43.86667 1999     7             
#                   Minke Whale -44.33833 44.27833 2006     8                  
#                   Minke Whale -44.33833 44.28000 2007     7                  

MarMammSightings <- read.csv("data/MarMammSightings.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
MarMammSightings$Species <- factor(MarMammSightings$Species)
MarMammSightings$Platform <- factor(MarMammSightings$Species)

###**************************************************************************************************##
#####---------------------------------PART II: Select species you want to model and season------------#
#(1975 - 2015)
#MarMamm_Winter <- MarMammSightings[MarMammSightings$Month %in% c('12', '1', '2'), ]
#MarMamm_Spring <- MarMammSightings[MarMammSightings$Month %in% c('3', '4', '5'), ]
MarMamm_Summer <- MarMammSightings[MarMammSightings$Month %in% c('6', '7', '8'), ]
#MarMamm_Fall <- MarMammSightings[MarMammSightings$Month %in% c('9', '10', '11'), ]
table(MarMamm_Summer$Species)

#Run the script below if you want to create csv files for each species (e.g. if you want to run MaxEnt outside R using the dismo): 

#setwd("R:/Science/Cetacean Monitoring/HSM Projects/SDM_all_cetaceans/MarineMammalData/MarMammData_for_MaxEnt")
# setwd("D:/GIS/Data/MarMammData/Cetaceans_for_MaxEnt")
# sp <- c('Atlantic White-sided Dolphin', 'Beluga', 'Minke Whale', 
#         'Humpback Whale', 'Blue Whale', 'Fin Whale', 'White-beaked Dolphin', 
#         'Killer Whale', 'Northern Bottlenose Whale', 'Sei Whale', 'Long-finned Pilot Whale', 
#         'North Atlantic Right Whale', 'Bowhead Whale', 'Harbour Porpoise', 'Common Dolphin', 'Sperm Whale')
# for(s in sp) {
#   target <- MarMamm_Summer[MarMamm_Summer$Species == s , ] # grabs data from specific species
#   no.target <- MarMamm_Summer[MarMamm_Summer$Species != s , ] # grabs all other species data
#   write.csv(target, file = paste0(s, " _summer.csv"), row.names = FALSE)
#   write.csv(no.target, file = paste0(s, " _summer_excluded.csv"), row.names = FALSE)
# }


###**************************************************************************************************##
###------------------------------------PART III: Read/select your predictors/environmental layers------------------------#
# CG prepared the environmental layers for MaxEnt using ArcMap. There are several tutorial to do this in more detail e.g. http://clp-foss4g-workshop.readthedocs.io/en/latest/maxent_data_prep.html
# This crip will read the ones for the cetacean SDM - DO NOT DISTRIBUTE THIS LAYERS (contact: Cesar Fuentes Yavo for SST and CHL)

#Once layers are ready, place them in the folder (predictors) and read them all:  
rasterdir <- "predictors/"
predictorfiles = list.files(path = paste(rasterdir, sep=""), 
                            pattern = "\\.asc$", full.names = F)

#****************************** S U M M E R *******************************************
#Select layers for your summer SDM
#If you want to ignore files within the folder selected above just add them to the following list:
predictorfiles <- predictorfiles[predictorfiles != "chl_concen_spring.asc" &
                                  predictorfiles != "chl_concen_summer.asc" &
                                  predictorfiles != "chl_magn_fall.asc" &
                                  predictorfiles != "chl_pers_fall.asc" &
                                  predictorfiles != "sst_fall.asc" &
                                  predictorfiles !=  "chl_magn_wint.asc" &
                                  predictorfiles !=  "sst_spring.asc" &
                                  predictorfiles !=  "chl_magn_winter.asc" &
                                  predictorfiles !=  "chl_pers_wint.asc" &
                                  predictorfiles !=  "chl_pers_winter.asc" &
                                  predictorfiles !=  "sst_winter.asc"] 
predictors = c()
for(x in predictorfiles)
{
  predname = paste(rasterdir, x, sep="")
  predictors = stack(c(predictors, raster(predname)))
}
predictors <- setMinMax(predictors)
predictors 
#plot(predictors) #Check that these are the preictors you want to include in you model!
#str(predictors) 


#****************************** F A L L *******************************************
# predictorfiles <- predictorfiles[  predictorfiles != "chl_magn_spring.asc" &
#                                      predictorfiles != "chl_pers_winter.asc" &
#                                      predictorfiles != "chl_magn_winter.asc" &
#                                      predictorfiles != "sst_summer.asc" &
#                                      predictorfiles != "chl_concen_spring.asc" &
#                                      predictorfiles != "chl_concen_summer.asc" &
#                                      predictorfiles != "chl_magn_spring.asc" &
#                                      predictorfiles != "chl_pers_spring.asc" &
#                                      predictorfiles !=  "chl_magn_wint.asc" &
#                                      predictorfiles !=  "sst_spring.asc" &
#                                      predictorfiles !=  "chl_pers_wint.asc" &
#                                      predictorfiles !=  "sst_winter.asc"]  
# predictors = c()
# for(x in predictorfiles)
# {
#   predname = paste(rasterdir, x, sep="")
#   predictors = stack(c(predictors, raster(predname)))
# }
# predictors <- setMinMax(predictors)
# predictors
# plot(predictors)


###**************************************************************************************************##
####------------------------------------PART IV: Creates function to select non-target species and bias file------------------------#
# Bias file - sightings used icome often from opportunistic surveys and thus do not have a measure of survey effort. 
#Sightings of cetacean species other than the ones that will be modeled are available, and we term these 
#non-target group species (non-TGS). This section creates a sampling distribution bias map 
#by plotting these non-TGS records in the study area. 

maxentData <- function(db = NULL, # provide mammal database here
                       hires.r = NULL, # high resolution raster (must match predictors raster)
                       lowres.r = NULL, # low resolution raster
                       species = NULL # focal species
) {
  
  target <- db[db$Species == species , c("Longitude", "Latitude")] # grabs data from specific species
  ts <- db[db$Species != species , ] # grabs all other species data
  x <- rasterize(ts[, c("Longitude", "Latitude")], lowres.r, fun = sum) # rasterize using big grid
  x[x > 0] <- 1
  bias <- resample(x, hires.r, method = "ngb") # match resolution of small grid
  crs(bias) <- proj4string(hires.r)
  bias <- randomPoints(bias, 10000)
  list(target = SpatialPoints(target), bias = SpatialPoints(bias)) # return data in a list
  
}

###**************************************************************************************************##
###------------------------------------PART V: Select only sightings that are inside of the study area-------#
#StudyArea <- readOGR("D:/GIS/Data/Polygons", "StudyArea")
StudyArea <- readOGR("Polygons", "StudyArea")
crs(StudyArea) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
plot(StudyArea)
#pts <- points(MarMamm_Summer$Longitude, MarMamm_Summer$Latitude, pch=20, col="red", cex=0.5) 
pts <- SpatialPoints(MarMamm_Summer[c(2,3)])
options(warn=1)
pts <- SpatialPointsDataFrame(pts, MarMamm_Summer)
proj4string(pts) = proj4string(StudyArea)
#points(pts[StudyArea,], pch = 3)
inside.StudyArea <- !is.na(over(pts, as(StudyArea, "SpatialPolygons"))) 
sum(inside.StudyArea) #Number of sightings inside of the study area that will be used in SDM
points(pts[!inside.StudyArea, ], pch=20, col="red", cex=0.5) #--> will not be inlucuded in MaxEnt
sum(!inside.StudyArea)
points(pts[inside.StudyArea, ], pch=20, col="green", cex=0.5) #--> will not be included in Maxent
MarMamm_Summer_NWAO <- pts[inside.StudyArea, ]
str(MarMamm_Summer_NWAO)
str(MarMamm_Summer)
MarMamm_Summer_NWAO <- as.data.frame(MarMamm_Summer_NWAO)
table(MarMamm_Summer_NWAO$Species)
#write.csv(MarMamm_Summer_NWAO, "D:/GIS/Data/MarMammData/MarMamm_Summer.csv", row.names=FALSE)
#*****************************

###**************************************************************************************************##
###------------------------------------PART VI: Select species to be modeled: TGS------------------------#
table(MarMamm_Summer_NWAO$Species)
hires.r <-lowres.r <-raster(predictors) #to slice up the lowres.r grid into finer grid (matching to enviro layers) so that it would run in MaxEnt.
#res(lowres.r) <- c(0.5, 0.5) #aggregates all the records (except the target species) in a 0.5 degree X 0.5 degree grid to get densities.  Hence, that is the grid that we report, since that is what the densities come from.
# Modify according with your preferences/hypothesis:

res(lowres.r) <- c(0.009, 0.009) # 0.0090 degrees -> 1 km
# res(lowres.r) <- c(0.0224, 0.0224) # 0.0224 degrees - 2.5 km
# res(lowres.r) <- c(0.0448, 0.0448) # 0.0224 degrees - 5 km

dat <- maxentData(db = MarMamm_Summer_NWAO, hires.r = hires.r,
                   lowres.r = lowres.r, species = c("Northern Bottlenose Whale"))
str(dat)
 
TGS_Summer <- MarMamm_Summer_NWAO[MarMamm_Summer_NWAO$Species %in% c("Northern Bottlenose Whale"), ]
str(TGS_Summer)   

###**************************************************************************************************##
###---------------------PART VII: Visualize sightings and bias map that will be used in SDM: ALWAYS a good idea!------#
plot(dat$bias, pch=20,col="lightgrey") #-->plot bias file
plot(StudyArea, add=TRUE)
points(dat$target, pch = 20, col = "red", cex=0.5)


###**************************************************************************************************##
###---------------------PART VIII: Sub-sampling per species - code ammended from http://www.molecularecologist.com/2013/04/species-distribution-models-in-r/
# Mixed random- systematic sampling of TGS records – 
# MaxEnt discards redundant records that occur in a single cell (records with the same geographic coordinates); 
# however, it does not discard multiple records that can occur in neighbouring cells and thus may over represent 
# regions with high sampling efforts (Kadmon et al.  2004). With the aim of reducing the spatial aggregation of 
# records in neighbouring cells, we randomly subsampled one whale sighting on a predetermined grid (Fourcade et al. 2014).
# To explore how the size of the grids may impact the SDM results, we conducted this subsampling at three different spatial
# resolutions: 1, 2.5, and 5 km. In this way, we used four datasets for each species to model their suitable habitat: 
#   not sampled, and sampled at 1, 2.5 and 5 km spatial resolutions.

#Check sample sizes first!
locs <- dat$target
locs <-as.data.frame(locs)
dim(locs)
str(locs)
head(locs)

#uncomment the following to do each sub-sample at different scales

# ############## S u b  - s a m p l e    0.0090 degrees - 1 km
longrid = seq(-67.81438,-41.98105,0.00895335303071)
latgrid = seq(39.09167,60.99167,0.00895335303071)

# identify points within each grid cell, draw one at random
subs_1km = c()

for(i in 1:(length(longrid)-1)){
  for(j in 1:(length(latgrid)-1)){
    gridsq = subset(locs, Latitude > latgrid[j]
                    & Latitude < latgrid[j+1]
                    & Longitude > longrid[i]
                    & Longitude < longrid[i+1])
    if(dim(gridsq)[1]>0){subs_1km = rbind(subs_1km, gridsq[sample(1:dim(gridsq)[1],1 ), ])}
  }
}

dim(subs_1km) # confirm that you have a smaller dataset than you started with
dim(locs)

subs_1km <- SpatialPoints(subs_1km)

#points(locs$Longitude, locs$Latitude, col='deeppink', pch=1, cex=0.1)
#points(subs$Longitude, subs$Latitude, col='black', pch=20, cex=0.01)
#str(subs)
#
# ############## S u b  - s a m p l e    0.0224 degrees - 2.5 km
longrid = seq(-67.81438,-41.98105,0.022383382576775)
latgrid = seq(39.09167,60.99167,0.022383382576775)

# identify points within each grid cell, draw one at random
subs_2.5km = c()

for(i in 1:(length(longrid)-1)){
  for(j in 1:(length(latgrid)-1)){
    gridsq = subset(locs, Latitude > latgrid[j]
                    & Latitude < latgrid[j+1]
                    & Longitude > longrid[i]
                    & Longitude < longrid[i+1])
    if(dim(gridsq)[1]>0){subs_2.5km = rbind(subs_2.5km, gridsq[sample(1:dim(gridsq)[1],1 ), ])}
  }
}

dim(subs_2.5km) # confirm that you have a smaller dataset than you started with
dim(locs)

subs_2.5km <- SpatialPoints(subs_2.5km)

#points(locs$Longitude, locs$Latitude, col='deeppink', pch=1, cex=0.1)
#points(subs$Longitude, subs$Latitude, col='black', pch=20, cex=0.01)
#str(subs)

############## S u b  - s a m p l e    0.0448 degrees - 5 km
longrid = seq(-67.81438,-41.98105,0.04476676515355)
latgrid = seq(39.09167,60.99167,0.04476676515355)
plot(longrid)

subs_5km = c()

for(i in 1:(length(longrid)-1)){
  for(j in 1:(length(latgrid)-1)){
    gridsq = subset(locs, Latitude > latgrid[j] 
                    & Latitude < latgrid[j+1] 
                    & Longitude > longrid[i] 
                    & Longitude < longrid[i+1])    
    if(dim(gridsq)[1]>0){subs_5km = rbind(subs_5km, gridsq[sample(1:dim(gridsq)[1],1 ), ])}
  }
}


dim(subs_5km) # confirm that you have a smaller dataset than you started with
dim(locs)

#Save records subsampled:
# write.csv(subs_5km, "outputs/subs_5km_Summer.csv", row.names=FALSE)
# subs_5km <- SpatialPoints(subs_5km)
#  

#points(locs$Longitude, locs$Latitude, col='deeppink', pch=1, cex=0.1)
#points(subs$Longitude, subs$Latitude, col='black', pch=20, cex=0.01)
#str(subs)

###**************************************************************************************************##
###------------------------------------PART IX: Run Model---------------------------------------------#
#http://www.recibio.net/wp-content/uploads/2014/11/Modelando-el-nicho-ecologico_MaribelArenasNavarro.pdf
#vignette('sdm', 'dismo')
# Following Phillips et al. (2006) and Merow et al. (2013), the MaxEnt runs were conducted using the following settings: 
#   selected random seed, 
#   maximum number of background points = 10,000 (a random sample of point locations from the landscape to represent the environmental conditions in the study area), 
#   regularization multiplier = 1 (included to reduce over-fitting), 
#   number of replicates = 100 (to do multiple runs for the same species/season as a means to provide averages of the results from all models created), 
#   no output grids, 
#   maximum iterations = 5000 (allows the model to have adequate opportunity for convergence), 
#   convergence threshold = 0.00001. 
#   To assess uncertainty in model predictions, cross-validation replication was used,which incorporates all available sightings, making better use of smaller data-sets
#   Cumulative output type was selected to visualize MaxEnt results; this output does not rely on post-processing assumptions and it is useful when illustrating potential species range boundaries 

########### 1: No subsample, no bias file ################
# model <- maxent(x = predictors, p = dat$target, removeDuplicates=TRUE,
#                 path="output/NoBias/No_subsample",
#                 args = c("-P", "-J", "replicates=100", "replicatetype=Crossvalidate",
#                          "randomseed", "nooutputgrids",
#                          "maximumiterations=5000",
#                          "betamultiplier=1"))
# 
# map <- predict(model, predictors, progress='text', args=c("outputformat=raw"))
# plot(map)
# HSMap <- mean(map)
# plot(HSMap)
# points(pts[inside.StudyArea, ], pch=20, col="black", cex=0.5)
# 
# scaledCumsum <- function(x){
#   z <- values(x)
#   nas <- is.na(z)
#   sorted <- sort(z, index.return = TRUE, na.last = NA, method = "quick")
#   cum <- cumsum(sorted$x)
#   z2 <- rep(NA, length(z))
#   z2[!nas][sorted$ix] <- cum/max(cum)*100
# 
#   values(x)=z2
#   return(x)
# }
# 
# HSMap_cumulative <- scaledCumsum(HSMap)
# plot(HSMap_cumulative)
# outfile = "output/NoBias/No_subsample/HSMap_cumulative_nobias.tif"
# writeRaster(HSMap_cumulative, filename=outfile, overwrite=TRUE, format="GTiff", datatype="FLT4S")
# 
# # 
# # ########## 2: No subsample, with bias file ################
model <- maxent(x = predictors, p = dat$target, removeDuplicates=TRUE, a = dat$bias,
                path="output/Bias/No_subsample",
                args = c("-P", "-J", "replicates=100", "replicatetype=Crossvalidate",
                         "randomseed", "nooutputgrids",
                         "maximumiterations=5000",
                         "betamultiplier=1"))

map <- predict(model, predictors, progress='text', args=c("outputformat=raw"))
plot(map)
HSMap <- mean(map)
plot(HSMap)
points(pts[inside.StudyArea, ], pch=20, col="black", cex=0.5)

scaledCumsum <- function(x){
  z <- values(x)
  nas <- is.na(z)
  sorted <- sort(z, index.return = TRUE, na.last = NA, method = "quick")
  cum <- cumsum(sorted$x)
  z2 <- rep(NA, length(z))
  z2[!nas][sorted$ix] <- cum/max(cum)*100

  values(x)=z2
  return(x)
}

HSMap_cumulative <- scaledCumsum(HSMap)
plot(HSMap_cumulative)
outfile = "output/Bias/No_subsample/HSMap_cumulative_bias.tif"
writeRaster(HSMap_cumulative, filename=outfile, overwrite=TRUE, format="GTiff", datatype="FLT4S")


# 
# ########## 3: 1km subsample, no bias file ################
# 
# model <- maxent(x = predictors, p = subs_1km, removeDuplicates=TRUE,
#                 path="output/NoBias/Subsample_1km",
#                 args = c("-P", "-J", "replicates=100", "replicatetype=Crossvalidate",
#                          "randomseed", "nooutputgrids",
#                          "maximumiterations=5000",
#                          "betamultiplier=1"))
# 
# map <- predict(model, predictors, progress='text', args=c("outputformat=raw"))
# plot(map)
# HSMap <- mean(map)
# plot(HSMap)
# points(pts[inside.StudyArea, ], pch=20, col="black", cex=0.5)
# 
# scaledCumsum <- function(x){
#   z <- values(x)
#   nas <- is.na(z)
#   sorted <- sort(z, index.return = TRUE, na.last = NA, method = "quick")
#   cum <- cumsum(sorted$x)
#   z2 <- rep(NA, length(z))
#   z2[!nas][sorted$ix] <- cum/max(cum)*100
# 
#   values(x)=z2
#   return(x)
# }
# 
# HSMap_cumulative <- scaledCumsum(HSMap)
# plot(HSMap_cumulative)
# outfile = "output/NoBias/Subsample_1km/HSMap_cumulative_1kmnobias.tif"
# writeRaster(HSMap_cumulative, filename=outfile, overwrite=TRUE, format="GTiff", datatype="FLT4S")

# 
# ########## 4: 1km subsample, with bias file ################

model <- maxent(x = predictors, p = subs_1km, removeDuplicates=TRUE, a = dat$bias,
                path="output/Bias/Subsample_1km",
                args = c("-P", "-J", "replicates=100", "replicatetype=Crossvalidate",
                         "randomseed", "nooutputgrids",
                         "maximumiterations=5000",
                         "betamultiplier=1"))

map <- predict(model, predictors, progress='text', args=c("outputformat=raw"))
plot(map)
HSMap <- mean(map)
plot(HSMap)
points(pts[inside.StudyArea, ], pch=20, col="black", cex=0.5)

scaledCumsum <- function(x){
  z <- values(x)
  nas <- is.na(z)
  sorted <- sort(z, index.return = TRUE, na.last = NA, method = "quick")
  cum <- cumsum(sorted$x)
  z2 <- rep(NA, length(z))
  z2[!nas][sorted$ix] <- cum/max(cum)*100

  values(x)=z2
  return(x)
}

HSMap_cumulative <- scaledCumsum(HSMap)
plot(HSMap_cumulative)
outfile = "output/Bias/Subsample_1km/HSMap_cumulative_1kmbias.tif"
writeRaster(HSMap_cumulative, filename=outfile, overwrite=TRUE, format="GTiff", datatype="FLT4S")

# 
# 
# ########## 5: 2.5km subsample, no bias file ################
# model <- maxent(x = predictors, p = subs_2.5km, removeDuplicates=TRUE,
#                 path="output/NoBias/Subsample_2.5km",
#                 args = c("-P", "-J", "replicates=100", "replicatetype=Crossvalidate",
#                          "randomseed", "nooutputgrids",
#                          "maximumiterations=5000",
#                          "betamultiplier=1"))
# 
# map <- predict(model, predictors, progress='text', args=c("outputformat=raw"))
# plot(map)
# HSMap <- mean(map)
# plot(HSMap)
# points(pts[inside.StudyArea, ], pch=20, col="black", cex=0.5)
# 
# scaledCumsum <- function(x){
#   z <- values(x)
#   nas <- is.na(z)
#   sorted <- sort(z, index.return = TRUE, na.last = NA, method = "quick")
#   cum <- cumsum(sorted$x)
#   z2 <- rep(NA, length(z))
#   z2[!nas][sorted$ix] <- cum/max(cum)*100
# 
#   values(x)=z2
#   return(x)
# }
# 
# HSMap_cumulative <- scaledCumsum(HSMap)
# plot(HSMap_cumulative)
# outfile = "output/NoBias/Subsample_2.5km/HSMap_cumulative_2.5kmnobias.tif"
# writeRaster(HSMap_cumulative, filename=outfile, overwrite=TRUE, format="GTiff", datatype="FLT4S")

# ########## 6: 2.5km subsample, with bias file ################

model <- maxent(x = predictors, p = subs_2.5km, removeDuplicates=TRUE, a = dat$bias,
                path="output/Bias/Subsample_2.5km",
                args = c("-P", "-J", "replicates=100", "replicatetype=Crossvalidate",
                         "randomseed", "nooutputgrids",
                         "maximumiterations=5000",
                         "betamultiplier=1"))

map <- predict(model, predictors, progress='text', args=c("outputformat=raw"))
plot(map)
HSMap <- mean(map)
plot(HSMap)
points(pts[inside.StudyArea, ], pch=20, col="black", cex=0.5)

scaledCumsum <- function(x){
  z <- values(x)
  nas <- is.na(z)
  sorted <- sort(z, index.return = TRUE, na.last = NA, method = "quick")
  cum <- cumsum(sorted$x)
  z2 <- rep(NA, length(z))
  z2[!nas][sorted$ix] <- cum/max(cum)*100

  values(x)=z2
  return(x)
}

HSMap_cumulative <- scaledCumsum(HSMap)
plot(HSMap_cumulative)
outfile = "output/Bias/Subsample_2.5km/HSMap_cumulative_2.5kmbias.tif"
writeRaster(HSMap_cumulative, filename=outfile, overwrite=TRUE, format="GTiff", datatype="FLT4S")

# 
# ########## 7: 5km subsample, no bias file ################
# 
# model <- maxent(x = predictors, p = subs_5km, removeDuplicates=TRUE,
#                 path="output/NoBias/Subsample_5km",
#                 args = c("-P", "-J", "replicates=100", "replicatetype=Crossvalidate",
#                          "randomseed", "nooutputgrids",
#                          "maximumiterations=5000",
#                          "betamultiplier=1"))
# 
# map <- predict(model, predictors, progress='text', args=c("outputformat=raw"))
# #plot(map)
# HSMap <- mean(map)
# plot(HSMap)
# points(pts[inside.StudyArea, ], pch=20, col="black", cex=0.5)
# 
# scaledCumsum <- function(x){
#   z <- values(x)
#   nas <- is.na(z)
#   sorted <- sort(z, index.return = TRUE, na.last = NA, method = "quick")
#   cum <- cumsum(sorted$x)
#   z2 <- rep(NA, length(z))
#   z2[!nas][sorted$ix] <- cum/max(cum)*100
# 
#   values(x)=z2
#   return(x)
# }
# 
# HSMap_cumulative <- scaledCumsum(HSMap)
# plot(HSMap_cumulative)
# outfile = "output/NoBias/Subsample_5km/HSMap_cumulative_5kmnobias.tif"
# writeRaster(HSMap_cumulative, filename=outfile, overwrite=TRUE, format="GTiff", datatype="FLT4S")

########## 8: 5km subsample, with bias file ################
model <- maxent(x = predictors, p = subs_5km, removeDuplicates=TRUE, a = dat$bias,
                path="output/Bias/Subsample_5km",    
                args = c("-P", "-J", "replicates=100", "replicatetype=Crossvalidate",
                         "randomseed", "nooutputgrids",
                         "maximumiterations=5000", 
                         "betamultiplier=1"))

map <- predict(model, predictors, progress='text', args=c("outputformat=raw"))
#plot(map)
HSMap <- mean(map)
#plot(HSMap)
#points(pts[inside.StudyArea, ], pch=20, col="black", cex=0.5)

scaledCumsum <- function(x){
  z <- values(x)
  nas <- is.na(z)
  sorted <- sort(z, index.return = TRUE, na.last = NA, method = "quick")
  cum <- cumsum(sorted$x)
  z2 <- rep(NA, length(z))
  z2[!nas][sorted$ix] <- cum/max(cum)*100
  
  values(x)=z2
  return(x)
}

HSMap_cumulative <- scaledCumsum(HSMap)
#plot(HSMap_cumulative)
outfile = "output/Bias/Subsample_5km/HSMap_cumulative_5kmbias.tif"
writeRaster(HSMap_cumulative, filename=outfile, overwrite=TRUE, format="GTiff", datatype="FLT4S")

#********************************************************************************************************************

#testing
# background data
bg <- randomPoints(predictors, 1000)
#simplest way to use 'evaluate'
e1 <- evaluate(model, p=dat$target, a=dat$bias, x=predictors)
# alternative 1
# extract values
pvtest <- data.frame(extract(predictors, occtest))
avtest <- data.frame(extract(predictors, bg))
e2 <- evaluate(model, p=pvtest, a=avtest)




