###################################
#October 2015
#C Gomez

#C O L L I N E A R I T Y     
#All environmental layers were processed to have the same geographic extent, 
# projection system (World Geodetic System 1984), and cell size (1.5 km), 
# and converted to an ASCII raster grid format using ArcGIS 10. 
# The variance inflation factor (VIF, Zuur et al. 2010) was used to investigate collinearity
# between the environmental variables. Note that the average regional chlorophyll-a magnitude
# was not included in the VIF calculations as it is comprised of only one (average) 
# value per unique region (Figure 3). VIF values less than three denote that the 
# environmental variables do not exhibit collinearity and thus are relevant to use in the SDM.

setwd("D:/RProjects/SDM_notShared")
# Import libraries
library(raster)
library(rgdal)
options(java.parameters = "-Xmx1g" )
library(rJava)
library(maptools)
source('D:/GIS/Data/HighstatLibV7.R')
library(RColorBrewer) 
P4S.latlon <- CRS("+proj=longlat +datum=WGS84")
col <- brewer.pal(9, 'NBW')
library(spcosa) # for random sampling - not used here
require(maps)
require(mapdata)
require(dismo)# dismo has the SDM analyses we"ll need
library(squash) #colours for maps
library(GISTools)
#ENMeval: Automated Runs and Evaluations of Ecological Niche Models
#see http://neondataskills.org/R/Raster-Data-In-R/ for tips on plotting in R

# Summer SDM <- Bathy + CTI + sst Summer + CHl summer + CHL spring
bathy <- raster("predictors/bathy.asc")
sst_summer <- raster("predictors/sst_summer.asc")
chl_concen_summer <- raster("predictors/chl_magn_summer.asc")
chl_pers_summer <- raster("predictors/chl_pers_summer.asc")
chl_concen_spring <- raster("predictors/chl_magn_spring.asc")
chl_pers_spring <- raster("predictors/chl_pers_spring.asc")
tci <- raster("predictors/tci_modified.asc")

predictors <- stack(bathy, tci, sst_summer, chl_pers_summer, chl_pers_spring, chl_concen_summer, chl_concen_spring)
predictors <- setMinMax(predictors)
predictors

plot(predictors)
NBW <- read.csv("D:/GIS/Data/MarMammData/NBottlenoseWhale/NBW_Summer.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
Blues <- read.csv("D:/GIS/Data/MarMammData/BlueWhales/Blues_Summer_postwhaling.csv",header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
Whales <- rbind(NBW, Blues)
Whales <- Whales[,c('Longitude', 'Latitude')]
presvals <- extract(predictors, Whales) 
str(presvals)
presvals <- presvals[complete.cases(presvals),] # The 'complete.cases' command returns only those rows in the data frame that have non-NA values for all columns.
set.seed(0) # setting random seed to always create the same
backgr <- randomPoints(predictors, 5000)
absvals <- extract(predictors, backgr)
pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
sdmdata <- data.frame(cbind(pb, rbind(presvals, absvals)))
sdmdata[,'biome'] = as.factor(sdmdata[,'biome'])
head(sdmdata)
summary(sdmdata)
pairs(sdmdata[,2:8], cex=0.1, fig=TRUE)
corvif(sdmdata[,2:6]) # --> Zuur et al. 2010 variance inflation factor (VIF); # Investigate colinearity in the environmental data (at the presence and background points)
# VIF less than 3: not collineariity. If VIF > 3 (or another preselected threshold), 
# sequentially drop the covariate with the highest VIF,
# recalculate the VIFs and repeat this process until all 
# VIFs are smaller than a preselected threshold. 

#*********************************************************************
# Fall SDM <- Bathy + CTI + sst Fall + CHl summer + CHL fall
bathy <- raster("D:/GIS/Data/LayersForMaxEnt/ASCCI_for_Maxent/bathy.asc")
sst_fall <- raster("D:/GIS/Data/LayersForMaxEnt/ASCCI_for_Maxent/sst_fall.asc")
chl_pers_fall <- raster("D:/GIS/Data/LayersForMaxEnt/ASCCI_for_Maxent/chl_pers_fall.asc")
chl_pers_summer <- raster("D:/GIS/Data/LayersForMaxEnt/ASCCI_for_Maxent/Chl_pers_summer.asc")
tci <- raster("D:/GIS/Data/LayersForMaxEnt/ASCCI_for_Maxent/tci_modified.asc")

predictors <- stack(bathy, tci, sst_fall, chl_pers_fall, chl_pers_spring)
predictors <- setMinMax(predictors)
predictors
plot(predictors)
NBW <- read.csv("D:/GIS/Data/MarMammData/NBottlenoseWhale/NBW_Summer.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
Blues <- read.csv("D:/GIS/Data/MarMammData/BlueWhales/Blues_Summer_postwhaling.csv",header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
Whales <- rbind(NBW, Blues)
Whales <- Whales[,c('Longitude', 'Latitude')]
presvals <- extract(predictors, Whales) 
str(presvals)
presvals <- presvals[complete.cases(presvals),] # The 'complete.cases' command returns only those rows in the data frame that have non-NA values for all columns.
set.seed(0) # setting random seed to always create the same
backgr <- randomPoints(predictors, 5000)
absvals <- extract(predictors, backgr)
pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
sdmdata <- data.frame(cbind(pb, rbind(presvals, absvals)))
sdmdata[,'biome'] = as.factor(sdmdata[,'biome'])
head(sdmdata)
summary(sdmdata)
pairs(sdmdata[,2:8], cex=0.1, fig=TRUE)
corvif(sdmdata[,2:6]) # --> Zuur et al. 2010 variance inflation factor (VIF); # Investigate colinearity in the environmental data (at the presence and background points)
# VIF less than 3: not collineariity. If VIF > 3 (or another preselected threshold), 
# sequentially drop the covariate with the highest VIF,
# recalculate the VIFs and repeat this process until all 
# VIFs are smaller than a preselected threshold. 
