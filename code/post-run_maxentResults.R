#June 2016
#C Gomez

setwd("D:/RProjects/SDM_notShared")
library(plyr)
# # Import libraries
# library(read.csv)
# library(rgdal)
# options(java.parameters = "-Xmx1g" )
# library(rJava)
# library(maptools)
# source('D:/GIS/Data/HighstatLibV7.R')
# library(RColorBrewer) 
# P4S.latlon <- CRS("+proj=longlat +datum=WGS84")
# library(spcosa) # for random sampling - not used here
# require(maps)
# require(mapdata)
# require(dismo)# dismo has the SDM analyses we"ll need
# library(squash) #colours for maps
# library(GISTools)

#################################################################################################################
#Summary of MaxEnt Results
#################################################################################################################
#  B O T T L E N O S E      W H A L E S 
#  AUC values        
################################################################################################################

#Bias1km
NBW_bias1km_nosub <- read.csv("outputs/BottlenoseWhales/Bias1km/No_subsample/maxentResults.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
NBW_bias1km_nosub$ID <- "NBW_bias1km_nosub"
NBW_bias1km_sub1km <- read.csv("outputs/BottlenoseWhales/Bias1km/Subsample_1km/maxentResults.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
NBW_bias1km_sub1km$ID <- "NBW_bias1km_sub1km"
NBW_bias1km_sub2.5km <- read.csv("outputs/BottlenoseWhales/Bias1km/Subsample_2.5km/maxentResults.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
NBW_bias1km_sub2.5km$ID <- "NBW_bias1km_sub2.5km"
NBW_bias1km_sub5km <- read.csv("outputs/BottlenoseWhales/Bias1km/Subsample_5km/maxentResults.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
NBW_bias1km_sub5km$ID <- "NBW_bias1km_sub5km"

#Bias2.5km
NBW_bias2.5km_nosub <- read.csv("outputs/BottlenoseWhales/Bias2.5km/No_subsample/maxentResults.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
NBW_bias2.5km_nosub$ID <- "NBW_bias2.5km_nosub"
NBW_bias2.5km_sub1km <- read.csv("outputs/BottlenoseWhales/Bias2.5km/Subsample_1km/maxentResults.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
NBW_bias2.5km_sub1km$ID <- "NBW_bias2.5km_sub1km"
NBW_bias2.5km_sub2.5km <- read.csv("outputs/BottlenoseWhales/Bias2.5km/Subsample_2.5km/maxentResults.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
NBW_bias2.5km_sub2.5km$ID <- "NBW_bias2.5km_sub2.5km"
NBW_bias2.5km_sub5km <- read.csv("outputs/BottlenoseWhales/Bias2.5km/Subsample_5km/maxentResults.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
NBW_bias2.5km_sub5km$ID <- "NBW_bias2.5km_sub5km"

#Bias5km
NBW_bias5km_nosub <- read.csv("outputs/BottlenoseWhales/Bias5km/No_subsample/maxentResults.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
NBW_bias5km_nosub$ID <- "NBW_bias5km_nosub"
NBW_bias5km_sub1km <- read.csv("outputs/BottlenoseWhales/Bias5km/Subsample_1km/maxentResults.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
NBW_bias5km_sub1km$ID <- "NBW_bias5km_sub1km"
NBW_bias5km_sub2.5km <- read.csv("outputs/BottlenoseWhales/Bias5km/Subsample_2.5km/maxentResults.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
NBW_bias5km_sub2.5km$ID <- "NBW_bias5km_sub2.5km"
NBW_bias5km_sub5km <- read.csv("outputs/BottlenoseWhales/Bias5km/Subsample_5km/maxentResults.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
NBW_bias5km_sub5km$ID <- "NBW_bias5km_sub5km"

#NoBias
NBW_nobias_nosub <- read.csv("outputs/BottlenoseWhales/NoBias/No_subsample/maxentResults.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
NBW_nobias_nosub$ID <- "NBW_nobias_nosub"
NBW_nobias_sub1km <- read.csv("outputs/BottlenoseWhales/NoBias/Subsample_1km/maxentResults.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
NBW_nobias_sub1km$ID <- "NBW_nobias_sub1km"
NBW_nobias_sub2.5km <- read.csv("outputs/BottlenoseWhales/NoBias/Subsample_2.5km/maxentResults.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
NBW_nobias_sub2.5km$ID <- "NBW_nobias_sub2.5km"
NBW_nobias_sub5km <- read.csv("outputs/BottlenoseWhales/NoBias/Subsample_5km/maxentResults.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
NBW_nobias_sub5km$ID <- "NBW_nobias_sub5km"

NBW_HS_MaxentResults <- rbind(NBW_bias1km_nosub, NBW_bias1km_sub1km, NBW_bias1km_sub2.5km, NBW_bias1km_sub5km, 
                    NBW_bias2.5km_nosub, NBW_bias2.5km_sub1km, NBW_bias2.5km_sub2.5km, NBW_bias2.5km_sub5km,
                    NBW_bias5km_nosub, NBW_bias5km_sub1km, NBW_bias5km_sub2.5km, NBW_bias5km_sub5km,
                    NBW_nobias_nosub, NBW_nobias_sub1km, NBW_nobias_sub2.5km, NBW_nobias_sub5km)

#NBW_HS_MaxentResults <- as.data.frame(NBW_HS_MaxentResults)
NBW_HS_MaxentResults <- NBW_HS_MaxentResults[,c('ID', 'Species', 'Test.AUC', 'AUC.Standard.Deviation', 'bathy.contribution', 'chl_magn_spring.contribution', 'chl_magn_summer.contribution', 'chl_pers_spring.contribution', 'chl_pers_summer.contribution', 'sst_summer.contribution', 'tci_modified.contribution')] 
NBW_HS_MaxentResults <- NBW_HS_MaxentResults[NBW_HS_MaxentResults$Species ==c ('species (average)'),]
write.csv(NBW_HS_MaxentResults, "outputs/NBW_HS_MaxentResults.csv")


#################################################################################################################
#  B L U E     W H A L E S            
#################################################################################################################

#Bias1km
BlueW_bias1km_nosub <- read.csv("outputs/BlueWhales/Bias1km/No_subsample/maxentResults.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
BlueW_bias1km_nosub$ID <- "BlueW_bias1km_nosub"
BlueW_bias1km_sub1km <- read.csv("outputs/BlueWhales/Bias1km/Subsample_1km/maxentResults.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
BlueW_bias1km_sub1km$ID <- "BlueW_bias1km_sub1km"
BlueW_bias1km_sub2.5km <- read.csv("outputs/BlueWhales/Bias1km/Subsample_2.5km/maxentResults.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
BlueW_bias1km_sub2.5km$ID <- "BlueW_bias1km_sub2.5km"
BlueW_bias1km_sub5km <- read.csv("outputs/BlueWhales/Bias1km/Subsample_5km/maxentResults.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
BlueW_bias1km_sub5km$ID <- "BlueW_bias1km_sub5km"

#Bias2.5km
BlueW_bias2.5km_nosub <- read.csv("outputs/BlueWhales/Bias2.5km/No_subsample/maxentResults.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
BlueW_bias2.5km_nosub$ID <- "BlueW_bias2.5km_nosub"
BlueW_bias2.5km_sub1km <- read.csv("outputs/BlueWhales/Bias2.5km/Subsample_1km/maxentResults.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
BlueW_bias2.5km_sub1km$ID <- "BlueW_bias2.5km_sub1km"
BlueW_bias2.5km_sub2.5km <- read.csv("outputs/BlueWhales/Bias2.5km/Subsample_2.5km/maxentResults.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
BlueW_bias2.5km_sub2.5km$ID <- "BlueW_bias2.5km_sub2.5km"
BlueW_bias2.5km_sub5km <- read.csv("outputs/BlueWhales/Bias2.5km/Subsample_5km/maxentResults.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
BlueW_bias2.5km_sub5km$ID <- "BlueW_bias2.5km_sub5km"

#Bias5km
BlueW_bias5km_nosub <- read.csv("outputs/BlueWhales/Bias5km/No_subsample/maxentResults.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
BlueW_bias5km_nosub$ID <- "BlueW_bias5km_nosub"
BlueW_bias5km_sub1km <- read.csv("outputs/BlueWhales/Bias5km/Subsample_1km/maxentResults.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
BlueW_bias5km_sub1km$ID <- "BlueW_bias5km_sub1km"
BlueW_bias5km_sub2.5km <- read.csv("outputs/BlueWhales/Bias5km/Subsample_2.5km/maxentResults.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
BlueW_bias5km_sub2.5km$ID <- "BlueW_bias5km_sub2.5km"
BlueW_bias5km_sub5km <- read.csv("outputs/BlueWhales/Bias5km/Subsample_5km/maxentResults.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
BlueW_bias5km_sub5km$ID <- "BlueW_bias5km_sub5km"

#NoBias
BlueW_nobias_nosub <- read.csv("outputs/BlueWhales/NoBias/No_subsample/maxentResults.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
BlueW_nobias_nosub$ID <- "BlueW_nobias_nosub"
BlueW_nobias_sub1km <- read.csv("outputs/BlueWhales/NoBias/Subsample_1km/maxentResults.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
BlueW_nobias_sub1km$ID <- "BlueW_nobias_sub1km"
BlueW_nobias_sub2.5km <- read.csv("outputs/BlueWhales/NoBias/Subsample_2.5km/maxentResults.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
BlueW_nobias_sub2.5km$ID <- "BlueW_nobias_sub2.5km"
BlueW_nobias_sub5km <- read.csv("outputs/BlueWhales/NoBias/Subsample_5km/maxentResults.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
BlueW_nobias_sub5km$ID <- "BlueW_nobias_sub5km"

BlueW_HS_MaxentResults <- rbind(BlueW_bias1km_nosub, BlueW_bias1km_sub1km, BlueW_bias1km_sub2.5km, BlueW_bias1km_sub5km, 
                                BlueW_bias2.5km_nosub, BlueW_bias2.5km_sub1km, BlueW_bias2.5km_sub2.5km, BlueW_bias2.5km_sub5km,
                                BlueW_bias5km_nosub, BlueW_bias5km_sub1km, BlueW_bias5km_sub2.5km, BlueW_bias5km_sub5km,
                                BlueW_nobias_nosub, BlueW_nobias_sub1km, BlueW_nobias_sub2.5km, BlueW_nobias_sub5km)

#BlueW_HS_MaxentResults <- as.data.frame(BlueW_HS_MaxentResults)
BlueW_HS_MaxentResults <- BlueW_HS_MaxentResults[,c('ID', 'Species', 'Test.AUC', 'AUC.Standard.Deviation', 'bathy.contribution', 'chl_magn_spring.contribution', 'chl_magn_summer.contribution', 'chl_pers_spring.contribution', 'chl_pers_summer.contribution', 'sst_summer.contribution', 'tci_modified.contribution')] 
BlueW_HS_MaxentResults <- BlueW_HS_MaxentResults[BlueW_HS_MaxentResults$Species ==c ('species (average)'),]
write.csv(BlueW_HS_MaxentResults, "outputs/BlueW_HS_MaxentResults.csv")
