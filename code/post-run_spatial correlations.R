#June 2016
#C Gomez

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
library(spcosa) # for random sampling - not used here
require(maps)
require(mapdata)
require(dismo)# dismo has the SDM analyses we"ll need
library(squash) #colours for maps
library(GISTools)

#################################################################################################################
#Analysis of MaxEnt Results
#################################################################################################################
#  B O T T L E N O S E      W H A L E S 
#   Analysis of spatial correlation between maps obtained using different settings          
################################################################################################################

#Bias1km
NBW_bias1km_nosub <- raster("outputs/BottlenoseWhales/Bias1km/No_subsample/HSMap_cumulative_bias1km&nosub.tif")
NBW_bias1km_sub1km <- raster("outputs/BottlenoseWhales/Bias1km/Subsample_1km/HSMap_cumulative_bias1km&sub1km.tif")
NBW_bias1km_sub2.5km <- raster("outputs/BottlenoseWhales/Bias1km/Subsample_2.5km/HSMap_cumulative_bias1km&sub2.5km.tif")
NBW_bias1km_sub5km <- raster("outputs/BottlenoseWhales/Bias1km/Subsample_5km/HSMap_cumulative_bias1km&sub5km.tif")

crs(NBW_bias1km_nosub) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
crs(NBW_bias1km_sub1km) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
crs(NBW_bias1km_sub2.5km) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
crs(NBW_bias1km_sub5km) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#Bias2.5km
NBW_bias2.5km_nosub <- raster("outputs/BottlenoseWhales/Bias2.5km/No_subsample/HSMap_cumulative_bias2.5km&nosub.tif")
NBW_bias2.5km_sub1km <- raster("outputs/BottlenoseWhales/Bias2.5km/Subsample_1km/HSMap_cumulative_bias2.5km&sub1km.tif")
NBW_bias2.5km_sub2.5km <- raster("outputs/BottlenoseWhales/Bias2.5km/Subsample_2.5km/HSMap_cumulative_bias2.5km&sub2.5km.tif")
NBW_bias2.5km_sub5km <- raster("outputs/BottlenoseWhales/Bias2.5km/Subsample_5km/HSMap_cumulative_bias2.5km&sub5km.tif")

crs(NBW_bias2.5km_nosub) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
crs(NBW_bias2.5km_sub1km) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
crs(NBW_bias2.5km_sub2.5km) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
crs(NBW_bias2.5km_sub5km) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 

#Bias5km
NBW_bias5km_nosub <- raster("outputs/BottlenoseWhales/Bias5km/No_subsample/HSMap_cumulative_bias5km&nosub.tif")
NBW_bias5km_sub1km <- raster("outputs/BottlenoseWhales/Bias5km/Subsample_1km/HSMap_cumulative_bias5km&sub1km.tif")
NBW_bias5km_sub2.5km <- raster("outputs/BottlenoseWhales/Bias5km/Subsample_2.5km/HSMap_cumulative_bias5km&sub2.5km.tif")
NBW_bias5km_sub5km <- raster("outputs/BottlenoseWhales/Bias5km/Subsample_5km/HSMap_cumulative_bias5km&sub5km.tif")

crs(NBW_bias5km_nosub) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
crs(NBW_bias5km_sub1km) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
crs(NBW_bias5km_sub2.5km) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
crs(NBW_bias5km_sub5km) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 

#NoBias
NBW_nobias_nosub <- raster("outputs/BottlenoseWhales/NoBias/No_subsample/HSMap_cumulative_nobias.tif")
NBW_nobias_sub1km <- raster("outputs/BottlenoseWhales/NoBias/Subsample_1km/HSMap_cumulative_1kmnobias.tif")
NBW_nobias_sub2.5km <- raster("outputs/BottlenoseWhales/NoBias/Subsample_2.5km/HSMap_cumulative_2.5kmnobias.tif")
NBW_nobias_sub5km <- raster("outputs/BottlenoseWhales/NoBias/Subsample_5km/HSMap_cumulative_5kmnobias.tif")

crs(NBW_nobias_nosub) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
crs(NBW_nobias_sub1km) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
crs(NBW_nobias_sub2.5km) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
crs(NBW_nobias_sub5km) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 


NBW_HSmaps <- stack(NBW_bias1km_nosub, NBW_bias1km_sub1km, NBW_bias1km_sub2.5km, NBW_bias1km_sub5km, 
                    NBW_bias2.5km_nosub, NBW_bias2.5km_sub1km, NBW_bias2.5km_sub2.5km, NBW_bias2.5km_sub5km,
                    NBW_bias5km_nosub, NBW_bias5km_sub1km, NBW_bias5km_sub2.5km, NBW_bias5km_sub5km,
                    NBW_nobias_nosub, NBW_nobias_sub1km, NBW_nobias_sub2.5km, NBW_nobias_sub5km)

NBW_HSmaps <- setMinMax(NBW_HSmaps) #Defining Min/Max Values, by default the raster doesn't have the min / max values associated with it's attributes.
plot(NBW_HSmaps)

Covariance <- layerStats(NBW_HSmaps, 'cov', na.rm=TRUE) 
write.csv(Covariance, "outputs/BottlenoseWhales/Covariance.csv")
PearsonCorrelation <- layerStats(NBW_HSmaps, 'pearson', na.rm=TRUE) 
write.csv(PearsonCorrelation, "outputs/BottlenoseWhales/PearsonCorrelation.csv")

#####Plot all maps
#display.brewer.all()
col=jet(5)
brk <- c(0, 20, 40, 60, 100)
plot(NBW, col=col, breaks=brk, main="Bottlenose Whales")
plot(NBW_HSmaps, col=col, breaks=brk, main="Bottlenose Whales")
# legend( par()$usr[2], 44,
#         legend = c("Very high", "High", "Medium", "Low", "Very low"), 
#         fill = col)

#plot (NBW, zlim=c(40,60), add = TRUE)


#################################################################################################################
#  B L U E     W H A L E S            
#################################################################################################################

#Bias1km
BlueW_bias1km_nosub <- raster("outputs/BlueWhales/Bias1km/No_subsample/HSMap_cumulative_bias1km&nosub.tif")
BlueW_bias1km_sub1km <- raster("outputs/BlueWhales/Bias1km/Subsample_1km/HSMap_cumulative_bias1km&sub1km.tif")
BlueW_bias1km_sub2.5km <- raster("outputs/BlueWhales/Bias1km/Subsample_2.5km/HSMap_cumulative_bias1km&sub2.5km.tif")
BlueW_bias1km_sub5km <- raster("outputs/BlueWhales/Bias1km/Subsample_5km/HSMap_cumulative_bias1km&sub5km.tif")

crs(BlueW_bias1km_nosub) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
crs(BlueW_bias1km_sub1km) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
crs(BlueW_bias1km_sub2.5km) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
crs(BlueW_bias1km_sub5km) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#Bias2.5km
BlueW_bias2.5km_nosub <- raster("outputs/BlueWhales/Bias2.5km/No_subsample/HSMap_cumulative_bias2.5km&nosub.tif")
BlueW_bias2.5km_sub1km <- raster("outputs/BlueWhales/Bias2.5km/Subsample_1km/HSMap_cumulative_bias2.5km&sub1km.tif")
BlueW_bias2.5km_sub2.5km <- raster("outputs/BlueWhales/Bias2.5km/Subsample_2.5km/HSMap_cumulative_bias2.5km&sub2.5km.tif")
BlueW_bias2.5km_sub5km <- raster("outputs/BlueWhales/Bias2.5km/Subsample_5km/HSMap_cumulative_bias2.5km&sub5km.tif")

crs(BlueW_bias2.5km_nosub) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
crs(BlueW_bias2.5km_sub1km) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
crs(BlueW_bias2.5km_sub2.5km) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
crs(BlueW_bias2.5km_sub5km) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 

#Bias5km
BlueW_bias5km_nosub <- raster("outputs/BlueWhales/Bias5km/No_subsample/HSMap_cumulative_bias5km&nosub.tif")
BlueW_bias5km_sub1km <- raster("outputs/BlueWhales/Bias5km/Subsample_1km/HSMap_cumulative_bias5km&sub1km.tif")
BlueW_bias5km_sub2.5km <- raster("outputs/BlueWhales/Bias5km/Subsample_2.5km/HSMap_cumulative_bias5km&sub2.5km.tif")
BlueW_bias5km_sub5km <- raster("outputs/BlueWhales/Bias5km/Subsample_5km/HSMap_cumulative_bias5km&sub5km.tif")

crs(BlueW_bias5km_nosub) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
crs(BlueW_bias5km_sub1km) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
crs(BlueW_bias5km_sub2.5km) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
crs(BlueW_bias5km_sub5km) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 

#NoBias
BlueW_nobias_nosub <- raster("outputs/BlueWhales/NoBias/No_subsample/HSMap_cumulative_nobias.tif")
BlueW_nobias_sub1km <- raster("outputs/BlueWhales/NoBias/Subsample_1km/HSMap_cumulative_1kmnobias.tif")
BlueW_nobias_sub2.5km <- raster("outputs/BlueWhales/NoBias/Subsample_2.5km/HSMap_cumulative_2.5kmnobias.tif")
BlueW_nobias_sub5km <- raster("outputs/BlueWhales/NoBias/Subsample_5km/HSMap_cumulative_5kmnobias.tif")

crs(BlueW_nobias_nosub) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
crs(BlueW_nobias_sub1km) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
crs(BlueW_nobias_sub2.5km) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
crs(BlueW_nobias_sub5km) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 


BlueW_HSmaps <- stack(BlueW_bias1km_nosub, BlueW_bias1km_sub1km, BlueW_bias1km_sub2.5km, BlueW_bias1km_sub5km, 
                      BlueW_bias2.5km_nosub, BlueW_bias2.5km_sub1km, BlueW_bias2.5km_sub2.5km, BlueW_bias2.5km_sub5km,
                      BlueW_bias5km_nosub, BlueW_bias5km_sub1km, BlueW_bias5km_sub2.5km, BlueW_bias5km_sub5km,
                      BlueW_nobias_nosub, BlueW_nobias_sub1km, BlueW_nobias_sub2.5km, BlueW_nobias_sub5km)

BlueW_HSmaps <- setMinMax(BlueW_HSmaps) #Defining Min/Max Values, by default the raster doesn't have the min / max values associated with it's attributes.
plot(BlueW_HSmaps)
Covariance <- layerStats(BlueW_HSmaps, 'cov', na.rm=TRUE) 
write.csv(Covariance, "outputs/BlueWhales/Covariance.csv")
PearsonCorrelation <- layerStats(BlueW_HSmaps, 'pearson', na.rm=TRUE) 
write.csv(PearsonCorrelation, "outputs/BlueWhales/PearsonCorrelation.csv")

#####Plot all maps
#display.brewer.all()
col=jet(5)
brk <- c(0, 20, 40, 60, 100)
plot(BlueW, col=col, breaks=brk, main="Bottlenose Whales")
plot(BlueW_HSmaps, col=col, breaks=brk, main="Bottlenose Whales")
# legend( par()$usr[2], 44,
#         legend = c("Very high", "High", "Medium", "Low", "Very low"), 
#         fill = col)

#plot (BlueW, zlim=c(40,60), add = TRUE)