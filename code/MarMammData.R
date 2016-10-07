#Read and clean marine mammal data 
#By Catalina Gomez, Amy-Lee Kouwenberg and Emma Marotte
#Last updated: February 2016
library(plyr)
library(raster)
library(rgdal)
options(java.parameters = "-Xmx1g" )
library(dismo)
library(rJava)
library(maptools)
library(RColorBrewer) 
require(maps)
require(sp)
require(rgdal)
library(lubridate)
ymd("20110604"); mdy("06-04-2011"); dmy("04/06/2011")

###**************************************************************************************************##
#####------------------------------------PART I: compile sightings------------------------------------#
# DATA 1: Maritimes Region (Provided by Pierre Clement - Includes CWS and Whitehead Lab sightings up to 2013) 
#setwd("D:/GIS/Data/MarMammData")
#Maritimes <- read.csv("D:/GIS/Data/MarMammData/Maritimes/CetaceanSightings.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)

#setwd("C:/Users/Owner/Documents/DFO Casual Position Sept-Mar 2015/SDM Cetaceans 2016/Cetacean Sightings/MarMammData")
#Maritimes <- read.csv("C:/Users/Owner/Documents/DFO Casual Position Sept-Mar 2015/SDM Cetaceans 2016/Cetacean Sightings/MarMammData/Maritimes/CetaceanSightings.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)

Maritimes$CommonName <- factor(Maritimes$CommonName) 
Maritimes$Species <- ifelse(Maritimes$CommonName=="DOLPHINS (NS)","Unk Dolphin",
                     ifelse(Maritimes$CommonName=="DOLPHINS-ATLANTIC BOTTLENOSE","Bottlenose Dolphin",
                     ifelse(Maritimes$CommonName=="DOLPHINS-COMMON","Common Dolphin",
                     ifelse(Maritimes$CommonName=="DOLPHIN-SPOTTED","Spotted Dolphin", 
                     ifelse(Maritimes$CommonName=="DOLPHINS-RISSOS","Risso's Dolphin", 
                     ifelse(Maritimes$CommonName=="DOLPHINS-STRIPED","Striped Dolphin", 
                     ifelse(Maritimes$CommonName=="DOLPHINS-WHITE-BEAKED","White-beaked Dolphin",
                     ifelse(Maritimes$CommonName=="DOLPHINS-WHITE-SIDED","White-sided Dolphin", 
                     ifelse(Maritimes$CommonName=="PORPOISE-HARBOUR","Harbour Porpoise", 
                     ifelse(Maritimes$CommonName=="PYGMY SPERM WHALE","Pygmy Sperm Whale", 
                     ifelse(Maritimes$CommonName=="WHALE- CUVIERS BEAKED","Cuvier's Beaked Whale", 
                     ifelse(Maritimes$CommonName=="WHALE- LONG-FINNED PILOT","Pilot Whale", 
                     ifelse(Maritimes$CommonName=="WHALE-ATLANTIC PILOT","Pilot Whale", 
                     ifelse(Maritimes$CommonName=="WHALE-BEAKED (NS)","Beaked Whale", 
                     ifelse(Maritimes$CommonName=="WHALE-BELUGA","Beluga", 
                     ifelse(Maritimes$CommonName=="WHALE-BLUE","Blue Whale", 
                     ifelse(Maritimes$CommonName=="WHALE-BOWHEAD","Bowhead Whale", 
                     ifelse(Maritimes$CommonName=="WHALE-FIN","Fin Whale", 
                     ifelse(Maritimes$CommonName=="WHALE-GREY","Grey Whale", 
                     ifelse(Maritimes$CommonName=="WHALE-HUMPBACK","Humpback Whale", 
                     ifelse(Maritimes$CommonName=="WHALE-KILLER","Killer Whale", 
                     ifelse(Maritimes$CommonName=="WHALE-MINKE","Minke Whale", 
                     ifelse(Maritimes$CommonName=="WHALE-NORTHERN BOTTLENOSE","Northern Bottlenose Whale", 
                     ifelse(Maritimes$CommonName=="WHALE-RIGHT","Right Whale", 
                     ifelse(Maritimes$CommonName=="WHALES (NS)","Unk Whale", 
                     ifelse(Maritimes$CommonName=="WHALE-SEI","Sei Whale", 
                     ifelse(Maritimes$CommonName=="WHALE-SOWERBYS BEAKED","Sowerby's Beaked Whale", 
                     ifelse(Maritimes$CommonName=="WHALE-SPERM","Sperm Whale", ""))))))))))))))))))))))))))))
Maritimes$Species <- mapvalues(Maritimes$Species, from = c("Unk Whale"), to = c("Unknown Whale"))
Maritimes$Species <- mapvalues(Maritimes$Species, from = c("Unk Dolphin"), to = c("Unknown Dolphin"))
table(Maritimes[,c('Species','Year')])
Maritimes <- Maritimes[Maritimes$DataSource !=c('CWS (observers)'),] # We got original data from Corina instead
Maritimes$ID_cgs <- ifelse(Maritimes$ID_cgs=="xx","xx", "DFO_Maritimes")
Maritimes <- Maritimes[,c('Species', 'Longitude', 'Latitude', 'Year', 'Month', 'Platform', 'ID_cgs')] 
dim(Maritimes)
table(Maritimes$Species)
#write.csv(Maritimes, "D:/GIS/Data/MarMammData/Maritimes/Maritimes.csv", row.names=FALSE)

# DATA 1.1: Maritimes Region (PROVIDED BY HILARY (INCLDES WHITEHEAD LAB SIGHTING OF BLUE AND SEI WHALES IN 
#                             2015 AND 2 RECORDS OF FLIGHTS FROM PLATFORMS OF OPPORTUNITY) 
Maritimes_2015 <- read.csv("D:/GIS/Data/MarMammData/Maritimes/Maritimes_2015/Blues_Maritimes_Hilary.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
Maritimes_2015 <- Maritimes_2015[,c('Species', 'Longitude', 'Latitude', 'Year', 'Month', 'Platform', 'ID_cgs')]
table(Maritimes_2015$Species)


#Data 2. EC-CWS (Environment Canada-Canadian Wildlife Service) or ECSAS (Eastern Canada Seabirds at Sea) provided by Carina Gjendrum on March 2016
CWS <- read.csv("D:/GIS/Data/MarMammData/CWS/CWS.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
CWS <- rename(CWS, c("Lat"="Latitude","Long"="Longitude", "English" = "Species")) # changes the names of the headings
CWS$ID_cgs <- "CWS" #creates a new column with a new ID
CWS$Species <- mapvalues(CWS$Species, from = c("Order: Whales and Dolphins"), to = c("Unknown Cetacean"))  # do this for each species
CWS$Species <- mapvalues(CWS$Species, from = c("Long-finned Pilot Whale (Blackfish)"), to = c("Long-finned Pilot Whale"))
CWS$Species <- mapvalues(CWS$Species, from = c("Bottle-nosed Dolphin"), to = c("Bottlenose Dolphin"))
CWS$Species <- mapvalues(CWS$Species, from = c("Family: Beaked Whales"), to = c("Unknown Beaked Whale"))
CWS$Species <- mapvalues(CWS$Species, from = c("Family: Dolphins"), to = c("Unknown Dolphin"))
CWS$Species <- mapvalues(CWS$Species, from = c("Family: Porpoise or Dolphin"), to = c("Unknown Porpoise or Dolphin"))
CWS$Species <- mapvalues(CWS$Species, from = c("Family: Porpoises"), to = c("Unknown Porpoise"))
CWS$Species <- mapvalues(CWS$Species, from = c("Family: Rorquals and Humpback Whales"), to = c("Unknown Rorqual Whale"))
CWS$Species <- mapvalues(CWS$Species, from = c("Genus: Pilot whales"), to = c("Pilot Whale"))
CWS$Species <- mapvalues(CWS$Species, from = c("Grampus, Risso's Dolphin"), to = c("Risso's Dolphin"))
CWS$Species <- mapvalues(CWS$Species, from = c("Pan-tropical Spotted Dolphin"), to = c("Pantropical Spotted Dolphin"))
CWS$Date = parse_date_time(CWS$Date, c("%d/%m/%y"))
CWS$Year=as.numeric (format(CWS$Date,"%Y"))
CWS$Month=as.numeric (format(CWS$Date,"%m"))
CWS <- CWS[,c('Species', 'Longitude', 'Latitude', 'Month', 'Year','Platform', 'ID_cgs')]
str(CWS)
table(CWS$Species)
#write.csv(CWS, "C:/Users/MarotteE/Desktop/HSM/CWS/CWS1.csv", row.names=FALSE)
#write.csv(CWS, row.names=FALSE)
row.names(CWS) <- NULL
table(CWS[,c('Platform','Year')])
table(CWS[,c('Species','Year')])


#Data 3: DFO_Newfoundland (cleaned and compiled by Amy-Lee Kouwenberg and Jack Lawson) 
Newfoundland <- read.csv("D:/GIS/Data/MarMammData/Newfoundland_Amy/NL Cetacean Sightings.csv", 
                         header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
Newfoundland <- rename(Newfoundland, c("Species.Name"="Species"))
Newfoundland$ID_cgs <- "DFO_Newfoundland"
Newfoundland <- Newfoundland[Newfoundland$Animal.Condition %in% c('1'), ]
Newfoundland <- Newfoundland[,c('Species', 'Longitude', 'Latitude', 'Year', 'Month', 'Platform', 'ID_cgs')] 
Newfoundland$Species <- mapvalues(Newfoundland$Species, from = c("Fin whale"), to = c("Fin Whale"))
Newfoundland$Species <- mapvalues(Newfoundland$Species, from = c("Atlantic White-Sided Dolphin"), to = c("Atlantic White-sided Dolphin"))
Newfoundland$Species <- mapvalues(Newfoundland$Species, from = c("Beluga Whale"), to = c("Beluga"))
Newfoundland$Species <- mapvalues(Newfoundland$Species, from = c("Blue whale"), to = c("Blue Whale"))
table(Newfoundland$Species)
#table(Newfoundland[,c('Species','Year')])


#Data 4: OBIS - Downloaded from OBIS on January 2016 by Emma Marotte
#BlueWhales_OBIS <- read.csv("D:/GIS/Data/MarMammData/OBIS/OBIS_sightings/Blue whale.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
setwd("D:/GIS/Data/MarMammData/OBIS/OBISsightings")
file_names <- dir("D:/GIS/Data/MarMammData/OBIS/OBISsightings") 
OBIS <- do.call(rbind,lapply(file_names, read.csv))
OBIS <- rename(OBIS, c("latitude"="Latitude","longitude"="Longitude", "tname" = "Species", "resname" = "Platform", "datecollected" = "date")) # chnages the names of the headings
OBIS$ID_cgs <- "OBIS" #creates a new column with a new ID

OBIS$Species <- mapvalues(OBIS$Species, from = c("Stenella frontalis"), to = c("Atlantic Spotted Dolphin"))  # do this for each species
OBIS$Species <- mapvalues(OBIS$Species, from = c("Lagenorhynchus acutus"), to = c("Atlantic White-sided Dolphin"))
OBIS$Species <- mapvalues(OBIS$Species, from = c("Hyperoodon ampullatus"), to = c("Northern Bottlenose Whale"))
OBIS$Species <- mapvalues(OBIS$Species, from = c("Mesoplodon europaeus"), to = c("Gervais' Beaked Whale"))
OBIS$Species <- mapvalues(OBIS$Species, from = c("Mesoplodon mirus"), to = c("True's Beaked Whale"))
OBIS$Species <- mapvalues(OBIS$Species, from = c("Ziphiidae"), to = c("Unknown Beaked Whale"))
OBIS$Species <- mapvalues(OBIS$Species, from = c("Mesoplodon"), to = c("Unknown Beaked Whale"))
OBIS$Species <- mapvalues(OBIS$Species, from = c("Delphinapterus leucas"), to = c("Beluga"))
OBIS$Species <- mapvalues(OBIS$Species, from = c("Mesoplodon densirostris"), to = c("Blainville's Beaked Whale"))
OBIS$Species <- mapvalues(OBIS$Species, from = c("Balaenoptera musculus"), to = c("Blue Whale"))
OBIS$Species <- mapvalues(OBIS$Species, from = c("Tursiops truncatus"), to = c("Bottlenose Dolphin"))
OBIS$Species <- mapvalues(OBIS$Species, from = c("Stenella attenuata"), to = c("Pantropical Spotted Dolphin"))
OBIS$Species <- mapvalues(OBIS$Species, from = c("Balaena mysticetus"), to = c("Bowhead Whale"))
OBIS$Species <- mapvalues(OBIS$Species, from = c("Delphinus delphis"), to = c("Common Dolphin"))
OBIS$Species <- mapvalues(OBIS$Species, from = c("Ziphius cavirostris"), to = c("Cuvier's Beaked Whale"))
OBIS$Species <- mapvalues(OBIS$Species, from = c("Pseudorca crassidens"), to = c("False Killer Whale"))
OBIS$Species <- mapvalues(OBIS$Species, from = c("Balaenoptera physalus"), to = c("Fin Whale"))
OBIS$Species <- mapvalues(OBIS$Species, from = c("Phocoena phocoena"), to = c("Harbour Porpoise"))
OBIS$Species <- mapvalues(OBIS$Species, from = c("Megaptera novaeangliae"), to = c("Humpback Whale"))
OBIS$Species <- mapvalues(OBIS$Species, from = c("Orcinus orca"), to = c("Killer Whale"))
OBIS$Species <- mapvalues(OBIS$Species, from = c("Globicephala melas"), to = c("Long-finned Pilot Whale"))
OBIS$Species <- mapvalues(OBIS$Species, from = c("Globicephala melaena"), to = c("Long-finned Pilot Whale"))
OBIS$Species <- mapvalues(OBIS$Species, from = c("Globicephala macrorhynchus"), to = c("Short-finned Pilot Whale"))
OBIS$Species <- mapvalues(OBIS$Species, from = c("Stenella longirostris"), to = c("Long-snouted Spinner Dolphin"))
OBIS$Species <- mapvalues(OBIS$Species, from = c("Balaenoptera acutorostrata"), to = c("Minke Whale"))
OBIS$Species <- mapvalues(OBIS$Species, from = c("Monodon monoceros"), to = c("Narwhal"))
OBIS$Species <- mapvalues(OBIS$Species, from = c("Eubalaena glacialis"), to = c("North Atlantic Right Whale"))
OBIS$Species <- mapvalues(OBIS$Species, from = c("Globicephala"), to = c("Pilot Whale"))
OBIS$Species <- mapvalues(OBIS$Species, from = c("Kogia breviceps"), to = c("Pygmy Sperm Whale"))
OBIS$Species <- mapvalues(OBIS$Species, from = c("Grampus griseus"), to = c("Risso's Dolphin"))
OBIS$Species <- mapvalues(OBIS$Species, from = c("Balaenoptera borealis"), to = c("Sei Whale"))
OBIS$Species <- mapvalues(OBIS$Species, from = c("Mesoplodon bidens"), to = c("Sowerby's Beaked Whale"))
OBIS$Species <- mapvalues(OBIS$Species, from = c("Physeter catodon"), to = c("Sperm Whale"))
OBIS$Species <- mapvalues(OBIS$Species, from = c("Physeter macrocephalus"), to = c("Sperm Whale"))
OBIS$Species <- mapvalues(OBIS$Species, from = c("Stenella coeruleoalba"), to = c("Striped Dolphin"))
OBIS$Species <- mapvalues(OBIS$Species, from = c("Lagenorhynchus albirostris"), to = c("White-beaked Dolphin"))

OBIS <- OBIS[OBIS$Platform !=c('A Biological Survey of the Waters of Woods Hole and Vacinity'),]
OBIS <- OBIS[OBIS$Platform !=c('AFAST Hatteras Aerial Survey -Left side- 2011-2012'),]
OBIS <- OBIS[OBIS$Platform !=c('AFAST Hatteras Aerial Survey -Right side- 2011-2012'),]
OBIS <- OBIS[OBIS$Platform !=c('AFAST Hatteras Aerial Survey -Right side- 2011-2012'),]
OBIS <- OBIS[OBIS$Platform !=c('AFTT Hatteras Aerial Survey -Left side- 2012-2013'),]
OBIS <- OBIS[OBIS$Platform !=c('AFTT Hatteras Aerial Survey -Right side- 2012-2013'),]
OBIS <- OBIS[OBIS$Platform !=c('Bottlenose dolphins off Outer Banks 2007-2012'),]
OBIS <- OBIS[OBIS$Platform !=c('Cape Hatteras 04-05'),]
OBIS <- OBIS[OBIS$Platform !=c('CRESLI marine mammal observations from whale watch cruises 2000-2011'),]
OBIS <- OBIS[OBIS$Platform !=c('CWS-EC Eastern Canada Seabirds at Sea (ECSAS)'),]
OBIS <- OBIS[OBIS$Platform !=c('DFO Maritimes Region Cetacean Sightings'),]
OBIS <- OBIS[OBIS$Platform !=c('DFO Quebec Region MLI museum collection'),]
OBIS <- OBIS[OBIS$Platform !=c('DUML surveys for the stock discrimination of bottlenose dolphins along the Outer Banks of North Carolina 2011-2012'),]
OBIS <- OBIS[OBIS$Platform !=c('DUML vessel-based photo-id and biopsy surveys in Onslow Bay CHPT OPAREA 2011-2013'),]
OBIS <- OBIS[OBIS$Platform !=c('DUML vessel-based photo-id and biopsy surveys in VACAPES OPAREA off Hatteras 2009, 2011-2013'),]
OBIS <- OBIS[OBIS$Platform !=c('Hatteras Eddy Cruise 2004'),]
OBIS <- OBIS[OBIS$Platform !=c('ICES contaminants and biological effects'),]
OBIS <- OBIS[OBIS$Platform !=c('MAR-ECO 2004 - Mammals and birds'),]
OBIS <- OBIS[OBIS$Platform !=c('MV Mammals'),]
OBIS <- OBIS[OBIS$Platform !=c("Mystic Aquarium's marine mammal and sea turtle stranding data 1976-2011"),]
OBIS <- OBIS[OBIS$Platform !=c('NEFSC 1995 AJ9501 (Part I)'),]
OBIS <- OBIS[OBIS$Platform !=c('NEFSC 1995 pe9501'),]
OBIS <- OBIS[OBIS$Platform !=c('NEFSC 1995 pe9502'),]
OBIS <- OBIS[OBIS$Platform !=c('NEFSC Deepwater Marine Mammal 2002'),]
OBIS <- OBIS[OBIS$Platform !=c('NEFSC Marine Mammal Abundance Cruise 2004 Passive Acoustic Monitoring - Rainbow Click Detections'),]
OBIS <- OBIS[OBIS$Platform !=c('NEFSC Mid-Atlantic Marine Mammal Abundance Survey 2004'),]
OBIS <- OBIS[OBIS$Platform !=c('NEFSC Survey 1997'),]
OBIS <- OBIS[OBIS$Platform !=c('NEFSC Survey 1998 1'),]
OBIS <- OBIS[OBIS$Platform !=c('NEFSC Survey 1998 2'),]
OBIS <- OBIS[OBIS$Platform !=c('Norfolk/VA Beach Inshore Vessel Surveys Nov 2012- Nov 2013'),]
OBIS <- OBIS[OBIS$Platform !=c('Norfolk/VA Beach MINEX Vessel Surveys'),]
OBIS <- OBIS[OBIS$Platform !=c('Opportunistic marine mammal sightings from commercial whale watching vessels, Montauk, New York 1981-1994'),]
OBIS <- OBIS[OBIS$Platform !=c('Sargasso sperm whales 2004'),]
OBIS <- OBIS[OBIS$Platform !=c('SEFSC Atlantic surveys 1992'),]
OBIS <- OBIS[OBIS$Platform !=c('SEFSC Atlantic surveys 1999'),]
OBIS <- OBIS[OBIS$Platform !=c('SEFSC Atlantic surveys, 1998 (3)'),]
OBIS <- OBIS[OBIS$Platform !=c('SEFSC Mid-Atlantic Tursiops Survey, 1995 (1)'),]
OBIS <- OBIS[OBIS$Platform !=c('SEFSC Mid-Atlantic Tursiops Survey, 1995 2'),]
OBIS <- OBIS[OBIS$Platform !=c('SEFSC Mid-Atlantic Tursiops Survey, 1995 3'),]
OBIS <- OBIS[OBIS$Platform !=c('SEFSC Southeast Cetacean Aerial Survey 1992'),]
OBIS <- OBIS[OBIS$Platform !=c('SEFSC Southeast Cetacean Aerial Survey 1995'),]
OBIS <- OBIS[OBIS$Platform !=c('Taxonomic Information System for the Belgian coastal area'),]
OBIS <- OBIS[OBIS$Platform !=c('UNCW Aerial Survey 1998-1999'),]
OBIS <- OBIS[OBIS$Platform !=c('UNCW Marine Mammal Aerial Surveys 2006-2007'),]
OBIS <- OBIS[OBIS$Platform !=c('UNCW Marine Mammal Sightings 1998-1999'),]
OBIS <- OBIS[OBIS$Platform !=c('UNCW Marine Mammal Sightings 2001'),]
OBIS <- OBIS[OBIS$Platform !=c('UNCW Marine Mammal Sightings 2002'),]
OBIS <- OBIS[OBIS$Platform !=c('UNCW Right Whale Aerial Surveys 2008'),]
OBIS <- OBIS[OBIS$Platform !=c('VACAPES ASWEX Aerial Monitoring 2011'),]
OBIS <- OBIS[OBIS$Platform !=c('VACAPES FIREX Aerial Monitoring 2011'),]
OBIS <- OBIS[OBIS$Platform !=c('VACAPES FIREX and ASW Aerial Monitoring 2010'),]
OBIS <- OBIS[OBIS$Platform !=c('VACAPES MISSELEX Aerial Monitoring March 2013'),]
OBIS <- OBIS[OBIS$Platform !=c('Virginia and Maryland Sea Turtle Research and Conservation Initiative Aerial Survey Sightings, May 2011 through July 2013'),]
OBIS <- OBIS[OBIS$Platform !=c('Virginia Aquarium Marine Mammal Strandings 1988-2008'),]
OBIS <- OBIS[OBIS$Platform !=c('Virginia CZM Wind Energy Area Survey- Left side - November 2012 through April 2014'),]
OBIS <- OBIS[OBIS$Platform !=c('Virginia CZM Wind Energy Area Survey- Right side - November 2012 through April 2014'),]
OBIS <- OBIS[OBIS$Platform !=c('Aerial survey of upper trophic level predators on PLatts Bank, Gulf of Maine'),]
OBIS <- OBIS[OBIS$Platform !=c('Atlantic Canada Conservation Data Centre'),]
OBIS <- OBIS[OBIS$Platform !=c('Bay of Fundy Species List'),]
OBIS <- OBIS[OBIS$Platform !=c('Deep Panuke whale Acoustic 2003'),]
OBIS <- OBIS[OBIS$Platform !=c('Duke Harbor Porpoise Tracking (aggregated per 1-degree cell)'),]
OBIS <- OBIS[OBIS$Platform !=c('Historical distribution of whales shown by logbook records 1785-1913'),]
OBIS <- OBIS[OBIS$Platform !=c('Long-Distance Movement of a Sei Whale in the North Atlantic, 2005 (aggregated per 1-degree cell)'),]
OBIS <- OBIS[OBIS$Platform !=c('PIROP Northwest Atlantic 1965-1992'),]
OBIS$date = parse_date_time(OBIS$date, c("%d/%m/%y", "%Y-%m-%d"))
OBIS$Year=as.numeric (format(OBIS$date,"%Y"))
OBIS$Month=as.numeric (format(OBIS$date,"%m"))
OBIS <- OBIS[,c('Species', 'Longitude', 'Latitude', 'Month', 'Year','Platform', 'ID_cgs')] 
row.names(OBIS) <- NULL
#write.csv(OBIS, "D:/GIS/Data/MarMammData/OBIS.csv", row.names=FALSE)

#Data 5: NARWC - Received April 6 2016 by Emma Marotte
setwd("D:/GIS/Data/MarMammData/NARWC")
NARWC_Sightings <- read.csv("D:/GIS/Data/MarMammData/NARWC/NARWC_Sightings.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
NARWC_Sightings <- rename(NARWC_Sightings, c("LATITUDE"="Latitude","LONGITUDE"="Longitude", "DEAD."="DEAD",
                                             "SPECCODE" = "Species", "MONTH" = "Month", "YEAR" = "Year", "DDSOURCE" = "Platform")) # changes the names of the headings
NARWC_Sightings <- NARWC_Sightings[NARWC_Sightings$DEAD ==c ('NO'),]
#NARWC_Sightings_DEAD <- NARWC_Sightings[NARWC_Sightings$DEAD ==c ('YES'),]
NARWC_Sightings$ID_cgs <- "NARWC" #creates a new column with a new ID

NARWC_Sightings$Species <- mapvalues(NARWC_Sightings$Species, from = c("BELU"), to = c("Beluga"))
NARWC_Sightings$Species <- mapvalues(NARWC_Sightings$Species, from = c("BLWH"), to = c("Blue Whale"))
NARWC_Sightings$Species <- mapvalues(NARWC_Sightings$Species, from = c("BEWH"), to = c("Unknown Beaked Whale"))
NARWC_Sightings$Species <- mapvalues(NARWC_Sightings$Species, from = c("BODO"), to = c("Bottlenose Dolphin"))
NARWC_Sightings$Species <- mapvalues(NARWC_Sightings$Species, from = c("BOWH"), to = c("Bowhead Whale"))
NARWC_Sightings$Species <- mapvalues(NARWC_Sightings$Species, from = c("FIWH"), to = c("Fin Whale"))
NARWC_Sightings$Species <- mapvalues(NARWC_Sightings$Species, from = c("FKWH"), to = c("False Killer Whale"))
NARWC_Sightings$Species <- mapvalues(NARWC_Sightings$Species, from = c("GEBW"), to = c("Gervais' Beaked Whale"))
NARWC_Sightings$Species <- mapvalues(NARWC_Sightings$Species, from = c("GOBW"), to = c("Cuvier's Beaked Whale"))
NARWC_Sightings$Species <- mapvalues(NARWC_Sightings$Species, from = c("GRAM"), to = c("Risso's Dolphin"))
NARWC_Sightings$Species <- mapvalues(NARWC_Sightings$Species, from = c("HAPO"), to = c("Harbour Porpoise"))
NARWC_Sightings$Species <- mapvalues(NARWC_Sightings$Species, from = c("HUWH"), to = c("Humpback Whale"))
NARWC_Sightings$Species <- mapvalues(NARWC_Sightings$Species, from = c("KIWH"), to = c("Killer Whale"))
NARWC_Sightings$Species <- mapvalues(NARWC_Sightings$Species, from = c("LFPW"), to = c("Long-finned Pilot Whale"))
NARWC_Sightings$Species <- mapvalues(NARWC_Sightings$Species, from = c("MIWH"), to = c("Minke Whale"))
NARWC_Sightings$Species <- mapvalues(NARWC_Sightings$Species, from = c("NBWH"), to = c("Northern Bottlenose Whale"))
NARWC_Sightings$Species <- mapvalues(NARWC_Sightings$Species, from = c("PIWH"), to = c("Pilot Whale"))
NARWC_Sightings$Species <- mapvalues(NARWC_Sightings$Species, from = c("RIWH"), to = c("North Atlantic Right Whale"))
NARWC_Sightings$Species <- mapvalues(NARWC_Sightings$Species, from = c("SADO"), to = c("Common Dolphin"))
NARWC_Sightings$Species <- mapvalues(NARWC_Sightings$Species, from = c("SEWH"), to = c("Sei Whale"))
NARWC_Sightings$Species <- mapvalues(NARWC_Sightings$Species, from = c("SOBW"), to = c("Sowerby's Beaked Whale"))
NARWC_Sightings$Species <- mapvalues(NARWC_Sightings$Species, from = c("SPDO"), to = c("Spotted Dolphin"))
NARWC_Sightings$Species <- mapvalues(NARWC_Sightings$Species, from = c("SPWH"), to = c("Sperm Whale"))
NARWC_Sightings$Species <- mapvalues(NARWC_Sightings$Species, from = c("STDO"), to = c("Striped Dolphin"))
NARWC_Sightings$Species <- mapvalues(NARWC_Sightings$Species, from = c("UNBA"), to = c("Unknown Rorqual Whale"))
NARWC_Sightings$Species <- mapvalues(NARWC_Sightings$Species, from = c("UNBF"), to = c("Unknown Blackfish"))
NARWC_Sightings$Species <- mapvalues(NARWC_Sightings$Species, from = c("UNBW"), to = c("Unknown Beaked Whale"))
NARWC_Sightings$Species <- mapvalues(NARWC_Sightings$Species, from = c("UNCW"), to = c("Common or White-sided Dolphin"))
NARWC_Sightings$Species <- mapvalues(NARWC_Sightings$Species, from = c("UNDO"), to = c("Unknown Porpoise or Dolphin"))
NARWC_Sightings$Species <- mapvalues(NARWC_Sightings$Species, from = c("UNFS"), to = c("Fin or Sei Whale"))
NARWC_Sightings$Species <- mapvalues(NARWC_Sightings$Species, from = c("UNGD"), to = c("Spotted or Bottlenose Dolphin"))
NARWC_Sightings$Species <- mapvalues(NARWC_Sightings$Species, from = c("UNKO"), to = c("Pygmy or Dwarf Sperm Whale"))
NARWC_Sightings$Species <- mapvalues(NARWC_Sightings$Species, from = c("UNLD"), to = c("Unknown Dolphin"))
NARWC_Sightings$Species <- mapvalues(NARWC_Sightings$Species, from = c("UNLW"), to = c("Unknown Large Whale"))
NARWC_Sightings$Species <- mapvalues(NARWC_Sightings$Species, from = c("UNMW"), to = c("Unknown Medium Whale"))
NARWC_Sightings$Species <- mapvalues(NARWC_Sightings$Species, from = c("UNRO"), to = c("Unknown Rorqual Whale"))
NARWC_Sightings$Species <- mapvalues(NARWC_Sightings$Species, from = c("UNST"), to = c("Unknown Dolphin"))
NARWC_Sightings$Species <- mapvalues(NARWC_Sightings$Species, from = c("UNWH"), to = c("Unknown Whale"))
NARWC_Sightings$Species <- mapvalues(NARWC_Sightings$Species, from = c("WBDO"), to = c("White-beaked Dolphin"))
NARWC_Sightings$Species <- mapvalues(NARWC_Sightings$Species, from = c("WSDO"), to = c("Atlantic White-sided Dolphin"))

NARWC_Sightings <- NARWC_Sightings[,c('Species', 'Longitude', 'Latitude', 'Month', 'Year','Platform', 'ID_cgs')]
str(NARWC_Sightings)
#write.csv(NARWC_Sightings, "D:/GIS/Data/MarMammData/NARWC/NARWC_Sightings1.csv", row.names=FALSE)


###**************************************************************************************************##
####------------------------------------PART II: Merge and clean sightings data------------------------------------#
MarMammSightings <- rbind(Maritimes, Maritimes_2015, Newfoundland, CWS, OBIS, NARWC_Sightings)
#row.names(MarMammSightings) <- NULL

MarMammSightings$Species <- mapvalues(MarMammSightings$Species, from = c("Blue whale"), to = c("Blue Whale"))
MarMammSightings$Species <- mapvalues(MarMammSightings$Species, from = c("Right Whale"), to = c("North Atlantic Right Whale"))
MarMammSightings$Species <- mapvalues(MarMammSightings$Species, from = c("Sperm whale"), to = c("Sperm Whale"))
MarMammSightings$Species <- mapvalues(MarMammSightings$Species, from = c("Beaked Whale"), to = c("Sperm Whale"))
MarMammSightings$Species <- mapvalues(MarMammSightings$Species, from = c("White-Beaked Dolphin"), to = c("White-beaked Dolphin"))
MarMammSightings$Species <- mapvalues(MarMammSightings$Species, from = c(" White-sided Dolphin"), to = c("Atlantic White-sided Dolphin"))
MarMammSightings$Species <- mapvalues(MarMammSightings$Species, from = c("Beluga Whale"), to = c("Beluga"))

table(MarMammSightings$Species)
#dups <- duplicated(MarMammSightings[, c('Longitude', 'Latitude', 'Species', 'Year', 'Month', 'Platform')]) # which records are duplicates 
dups <- duplicated(MarMammSightings[, c('Species', 'Year', 'Month', 'Longitude', 'Latitude')])
sum(dups)
#which(dups)
MarMammSightings <- MarMammSightings[!dups, ] # keeps only the records that are _NOT_ duplicated
dim(MarMammSightings)

MarMammSightings <- MarMammSightings[MarMammSightings$Year >= c( '1975'), ] #Remove everything that was before 1975 (whaling period) MaxEnt will only be for 1975 - 2015 sightings:
#Remove everything that is not a cetacean:
MarMammSightings <- MarMammSightings[MarMammSightings$Species !=c('Bearded Seal'),]
MarMammSightings <- MarMammSightings[MarMammSightings$Species !=c('Gray Seal'),]
MarMammSightings <- MarMammSightings[MarMammSightings$Species !=c('Grey Whale'),]
MarMammSightings <- MarMammSightings[MarMammSightings$Species !=c('Harp Seal'),]
MarMammSightings <- MarMammSightings[MarMammSightings$Species !=c('Unknown Seal'),]
MarMammSightings <- MarMammSightings[MarMammSightings$Species !=c('Harbour Seal'),]
MarMammSightings <- MarMammSightings[MarMammSightings$Species !=c('Polar Bear'),]
MarMammSightings <- MarMammSightings[MarMammSightings$Species !=c('Narwhal'),]
MarMammSightings <- MarMammSightings[MarMammSightings$Species !=c('Bowhead Whale'),]
MarMammSightings <- MarMammSightings[MarMammSightings$Species !=c('Megaptera nodosa'),]
MarMammSightings <- MarMammSightings[MarMammSightings$Species !=c('Long-snouted Spinner Dolphin'),]
MarMammSightings <- MarMammSightings[MarMammSightings$Species !=c('Spotted Dolphin'),]
MarMammSightings <- MarMammSightings[MarMammSightings$Species !=c('Pantropical Spotted Dolphin'),]
MarMammSightings <- MarMammSightings[MarMammSightings$Species !=c('Walrus'),]
MarMammSightings <- MarMammSightings[MarMammSightings$Species !=c('Family: Seals (True seals)'),]
MarMammSightings <- MarMammSightings[MarMammSightings$Species !=c('Family: Seals, Sea Lions and Walruses'),]
MarMammSightings <- MarMammSightings[MarMammSightings$Species !=c('Genus: Sea Lions (all genera)'),]
MarMammSightings <- MarMammSightings[MarMammSightings$Species !=c('Hooded Seal'),]
MarMammSightings <- MarMammSightings[MarMammSightings$Species !=c('Northern Fur Seal'),]
MarMammSightings <- MarMammSightings[MarMammSightings$Species !=c('Ringed Seal'),]
MarMammSightings <- MarMammSightings[MarMammSightings$Species !=c('Ribbon Seal'),]
MarMammSightings <- MarMammSightings[MarMammSightings$Species !=c('Steller Sea Lion'),]
MarMammSightings <- MarMammSightings[MarMammSightings$Species !=c('Spotted Seal'),]

table(MarMammSightings$Species)
str(MarMammSightings)
row.has.na <- apply(MarMammSightings, 1, function(x){any(is.na(x))})
sum(row.has.na)
MarMammSightings <- MarMammSightings[!row.has.na,]
table(MarMammSightings$Species)

#write.csv(MarMammSightings, "D:/GIS/Data/MarMammData/MarMammSightings.csv", row.names=FALSE) #creates csv files with al cetacean sightings (1975 - 2015) alive animals

###**************************************************************************************************##
#####------------------------------------PART III: Filter per season and save csv files per species-------------------------#
#(1975 - 2015)
#MarMamm_Winter <- MarMammSightings[MarMammSightings$Month %in% c('12', '1', '2'), ]
#MarMamm_Spring <- MarMammSightings[MarMammSightings$Month %in% c('3', '4', '5'), ]
MarMamm_Summer <- MarMammSightings[MarMammSightings$Month %in% c('6', '7', '8'), ]
#MarMamm_Fall <- MarMammSightings[MarMammSightings$Month %in% c('9', '10', '11'), ]
table(MarMamm_Summer$Species)
#  write.csv(MarMamm_Winter, "D:/GIS/Data/MarMammData/MarMamm_Winter.csv", row.names=FALSE)
#  write.csv(MarMamm_Spring, "D:/GIS/Data/MarMammData/MarMamm_Spring.csv", row.names=FALSE)
#  write.csv(MarMamm_Summer, "D:/GIS/Data/MarMammData/MarMamm_Summer.csv", row.names=FALSE)
#  write.csv(MarMamm_Fall, "D:/GIS/Data/MarMammData/MarMamm_Fall.csv", row.names=FALSE)

#KilleWhales_Summer <- MarMamm_Summer[MarMamm_Summer$Species %in% c('Killer Whale'), ]
#  write.csv(KilleWhales_Summer, "D:/GIS/Data/MarMammData/KillerWhales_Summer.csv", row.names=FALSE)

#Run the script below if you want to create csv files for each species (if you want to run MaxEnt trought th dismo): 
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

