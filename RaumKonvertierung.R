#******************************************
#
# "Räumliche Konvertierung"
#
# by Guido Schulz
#******************************************
Sys.setlocale("LC_MESSAGES", 'en_GB.UTF-8')
Sys.setenv(LANG = "en_US.UTF-8")

# Packages ----------------------------------------------------------------

install.packages(c("spdep",      "sp",      "maptools", "lattice", 
                   "rgdal",      "rgeos",   "foreign",  "PBSmapping",
                   "reshape",    "plyr"))
library("spdep")
library("sp")
library("maptools")
library("lattice")
library("rgdal")
library("rgeos")
library("foreign")
library("PBSmapping")
library("reshape")
library("plyr")

#                   "car",        "ggplot2", "spatstat", "RColorBrewer",
#                   "colorspace", "ggplot2", "hexbin",   "vioplot",
#                   "vcd",        "ncf",     "spgwr",    "leaps",
#                   "RANN",       "lmtest"))
#install.packages("gdal")
#library("gdal")

# Shape files --------------------------------------------------------------

#~~~~~~~~~~~~~~~~~~~~~~~~
# ImmoScout Ortsteile
#~~~~~~~~~~~~~~~~~~~~~~~~

setwd("/home/dao/Desktop/MasterArbeit/GentriMap/4 Geodaten")
IS       <- readOGR(dsn="ImmoScout", layer="Berlin_BGID_projected")
zielCRS  <- IS@proj4string
# EPSG 3068 SOLDNER BERLIN
#zielCRS <- CRS("+proj=cass +lat_0=52.41864827777778 +lon_0=13.62720366666667 +x_0=40000 +y_0=10000 +datum=potsdam +units=m
#+no_defs +ellps=bessel +towgs84=598.1,73.7,418.2,0.202,0.045,-2.455,6.7 ")

#~~~~~~~~~~~~~~~~~~~~~~~~
# Postleitzahlen PLZ
#~~~~~~~~~~~~~~~~~~~~~~~~

setwd("/home/dao/Desktop/MasterArbeit/GentriMap/4 Geodaten")
PLZ1     <- readOGR(dsn="PLZ_GS/RBS_OD_PLZ_01_2014/", layer="RBS_OD_PLZ_1312")
proj4string(PLZ1)    <- CRS("+proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs") # EPSG:25833
PLZ    <- spTransform(PLZ1, zielCRS)
#plot(PLZ)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Stat. Landesamt LOR (Prognose-, Planungsräume, Bezirksregionen)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

setwd("/home/dao/Desktop/MasterArbeit/GentriMap/4 Geodaten")
LOR1     <- readOGR(dsn="LOR/LORneu/LOR_SHP_EPSG_3068/", layer="Planungsraum_EPSG_3068")
PGR1     <- readOGR(dsn="LOR/LORneu/LOR_SHP_EPSG_3068/", layer="Prognoseraum_EPSG_3068")
BZR1     <- readOGR(dsn="LOR/LORneu/LOR_SHP_EPSG_3068/", layer="Bezirksregion_EPSG_3068")
#SG       <- readOGR(dsn="Geoinstitut/Geoinstitut/", layer="Digk5_StatGeb")

proj4string(LOR1) <- CRS("+proj=cass +lat_0=52.41864827777778 +lon_0=13.62720366666667 
                         +x_0=40000 +y_0=10000 +ellps=bessel +datum=potsdam +units=m +no_defs ")
proj4string(PGR1) <- CRS("+proj=cass +lat_0=52.41864827777778 +lon_0=13.62720366666667 
                         +x_0=40000 +y_0=10000 +ellps=bessel +datum=potsdam +units=m +no_defs ")
proj4string(BZR1) <- CRS("+proj=cass +lat_0=52.41864827777778 +lon_0=13.62720366666667 
                         +x_0=40000 +y_0=10000 +ellps=bessel +datum=potsdam +units=m +no_defs ")
#SG@proj4string
#proj4string(SG)   <- zielCRS

LOR <- spTransform(LOR1, zielCRS)
PGR <- spTransform(PGR1, zielCRS)
BZR <- spTransform(BZR1, zielCRS)
#plot(LOR)
#plot(BZR)

#~~~~~~~~~~~~~~~~~~~~~~~~
# Sanierungsgebiete
#~~~~~~~~~~~~~~~~~~~~~~~~

setwd("/home/dao/Desktop/MasterArbeit/GentriMap/4 Geodaten")
SanGebiete1                <- readOGR(dsn="Sanierungsgebiete_GS/Sanierungsgebiete_EPSG_25833/",
                                         layer="Sanierungsgebiete_EPSG_25833")
proj4string(SanGebiete1)   <- CRS("+proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs") # EPSG:25833
SanGebiete                 <- spTransform(SanGebiete1, zielCRS)

SanGebiete@data$KLASSENNAM <- revalue(SanGebiete@data$KLASSENNAM, c("Verfahren_aufgehoben"  = "aufgehoben",
                                                                    "Verfahren_Umfassend"   = "umfassend",
                                                                    "Verfahren_Vereinfacht" = "vereinfacht"))
#SanGebiete@data$KLASSENNAM

#plot(PLZ)
#plot(SanGebiete, col="red", add=T)

# Bloecke ----------------------------------------------------------------

#*******************
# Bloecke 2007
#*******************

setwd(dir="/home/dao/Desktop/MasterArbeit/GentriMap/4 Geodaten/Bloecke_GS/")
EW_07_raw <- read.dbf(file = "2007_EPSG3068/Sachdaten/e06_06ewdichte2007.dbf")
head(EW_07_raw)
bloecke07_attributes <- read.dbf(file = "2007_EPSG3068/06_06ewdichte2007_Flaechen.DBF")
bloecke07_attributes$order <- seq(1:length(bloecke07_attributes$SCHLUESSEL))
head(bloecke07_attributes)

EW_07 <- merge(bloecke07_attributes, EW_07_raw, by="SCHLUESSEL", all.x=T, all.y=T)
head(EW_07)
length(EW_07_raw$SCHLUESSEL)
length(bloecke07_attributes$SCHLUESSEL)
length(EW_07$SCHLUESSEL)
subset(EW_07_raw, EW_07_raw$EINWOHNER>0 & is.na(EW_07_raw$EW_PRO_HA)) # diese 38 Fälle sind das Problem


table(EW_07$KLASSENNAM, useNA=c("always"))
subset(EW_07,is.na(EW_07$KLASSENNAM))
subset(EW_07, EW_07$EINWOHNER>0 & (is.na(EW_07$FLAECHE_IN)))
is.na(EW_07$KLASSENNAM)

bloecke07  <- readOGR(dsn="/home/dao/Desktop/MasterArbeit/GentriMap/4 Geodaten/Bloecke_GS/2007_EPSG3068/", layer="06_06ewdichte2007_Flaechen.shp")
bloecke07  <- spTransform(bloecke07, zielCRS)
#sum(as.numeric(bloecke07@data$EW2013), na.rm=T)
#str(bloecke07@data)
#plot(bloecke07)

#*******************
# Bloecke 2008
#*******************

EW_08_raw <- read.dbf(file = "2008_EPSG3068/Sachdaten/06_06EWdichte2008.DBF")
head(EW_08_raw)
bloecke08_attributes <- read.dbf(file = "2008_EPSG3068/alt/06_06ewdichte2008.DBF")
bloecke08_attributes$order <- seq(1:length(bloecke08_attributes$SCHLUESSEL))
head(bloecke08_attributes)

EW_08 <- merge(bloecke08_attributes, EW_08_raw, by="SCHLUESSEL", all.x=T, all.y=T)
head(EW_08)
length(EW_08_raw$SCHLUESSEL)
length(bloecke08_attributes$SCHLUESSEL)
length(EW_08$SCHLUESSEL)
EW_08 <- EW_08[with(EW_08, order(order)), ]
head(EW_08)
#sum(EW_08$EINWOHNER, na.rm=T)
setwd(dir="2008_EPSG3068/")
write.dbf(EW_08, file="06_06ewdichte2008", factor2char = TRUE)

bloecke08  <- readOGR(dsn="/home/dao/Desktop/MasterArbeit/GentriMap/4 Geodaten/Bloecke_GS/2008_EPSG3068/", layer="06_06ewdichte2008")
proj4string(bloecke08) <- CRS("+proj=cass +lat_0=52.41864827777778 +lon_0=13.62720366666667 
                              +x_0=40000 +y_0=10000 +ellps=bessel +datum=potsdam +units=m +no_defs")
bloecke08  <- spTransform(bloecke08, zielCRS)
bloecke08@data$EINWOHNER[is.na(bloecke08@data$EINWOHNER)] <- 0
bloecke08@data$EW_PRO_HA[is.na(bloecke08@data$EW_PRO_HA)] <- 0
bloecke08 <- bloecke08[bloecke08@data$EINWOHNER>0, ] # alle Blöcke löschen, wo niemand wohnt
#length(bloecke08@data$SCHLUESSEL)
#sum(as.numeric(bloecke08@data$EW2013), na.rm=T)
#str(bloecke08@data)
#plot(bloecke08)
#View(bloecke08@data)

#*******************
# Bloecke 2009
#*******************

setwd(dir="/home/dao/Desktop/MasterArbeit/GentriMap/4 Geodaten/Bloecke_GS/")
EW_09_raw <- read.dbf(file = "2009_EPSG3068/Sachdaten/s06_06EWdichte2009.DBF")
head(EW_09_raw)
bloecke09_attributes <- read.dbf(file = "2009_EPSG3068/alt/06_06ewdichte2009.DBF")
bloecke09_attributes$order <- seq(1:length(bloecke09_attributes$SCHLUESSEL))
head(bloecke09_attributes)

EW_09 <- merge(bloecke09_attributes, EW_09_raw, by="SCHLUESSEL", all.x=T, all.y=T)
length(EW_09_raw$SCHLUESSEL)
length(bloecke09_attributes$SCHLUESSEL)
length(EW_09$SCHLUESSEL)
EW_09 <- EW_09[with(EW_09, order(order)), ]
head(EW_09)
sum(EW_09$EW_GESAMT, na.rm=T)
write.dbf(EW_09, file="06_06ewdichte2009", factor2char = TRUE)

bloecke09  <- readOGR(dsn="/home/dao/Desktop/MasterArbeit/GentriMap/4 Geodaten/Bloecke_GS/2009_EPSG3068/", layer="06_06ewdichte2009")
proj4string(bloecke09) <- CRS("+proj=cass +lat_0=52.41864827777778 +lon_0=13.62720366666667 
                              +x_0=40000 +y_0=10000 +ellps=bessel +datum=potsdam +units=m +no_defs")
bloecke09  <- spTransform(bloecke09, zielCRS)
bloecke09@data$EW_GESAMT[is.na(bloecke09@data$EW_GESAMT)] <- 0
bloecke09@data$EW_PRO_HA[is.na(bloecke09@data$EW_PRO_HA)] <- 0
bloecke09@data <- bloecke09@data[,-(3)] # leere LOR Variable löschen
bloecke09 <- bloecke09[bloecke09@data$EW_GESAMT>0, ] # alle Blöcke löschen, wo niemand wohnt
# length(bloecke09@data$SCHLUESSEL)
#sum(as.numeric(bloecke09@data$EW2013), na.rm=T)
#str(bloecke09@data)
#View(bloecke09@data)
#plot(bloecke09)

#*******************
# Bloecke 2010-2013
#*******************

setwd(dir="/home/dao/Desktop/MasterArbeit/GentriMap/4 Geodaten/Bloecke_GS")
EW_10_13 <- read.table(file ="EW_2010-2013.csv", header=T, sep=",")
EW_10_13$spatial_na <- as.factor(data$spatial_na)
EW_10_13$spatial_al <- as.factor(data$spatial_al)
EW_10_13[is.na(EW_10_13)] <- 0
write.dbf(EW_10_13, file="bloecke_EW", factor2char = TRUE)

bloecke10_13  <- readOGR(dsn="/home/dao/Desktop/MasterArbeit/GentriMap/4 Geodaten/Bloecke_GS", layer="bloecke_EW")
bloecke10_13  <- spTransform(bloecke10_13, zielCRS)
bloecke10_13  <- bloecke10_13[bloecke10_13@data$EW2013>0 | 
                              bloecke10_13@data$EW2012>0 |
                              bloecke10_13@data$EW2011>0 |
                              bloecke10_13@data$EW2010>0, ] # alle Blöcke löschen, wo niemand wohnt
#length(bloecke10_13@data$EW2013)
#sum(as.numeric(bloecke10_13@data$EW2012), na.rm=T)
#sum(EW_10_13$EW2012) # die beiden Summen sollten gleich sein für alle Jahre - passt!
#str(bloecke10_13@data)
#plot(bloecke10_13)
#View(bloecke10_13@data)

# Daten -------------------------------------------------------------------

#~~~~~~~~~~~~~~~~~~~~~~~~
# Einwohner
#~~~~~~~~~~~~~~~~~~~~~~~~

setwd(dir = "/home/dao/Desktop/MasterArbeit/R_data")
EW_files <- dir(path="EW_-LOR-/", pattern = glob2rx("*.csv"))
setwd(dir = "/home/dao/Desktop/MasterArbeit/R_data/EW_-LOR-/")
EW <- lapply(EW_files, FUN = read.table, header = TRUE, sep=";",fill=TRUE)
head(EW[[2]])
names(EW[[2]])
EW <- lapply(EW, function(x) {x$ZEIT <- substr(x$ZEIT,1,4)
                              x})
#View(EW[[2]])

#~~~~~~~~~~~~~~~~~~~~~~~~
# Wohndauer
#~~~~~~~~~~~~~~~~~~~~~~~~

setwd(dir = "/home/dao/Desktop/MasterArbeit/R_data")
WHNDAUER_files <- dir(path="EW_Wohndauer_-LOR-/", pattern = glob2rx("*.csv"))
setwd(dir = "/home/dao/Desktop/MasterArbeit/R_data/EW_Wohndauer_-LOR-/")
WHNDAUER <- lapply(WHNDAUER_files, FUN = read.table, header = TRUE, sep=";",fill=TRUE)
names(WHNDAUER[[2]])
#View(WHNDAUER[[2]])

#~~~~~~~~~~~~~~~~~~~~~~~~
# Wohnlage
#~~~~~~~~~~~~~~~~~~~~~~~~

setwd(dir = "/home/dao/Desktop/MasterArbeit/R_data")
WHNLAGE_files <- dir(path="EW_Wohnlage_-LOR-/", pattern = glob2rx("*.csv"))
setwd(dir = "/home/dao/Desktop/MasterArbeit/R_data/EW_Wohnlage_-LOR-/")
WHNLAGE <- lapply(WHNLAGE_files, FUN = read.table, header = TRUE, sep=";",fill=TRUE)
names(WHNLAGE[[2]])
#View(WHNLAGE[[2]])

#~~~~~~~~~~~~~~~~~~~~~~~~
# Ausländer und Alter
#~~~~~~~~~~~~~~~~~~~~~~~~

setwd(dir = "/home/dao/Desktop/MasterArbeit/R_data")
ALTERAUSLAENDER_files <- dir(path="EW_Alter_Auslaender_-LOR-/", pattern = glob2rx("*.csv"))
setwd(dir = "/home/dao/Desktop/MasterArbeit/R_data/EW_Alter_Auslaender_-LOR-/")
ALTERAUSLAENDER <- lapply(ALTERAUSLAENDER_files, FUN = read.table, header = TRUE, sep=";",fill=TRUE)
head(ALTERAUSLAENDER[[2]])
names(ALTERAUSLAENDER[[2]])

ALTERAUSLAENDER <- lapply(ALTERAUSLAENDER, function(x) {x$ZEIT <- substr(x$ZEIT,1,4)
                                            x})
#View(ALTERAUSLAENDER[[2]])

#~~~~~~~~~~~~~~~~~~~~~~~~
# Migrationshintergrund
#~~~~~~~~~~~~~~~~~~~~~~~~

setwd(dir = "/home/dao/Desktop/MasterArbeit/R_data")
MIGHINTER_files <- dir(path="EW_Migrationshintergrund_-LOR-/", pattern = glob2rx("*.csv"))
setwd(dir = "/home/dao/Desktop/MasterArbeit/R_data/EW_Migrationshintergrund_-LOR-/")
MIGHINTER <- lapply(MIGHINTER_files, FUN = read.table, header = TRUE, sep=";",fill=TRUE)
head(MIGHINTER[[2]])
names(MIGHINTER[[2]])
MIGHINTER <- lapply(MIGHINTER, function(x) {x$ZEIT <- substr(x$ZEIT,1,4)
                                                        x})
#View(MIGHINTER[[2]])

#~~~~~~~~~~~~~~~~~~~~~~~~
# Binnenwanderung LOR 
#~~~~~~~~~~~~~~~~~~~~~~~~
install.packages("reshape2")
library("reshape2")
setwd(dir = "/home/dao/Desktop/MasterArbeit/R_data")
BINNENWAND_files <- dir(path="Binnenwanderungen_-LOR-/", pattern = glob2rx("*.csv"))
setwd(dir = "/home/dao/Desktop/MasterArbeit/R_data/Binnenwanderungen_-LOR-/")
BINNENWAND <- lapply(BINNENWAND_files, FUN = read.table, header = TRUE, sep=",",fill=TRUE)
colnames(BINNENWAND[[6]])[1] <- "VonLOR"
colnames(BINNENWAND[[6]])[2] <- "NachLOR"
colnames(BINNENWAND[[7]])[1] <- "VonLOR"
colnames(BINNENWAND[[7]])[2] <- "NachLOR"
  
head(BINNENWAND[[2]])
str(BINNENWAND)
names(BINNENWAND[[1]])
BINNENWANDnew <- lapply(BINNENWAND, function(x) {cast(data = x, VonLOR ~ NachLOR)
                                              x})
BINNENWANDnew <- lapply(BINNENWAND, function(x) {dcast(data = x, VonLOR ~ NachLOR)
                                              x})


BINNENWAND[[1]] <- cast(data = BINNENWAND[[1]], formula = VonLOR ~ NachLOR)
BINNENWAND[[1]][is.na(BINNENWAND[[1]])] <- 0
diag(BINNENWAND[[1]]) <- NA
head(BINNENWAND[[1]])
names(BINNENWAND[[1]])

