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
                   "rgdal",      "rgeos",   "foreign",  "PBSmapping"))
library("spdep")
library("sp")
library("maptools")
library("lattice")
library("rgdal")
library("rgeos")
library("foreign")
library("PBSmapping")

#                   "car",        "ggplot2", "spatstat", "RColorBrewer",
#                   "colorspace", "ggplot2", "hexbin",   "vioplot",
#                   "vcd",        "ncf",     "spgwr",    "leaps",
#                   "RANN",       "lmtest"))
#install.packages("gdal")
#library("gdal")

setwd("/home/dao/Desktop/MasterArbeit/GentriMap/4 Geodaten")

IS      <- readOGR(dsn="ImmoScout", layer="Berlin_BGID_projected")
IS@proj4string -> zielCRS
# EPSG 3068 SOLDNER BERLIN
#zielCRS <- CRS("+proj=cass +lat_0=52.41864827777778 +lon_0=13.62720366666667 +x_0=40000 +y_0=10000 +datum=potsdam +units=m
#+no_defs +ellps=bessel +towgs84=598.1,73.7,418.2,0.202,0.045,-2.455,6.7 ")

PLZ1     <- readOGR(dsn="PLZ/PLZ", layer="post_pl")
PLZgeb1  <- readOGR(dsn="PLZ/plz-gebiete.shp", layer="plz-gebiete")

LOR1     <- readOGR(dsn="LOR/LORneu/LOR_SHP_EPSG_3068/", layer="Planungsraum_EPSG_3068")
PGR1     <- readOGR(dsn="LOR/LORneu/LOR_SHP_EPSG_3068/", layer="Prognoseraum_EPSG_3068")
BZR1     <- readOGR(dsn="LOR/LORneu/LOR_SHP_EPSG_3068/", layer="Bezirksregion_EPSG_3068")

SG      <- readOGR(dsn="Geoinstitut/Geoinstitut/", layer="Digk5_StatGeb")

proj4string(PLZ1)    <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ")
proj4string(PLZgeb1) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ")

PLZ2    <- spTransform(PLZ1, zielCRS)
PLZgeb2 <- spTransform(PLZgeb1, zielCRS)

PLZgeb2@data$plzstr(PLZgeb2@data$plz)

PLZ3 <- PLZ2[PLZ2@data$PLZ99_N>=10115, ]
PLZ4 <- PLZ3[PLZ3@data$PLZ99_N<=14199, ]

PLZ <- PLZ4

proj4string(LOR1) <- CRS("+proj=cass +lat_0=52.41864827777778 +lon_0=13.62720366666667 
                         +x_0=40000 +y_0=10000 +ellps=bessel +datum=potsdam +units=m +no_defs ")
proj4string(PGR1) <- CRS("+proj=cass +lat_0=52.41864827777778 +lon_0=13.62720366666667 
                         +x_0=40000 +y_0=10000 +ellps=bessel +datum=potsdam +units=m +no_defs ")
proj4string(BZR1) <- CRS("+proj=cass +lat_0=52.41864827777778 +lon_0=13.62720366666667 
                         +x_0=40000 +y_0=10000 +ellps=bessel +datum=potsdam +units=m +no_defs ")

LOR <- spTransform(LOR1, zielCRS)
PGR <- spTransform(PGR1, zielCRS)
BZR <- spTransform(BZR1, zielCRS)

proj4string(SG)  <- zielCRS

#plot(PLZ4)
#plot(LOR2, add=T)
#plot(IS)
#plot(LOR2,add=T)
#plot(PLZ4, add=T)
#plot(PGR2)
#plot(HB, add=T)
#plot(LOR2)


# Bloecke ----------------------------------------------------------------

#*******************
# Bloecke 2007
#*******************

EW_07_raw <- read.dbf(file = "2007_EPSG3068/Sachdaten/e06_06ewdichte2007.dbf")
head(EW_07_raw)
bloecke07_attributes <- read.dbf(file = "2007_EPSG3068/06_06ewdichte2007_Flaechen.DBF")
bloecke07_attributes$order <- seq(1:length(bloecke07_attributes$SCHLUESSEL))
head(bloecke07_attributes)

EW_07 <- merge(bloecke07_attributes, EW_07_raw, by="SCHLUESSEL", all.x=T, all.y=T)
head(EW_07)
length(EW_07$SCHLUESSEL)
length(bloecke07_attributes$SCHLUESSEL)

table(EW_07$KLASSENNAM, useNA=c("always"))
subset(EW_07,is.na(EW_07$KLASSENNAM))
subset(EW_07, EW_07$EINWOHNER>0 & (is.na(EW_07$FLAECHE_IN)))
is.na(EW_07$KLASSENNAM)

bloecke07  <- readOGR(dsn="/home/dao/Desktop/MasterArbeit/GentriMap/4 Geodaten/Bloecke_GS/2007_EPSG3068/", layer="bloecke_EW")
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

setwd(dir = "/home/dao/Desktop/MasterArbeit/R_data")
WHNDAUER_files <- dir(path="EW_Wohndauer_-LOR-/", pattern = glob2rx("*.csv"))
setwd(dir = "/home/dao/Desktop/MasterArbeit/R_data/EW_Wohndauer_-LOR-/")
WHNDAUER <- lapply(WHNDAUER_files, FUN = read.table, header = TRUE, sep=";",fill=TRUE)
names(WHNDAUER[[2]])
#View(WHNDAUER[[2]])

setwd(dir = "/home/dao/Desktop/MasterArbeit/R_data")
WHNLAGE_files <- dir(path="EW_Wohnlage_-LOR-/", pattern = glob2rx("*.csv"))
setwd(dir = "/home/dao/Desktop/MasterArbeit/R_data/EW_Wohnlage_-LOR-/")
WHNLAGE <- lapply(WHNLAGE_files, FUN = read.table, header = TRUE, sep=";",fill=TRUE)
names(WHNLAGE[[2]])
#View(WHNLAGE[[2]])

setwd(dir = "/home/dao/Desktop/MasterArbeit/R_data")
ALTERAUSLAENDER_files <- dir(path="EW_Alter_Auslaender_-LOR-/", pattern = glob2rx("*.csv"))
setwd(dir = "/home/dao/Desktop/MasterArbeit/R_data/EW_Alter_Auslaender_-LOR-/")
ALTERAUSLAENDER <- lapply(ALTERAUSLAENDER_files, FUN = read.table, header = TRUE, sep=";",fill=TRUE)
head(ALTERAUSLAENDER[[2]])

ALTERAUSLAENDER <- lapply(ALTERAUSLAENDER, function(x) {x$ZEIT <- substr(x$ZEIT,1,4)
                                            x})



#View(ALTERAUSLAENDER[[2]])




