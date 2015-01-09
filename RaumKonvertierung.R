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
                   "rgdal",      "rgeos"))
library("spdep")
library("sp")
library("maptools")
library("lattice")
library("rgdal")
library("rgeos")

#                   "car",        "ggplot2", "spatstat", "RColorBrewer",
#                   "colorspace", "ggplot2", "hexbin",   "vioplot",
#                   "vcd",        "ncf",     "spgwr",    "leaps",
#                   "RANN",       "lmtest"))
#install.packages("gdal")
#library("gdal")

setwd("/home/dao/Desktop/MasterArbeit/GentriMap/4 Geodaten")

IS      <- readOGR(dsn="ImmoScout", layer="Berlin_BGID_projected")
IS@proj4string -> zielCRS
#zielCRS <- CRS("+proj=cass +lat_0=52.41864827777778 +lon_0=13.62720366666667 +x_0=40000 +y_0=10000 +datum=potsdam +units=m
#+no_defs +ellps=bessel +towgs84=598.1,73.7,418.2,0.202,0.045,-2.455,6.7 ")

PLZ1     <- readOGR(dsn="PLZ/PLZ", layer="post_pl")
PLZgeb1  <- readOGR(dsn="PLZ/plz-gebiete.shp", layer="plz-gebiete")

LOR1     <- readOGR(dsn="LOR/LORneu/LOR_SHP_EPSG_3068/", layer="Planungsraum_EPSG_3068")
PGR1     <- readOGR(dsn="LOR/LORneu/LOR_SHP_EPSG_3068/", layer="Prognoseraum_EPSG_3068")
BZR1     <- readOGR(dsn="LOR/LORneu/LOR_SHP_EPSG_3068/", layer="Bezirksregion_EPSG_3068")

HB      <- readOGR(dsn="Geoinstitut/Geoinstitut/", layer="Digk5_Hauptblock")
MB      <- readOGR(dsn="Geoinstitut/Geoinstitut/", layer="Digk5_Metablock")
TB      <- readOGR(dsn="Geoinstitut/Geoinstitut/", layer="Digk5_Teilblock")
STR     <- readOGR(dsn="Geoinstitut/Geoinstitut/", layer="Digk5_strassen")
SG      <- readOGR(dsn="Geoinstitut/Geoinstitut/", layer="Digk5_StatGeb")



proj4string(PLZ1)    <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ")
proj4string(PLZgeb1) <- CRS(" +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ")

PLZ2    <- spTransform(PLZ1, zielCRS)
PLZgeb2 <- spTransform(PLZgeb1, zielCRS)

PLZgeb2@data$plzstr(PLZgeb2@data$plz)

PLZ3 <- PLZ2[PLZ2@data$PLZ99_N>=10115, ]
PLZ4 <- PLZ3[PLZ3@data$PLZ99_N<=14199, ]


proj4string(LOR1) <- CRS("+proj=cass +lat_0=52.41864827777778 +lon_0=13.62720366666667 
                         +x_0=40000 +y_0=10000 +ellps=bessel +datum=potsdam +units=m +no_defs ")
proj4string(PGR1) <- CRS("+proj=cass +lat_0=52.41864827777778 +lon_0=13.62720366666667 
                         +x_0=40000 +y_0=10000 +ellps=bessel +datum=potsdam +units=m +no_defs ")
proj4string(BZR1) <- CRS("+proj=cass +lat_0=52.41864827777778 +lon_0=13.62720366666667 
                         +x_0=40000 +y_0=10000 +ellps=bessel +datum=potsdam +units=m +no_defs ")

LOR2 <- spTransform(LOR1, zielCRS)
PGR2 <- spTransform(PGR1, zielCRS)
BZR2 <- spTransform(BZR1, zielCRS)

plot(PLZ4)
plot(LOR2, add=T)

plot(IS)
plot(LOR2,add=T)
plot(PLZ4, add=T)
plot(PGR2)

proj4string(HB)  <- zielCRS
proj4string(MB)  <- zielCRS
proj4string(TB)  <- zielCRS
proj4string(STR) <- zielCRS
proj4string(SG)  <- zielCRS

plot(HB, add=T)
plot(LOR2)


B_EWdata_full   <- read.table("Einwohnerdaten/bloecke_nutzung_PLR_ewdaten.csv", header = TRUE, sep=",", 
                              colClasses="character")
B_EWdata_full$Einwohneranzahl  <- as.numeric(B_EWdata_full$Einwohneranzahl)
B_EWdata_full$Fläche.in.ha     <- as.numeric(B_EWdata_full$Fläche.in.ha)
B_EWdata_full$Einwohner.pro.ha <- as.numeric(B_EWdata_full$Einwohner.pro.ha)
B_EWdata_full$Einwohneranzahl[is.na(B_EWdata_full$Einwohneranzahl)]   <- 0
B_EWdata_full$Einwohner.pro.ha[is.na(B_EWdata_full$Einwohner.pro.ha)] <- 0
keeps <- c("SCHLUESSEL_BLOCK","SCHLUESSEL_PLR","Einwohneranzahl")
B_EWdata <- B_EWdata_full[keeps]
B_EWdata$SCHLUESSEL <- as.factor(substr(B_EWdata$SCHLUESSEL_BLOCK,1,9))

B_EWdata_agg <-aggregate(Einwohneranzahl ~ SCHLUESSEL, data=B_EWdata, FUN=sum)

B_EWdata_agg <- B_EWdata_agg[B_EWdata_agg$Einwohneranzahl>0,]
#B_EWdata_agg$SCHLUESSEL <- B_EWdata_agg$SCHLUESSEL_BLOCKshort

HB@data$SCHLUESSEL <- (substr(HB@data$SCHLUESSEL,1,9))
shpSCHLUESSEL <- as.data.frame(HB@data$SCHLUESSEL)
names(shpSCHLUESSEL)[names(shpSCHLUESSEL)=="HB@data$SCHLUESSEL"] <- "SCHLUESSEL"

ziel<- merge(shpSCHLUESSEL,B_EWdata_agg, by.x="SCHLUESSEL", by.y="SCHLUESSEL", all.x=T)
sum(ziel$Einwohneranzahl, na.rm=T)
