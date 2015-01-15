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

bloecke1 <- readOGR(dsn="Bloecke_GS/", layer="bloecke_EW")
bloecke1@data
bloecke  <- spTransform(bloecke1, zielCRS)
bloecke@data[is.na(bloecke@data)] <- 0
bloecke@data$EW2013 <- as.numeric(bloecke@data$EW2013)
bloecke@data$EW2012 <- as.numeric(bloecke@data$EW2012)
bloecke@data$EW2011 <- as.numeric(bloecke@data$EW2011)
bloecke@data$EW2010 <- as.numeric(bloecke@data$EW2010)


sum(as.numeric(bloecke1@data$EW2013), na.rm=T)
View(bloecke1)


B_EWdata_full   <- read.table("Einwohnerdaten/nurbloecke_mitEinwohnern.csv", header = TRUE, sep=",", 
                              colClasses="character")

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
ALTERAUSLAENDER <- lapply(ALTERAUSLAENDER, FUN = substr )
ALTERAUSLAENDER[[1$ZEIT]]
names(ALTERAUSLAENDER[[2]])

jahre <- list(2007, 2008, 2009, 2010, 2011, 2012, 2013)
Map(cbind, WHNLAGE, ZEIT = jahre)


#View(ALTERAUSLAENDER[[2]])




