library("maptools")
library("sp")
library("leafletR")

setwd("/home/dao/Desktop/MasterArbeit/GentriMap/4 Geodaten")
PLZ                 <- readOGR(dsn="PLZ_GS/RBS_OD_PLZ_01_2014/", layer="RBS_OD_PLZ_1312", encoding = "UTF-8")
proj4string(PLZ)    <- CRS("+proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs") # EPSG:25833
plot(PLZ)
PLZ_GSW2013  <- read.csv("/home/dao/Desktop/PLZ_GSW2013_Daten.csv")
PLZ@data     <- merge(PLZ@data, PLZ_GSW2013, all.x=T)

PLZjson <- toGeoJSON(data=PLZ, dest=tempdir())

brks <- seq(0, max(PLZ@data$GSWmiete_kaltMEDIAN, na.rm=T), by=0.5); length(brks)
clrs <- colorRampPalette(c("blue","yellow", "red"))(25)
stl <- styleGrad(prop="GSWmiete_kaltMEDIAN", breaks=brks, style.val=clrs, 
                 out=1, leg="Kaltmiete Median")
SPleaflet  <- leaflet(data=PLZjson, dest=tempdir(),
                      title="Trying to plot Kaltmiete Median", base.map="tls",
                      style=stl, popup="*")
SPleaflet # tadaa


if(!require(devtools)) { install.packages('devtools'); require(devtools) }
devtools::install_github('leafletR', 'chgrl')
# R noch mal neustarten
library("maptools")
library("sp")
library("leafletR")
library("rgdal")
library("classInt")

setwd("/home/dao/Desktop/MasterArbeit/GentriMap/4 Geodaten")
PLZ                 <- readOGR(dsn="PLZ_GS/RBS_OD_PLZ_01_2014/", layer="RBS_OD_PLZ_1312", encoding = "UTF-8")
proj4string(PLZ)    <- CRS("+proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs") # EPSG:25833
plot(PLZ)
PLZ_GSW2013  <- read.csv("/home/dao/Desktop/PLZ_GSW2013_Daten.csv")
PLZ@data     <- merge(PLZ@data, PLZ_GSW2013, all.x=T)

PLZjson <- toGeoJSON(data=PLZ, dest=tempdir())

brks <- seq(4.5, max(PLZ@data$GSWmiete_kaltMEDIAN, na.rm=T), by=0.5)
clrs <- colorRampPalette(c("yellow", "red"))(length(brks))
stl <- styleGrad(prop="GSWmiete_kaltMEDIAN", breaks=brks, style.val=clrs, 
                 out=1, leg="Kaltmiete Median", lwd=2)
SPleaflet  <- leaflet(data=PLZjson, dest=tempdir(),
                      title="Trying to plot Kaltmiete Median", base.map="tonerlite",
                      style=stl, popup="*")
SPleaflet


  
LORjson <- toGeoJSON(data=LOR, dest=tempdir())
brksIntervalls <- classIntervals(LOR@data$EWdichte2013, n=10)
brks           <- round(brksIntervalls$brks, digits=-3)
#brks <- seq(3, max(LOR@data$EWdichte2013, na.rm=T), by=1000); length(brks)
clrs <- colorRampPalette(c("yellow", "red"))(length(brks))
stl <- styleGrad(prop="EWdichte2013", breaks=brks, style.val=clrs, 
                 out=1, leg="Einwohner Dichte pro km²", lwd=2)
SPleaflet  <- leaflet(data=LORjson, dest=tempdir(),
                      title="Einwohner Dichte pro km²", base.map="tls",
                      style=stl, popup="*")
SPleaflet

LOR4leaflet <- LOR
LOR4leaflet@data <- subset(LOR4leaflet@data, select=c(RAUMID, RAUMID_NAME,BZR,BZR_NAME,
                                                      PGR,PRG_NAME,BEZ,BEZ_NAME,                   
                                                      STADTRAUM,FL_HA,
                                                      EWdichte.2013))
LORjson <- toGeoJSON(data=LOR4leaflet, dest=tempdir())
brksIntervalls <- classIntervals(LOR@data$EWdichte.2013, n=10); brksIntervalls
brks           <- round(brksIntervalls$brks, digits=-3); brks
#brks <- seq(3, max(LOR@data$EWdichte2013, na.rm=T), by=1000); length(brks)
clrs <- colorRampPalette(c("yellow", "red","blue"))(length(brks))
stl <- styleGrad(prop="EWdichte.2013", breaks=brks, style.val=clrs, 
                 out=1, leg="Einwohner 2013", lwd=2)
SPleaflet  <- leaflet(data=LORjson, dest=tempdir(),
                      title="Einwohner Dichte 2013", base.map="tls",
                      style=stl, popup="*")
SPleaflet
View(LOR@data)

install.packages("plotGoogleMaps")
install.packages("RColorBrewer")
library(plotGoogleMaps)
library(RColorBrewer)
m<-plotGoogleMaps(LOR4leaflet,zcol="EWdichte.2013",filename='MyMap6.htm',
                  mapTypeId='TERRAIN',colPalette= colorRampPalette(c("yellow", "red","blue"))(length(brks)),
                  strokeColor="white")
m


