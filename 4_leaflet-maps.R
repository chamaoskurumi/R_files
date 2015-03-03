#********************************************
#                                           #
#         leaflet Kartenerstellung          #
#                                           #
#********************************************

if(!require(devtools)) { install.packages('devtools'); require(devtools) }
devtools::install_github('leafletR', 'chgrl')
# R noch mal neustarten
library("maptools")
library("sp")
library("leafletR")
library("rgdal")
library("classInt")

source("/home/dao/Desktop/MasterArbeit/R_files/functions/myleaflet_FUNCTION.R")

# ---- Choreopleth Maps
##############################################

LOR4leaflet <- LOR
LOR4leaflet@data <- subset(LOR4leaflet@data, select=c(RAUMID, RAUMID_NAME,BZR,BZR_NAME,
                                                      PGR,PRG_NAME,BEZ,BEZ_NAME,                   
                                                      STADTRAUM,FL_HA,
                                                      Miete_H1_wmean.2013))
LORjson <- toGeoJSON(data=LOR4leaflet, dest=tempdir())
brksIntervalls <- classIntervals(LOR4leaflet@data$Miete_H1_wmean.2013, n=10); brksIntervalls
brks           <- round(brksIntervalls$brks, digits=1); brks
#brks <- seq(3, max(LOR@data$EWdichte2013, na.rm=T), by=1000); length(brks)
clrs <- colorRampPalette(c("yellow", "red"))(length(brks))
stl <- styleGrad(prop="Miete_H1_wmean.2013", breaks=brks, style.val=clrs, 
                 out=1, leg="Median Angebotsmiete 1.Halbjahr 2013", 
                 lwd=1, col="blue", alpha=0.4)
SPleaflet  <- leaflet(data=LORjson, dest=tempdir(),
                      title="Median Angebotsmiete 1.Halbjahr 2013", base.map="darkmatter",
                      style=stl, popup="*")
SPleaflet
#View(LOR@data)
names(LOR@data)


myleaflet(SPdata.frame = LOR,
          layer = "Miete_H2_wmean.2008",
          popupNAMES = c("RAUMID", "RAUMID_NAME",
                         "BZR_NAME",
                         "Miete_H2_wmean.2008"), 
          base.mapNAME = "positron", 
          colorNAMES = c("yellow", "red"),
          roundDIGITS = 1, 
          intervallBRKno = 10,
          titleNAME = "Miete pro LOR 2.Hjahr 2008")

myleaflet(SPdata.frame = LOR,
          layer = "Miete_H2_wmean.2013",
          popupNAMES = c("RAUMID", "RAUMID_NAME",
                         "BZR_NAME",
                         "Miete_H2_wmean.2008"), 
          base.mapNAME = "positron", 
          colorNAMES = c("yellow", "red"),
          roundDIGITS = 1, 
          intervallBRKno = 10,
          titleNAME = "Miete pro PLZ 2.Hjahr 2013")


myleaflet(SPdata.frame = PLZ2013  ,
          layer = "Miete_H2",
          popupNAMES = "*", 
          base.mapNAME = "tls", 
          colorNAMES = c("yellow", "red"),
          roundDIGITS = 1, 
          intervallBRKno = 10,
          titleNAME = "Miete pro PLZ 2.Hjahr 2013")

myleaflet(SPdata.frame = PLZ2008  ,
          layer = "Miete_H2",
          popupNAMES = "*", 
          base.mapNAME = "tls", 
          colorNAMES = c("yellow", "red"),
          roundDIGITS = 1, 
          intervallBRKno = 10,
          titleNAME = "Miete pro PLZ 2.Hjahr 2008")
names(PLZ2013@data)

# ----  Sanierungsgebiete
##############################################

# Kategorielle leaflet Karte

SanGebietejson <- toGeoJSON(data=SanGebiete, dest=tempdir())
sty <- styleCat(prop="KLASSENNAM", val=c("aufgehoben",
                                         "umfassend",
                                         "vereinfacht"),
                style.val=c("green", "red", "blue"), leg="Sanierungsgebiete")
SanGebieteleaflet  <- leaflet(data=SanGebietejson, dest=tempdir(),
                              title="Sanierungsgebiete", 
                              base.map="positron",
                              style=sty, 
                              popup=c("GEBIETSNAM","KLASSENNAM"))
SanGebieteleaflet


# ----  plotGoogleMaps Alternative
##############################################

install.packages("plotGoogleMaps")
install.packages("RColorBrewer")
library(plotGoogleMaps)
library(RColorBrewer)

LOR4leaflet <- LOR
m<-plotGoogleMaps(LOR4leaflet,zcol="Miete_H2_wmean.2013",
                  filename=,
                  mapTypeId='TERRAIN',
                  colPalette= colorRampPalette(c("yellow", "red"))(10),
                  strokeColor="grey")
m


