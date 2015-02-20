#********************************************
#                                           #
#   myleaflet Function                      #
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

myleaflet <- function(SPdata.frame, layer, popupNAMES, base.mapNAME, 
                      colorNAMES, roundDIGITS, intervallBRKno,
                      titleNAME) {
  SPdata.framejson <- toGeoJSON(data=SPdata.frame, dest=tempdir())
  brksIntervalls <- classIntervals(SPdata.frame@data[[layer]], n=intervallBRKno)
  brks           <- round(brksIntervalls$brks, digits=roundDIGITS)
  clrs <- colorRampPalette(colorNAMES)(length(brks))
  stl <- styleGrad(prop=layer, breaks=brks, style.val=clrs, 
                   out=1, leg=titleNAME, lwd=2)
  MYleaflet  <- leaflet(data=SPdata.framejson, dest=tempdir(),
                      title=titleNAME, base.map=base.mapNAME,
                      style=stl, popup=popupNAMES)
  return(MYleaflet)}

# myleaflet(SPdata.frame = PLZ, 
#           layer = "FLAECHE_HA", 
#           popupNAMES = c("PLZ", "FLAECHE_HA"),
#           base.mapNAME = "tls",
#           colorNAMES = c("blue","red"),
#           roundDIGITS = -1,
#           intervallBRKno = 3,
#           titleNAME = "Flaeche von die PLZ")