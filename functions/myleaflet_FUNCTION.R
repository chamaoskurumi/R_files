# ---- myleaflet function
##############################################


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
