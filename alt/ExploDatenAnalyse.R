#********************************************
#                                           #
#         Explorative - Datenanalyse        #
#                                           #
#********************************************

install.packages("googleVis","ggplot2", "beanplot","rgdal","sp",
                 "leafletR","plotGoogleMaps","GeoXp",
                 "gridExtra", "plyr")
require(devtools)
install_github('rCharts', 'ramnathv')
library("rCharts")
library("googleVis")
library("ggplot2")
library("beanplot")
library("rgdal")
library("sp")
library("leafletR")
library("plotGoogleMaps")
library("GeoXp")
library("gridExtra")
library("plyr")

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Variablen für Explorative Datenanalys generieren  =================

ExDF           <- LORattrFULLwide
ExDF$PDAU5chg  <- ExDF$PDAU5.2008-ExDF$PDAU5.2013
ExDF$PDAU10chg <- ExDF$PDAU10.2008-ExDF$PDAU10.2013

names(ExDF)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Scatter Plots mit size proportional zur EW Zahl =================

sctr_PDAU5chg <- ggplot(ExDF, aes(BEZ_NAME, PDAU5chg), weight=E_E.2013) + 
                 geom_jitter(aes(colour=BEZ_NAME, size = E_E.2013), 
                             position = position_jitter(width = .3)) +
                 scale_size_continuous(breaks=c(1000,5000,10000,20000), range=c(1,10)) +
                 geom_hline(yintercept=0, col="black") 
sctr_PDAU5chg

sctr_PDAU10chg <- ggplot(ExDF, aes(BEZ_NAME, PDAU10chg), weight=E_E.2013) + 
  geom_jitter(aes(colour=BEZ_NAME, size = E_E.2013), 
              position = position_jitter(width = .3)) +
  scale_size_continuous(breaks=c(1000,5000,10000,20000), range=c(1,10)) +
  geom_hline(yintercept=0, col="black") 
sctr_PDAU10chg

sctr_PDAU5chg <- ggplot(ExDF, aes(BEZ_NAME, PDAU5chg), weight=E_E.2013) + 
  geom_jitter(aes(colour=BEZ_NAME, size = E_E.2013), 
              position = position_jitter(width = .3)) +
  scale_size(breaks=c(1000,5000,10000,20000), range=c(1,10)) +
  geom_hline(yintercept=0, col="black") 
sctr_PDAU5chg

ExLOR <- LORshape
ExLORdf       <- as(ExLOR, "data.frame")
Exattr        <- merge(ExLORdf, ExDF, sort=F, 
                    by.x="RAUMID", by.y="RAUMID", all.x=T, all.y=T)
ExLOR@data <- Exattr 

ExLORjson      <- toGeoJSON(data=ExLOR, dest=tempdir())
brksIntervalls <- classIntervals(ExLOR@data$PDAU5chg, n=10); brksIntervalls
brks           <- round(brksIntervalls$brks, digits=1); brks
#brks <- seq(3, max(LOR@data$EWdichte2013, na.rm=T), by=1000); length(brks)
clrs <- colorRampPalette(c("yellow", "red"))(length(brks))
stl <- styleGrad(prop="PDAU5chg", breaks=brks, style.val=clrs, 
                 out=1, 
                 leg="Veränderung des Anteils der Bev. mit Wohndauer > 5 Jahre", lwd=2)
SPleaflet  <- leaflet(data=ExLORjson, dest=tempdir(),
                      title="Veränderung Wohndauer > 5 Jahre", base.map="tls",
                      style=stl, popup=c("RAUMID",
                                         "RAUMID_NAME",
                                         "BEZ_NAME",
                                         "E_E.2013",
                                         "PDAU5chg",
                                         "PDAU5.2008",
                                         "PDAU5.2013"))
SPleaflet


brksIntervalls <- classIntervals(ExLOR@data$PDAU10chg, n=10); brksIntervalls
brks           <- round(brksIntervalls$brks, digits=1); brks
#brks <- seq(3, max(LOR@data$EWdichte2013, na.rm=T), by=1000); length(brks)
clrs <- colorRampPalette(c("yellow", "red"))(length(brks))
stl <- styleGrad(prop="PDAU10chg", breaks=brks, style.val=clrs, 
                 out=1, 
                 leg="Veränderung des Anteils der Bev. mit Wohndauer > 10 Jahre", lwd=2)
SPleaflet  <- leaflet(data=ExLORjson, dest=tempdir(),
                      title="Veränderung Wohndauer > 10 Jahre", base.map="tls",
                      style=stl, popup=c("RAUMID",
                                         "RAUMID_NAME",
                                         "BEZ_NAME",
                                         "E_E.2013",
                                         "PDAU10chg",
                                         "PDAU10.2008",
                                         "PDAU10.2013"))
SPleaflet


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# BEISPIELE rCHARTS

names(iris) = gsub("\\.", "", names(iris))
rPlot(SepalLength ~ SepalWidth | Species, data = iris, color = 'Species', type = 'point')

hair_eye = as.data.frame(HairEyeColor)
rPlot(Freq ~ Hair | Eye, color = 'Eye', data = hair_eye, type = 'bar')

r1 <- rPlot(mpg ~ wt | am + vs, data = mtcars, type = "point", color = "gear");r1

hair_eye_male <- subset(as.data.frame(HairEyeColor), Sex == "Male")
n1 <- nPlot(Freq ~ Hair, group = "Eye", data = hair_eye_male, type = "multiBarChart");n1
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ??? =================

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ??? =================