#********************************************
#                                           #
#         Explorative - Datenanalyse        #
#                                           #
#********************************************

#install.packages("googleVis","ggplot2", "beanplot","rgdal","sp",
#                 "leafletR","plotGoogleMaps","GeoXp",
#                 "gridExtra", "plyr","vioplot")
#install.packages(c("devtools","RJSONIO", "knitr", "shiny", "httpuv"))
require(devtools)
install_github('rCharts', 'ramnathv')
library("rCharts")
install_github("mages/googleVis")
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
library("vioplot")


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Variablen für Explorative Datenanalys generieren  =================

ExDF                <- LORattrFULLwide4shape
names(ExDF)
# Änderung der Wohndauer
ExDF$PDAU5chg       <- ExDF$PDAU5.2008-ExDF$PDAU5.2013
ExDF$PDAU10chg      <- ExDF$PDAU10.2008-ExDF$PDAU10.2013
# Mietniveau 2013 mitteln 
ExDF$Miete.2013     <- (ExDF$Miete_H1_wmean.2013 + ExDF$Miete_H2_wmean.2013)/2
# Mietpreisänderung
ExDF$MIETE_H1chg    <- ExDF$Miete_H1_wmean.2013 - ExDF$Miete_H1_wmean.2008
ExDF$MIETE_H1chgr   <- ((ExDF$Miete_H1_wmean.2013 - ExDF$Miete_H1_wmean.2008)/ExDF$Miete_H1_wmean.2008)*100
ExDF$MIETE_H2chg    <- ExDF$Miete_H2_wmean.2013 - ExDF$Miete_H2_wmean.2008
ExDF$MIETE_H2chgr   <- ((ExDF$Miete_H2_wmean.2013 - ExDF$Miete_H2_wmean.2008)/ExDF$Miete_H2_wmean.2008)*100
ExDF$MIETE_chg      <- (ExDF$MIETE_H1chg+ExDF$MIETE_H2chg)/2
ExDF$MIETE_chgr     <- round((ExDF$MIETE_H1chgr+ExDF$MIETE_H2chgr)/2,digits=0)
# Änderung Arbeitslosigkeit
ExDF$Alosechg      <- ExDF$Alose.2013 - ExDF$Alose.2008
ExDF$Alose_u25chg  <- ExDF$Alose_u25.2013 - ExDF$Alose_u25.2008
ExDF$Alose_Hartzchg<-ExDF$Alose_Hartz.2013 - ExDF$Alose_Hartz.2008
ExDF$Hartz_u15chg  <- ExDF$Hartz_u15.2013 - ExDF$Hartz_u15.2008
# Ausländeranteil
ExDF$E_Ar.2013        <- round((ExDF$E_A.2013       /ExDF$E_E.2013)*100,digits=1)
ExDF$HK_Turkr.2013    <- round((ExDF$HK_Turk.2013   /ExDF$E_E.2013)*100,digits=1)
ExDF$HK_Arabr.2013    <- round((ExDF$HK_Arab.2013   /ExDF$E_E.2013)*100,digits=1)
ExDF$HK_EU15r.2013    <- round((ExDF$HK_EU15.2013   /ExDF$E_E.2013)*100,digits=1)
ExDF$HK_EU27r.2013    <- round((ExDF$HK_EU27.2013   /ExDF$E_E.2013)*100,digits=1)
ExDF$HK_EheJugr.2013  <- round((ExDF$HK_EheJug.2013 /ExDF$E_E.2013)*100,digits=1)
ExDF$HK_EheSUr.2013   <- round((ExDF$HK_EheSU.2013  /ExDF$E_E.2013)*100,digits=1)
# Änderung Ausländeranteil
ExDF$E_Achgr        <- round((((ExDF$E_A.2013         /ExDF$E_E.2013)*100) -  ((ExDF$E_A.2008       /ExDF$E_E.2008)*100)),digits=1)
ExDF$HK_Turkchgr    <- round((((ExDF$HK_Turk.2013     /ExDF$E_E.2013)*100) -  ((ExDF$HK_Turk.2008   /ExDF$E_E.2008)*100)),digits=1)
ExDF$HK_Arabchgr    <- round((((ExDF$HK_Arab.2013     /ExDF$E_E.2013)*100) -  ((ExDF$HK_Arab.2008   /ExDF$E_E.2008)*100)),digits=1)
ExDF$HK_EU15chgr    <- round((((ExDF$HK_EU15.2013     /ExDF$E_E.2013)*100) -  ((ExDF$HK_EU15.2008   /ExDF$E_E.2008)*100)),digits=1)
ExDF$HK_EU27chgr    <- round((((ExDF$HK_EU27.2013     /ExDF$E_E.2013)*100) -  ((ExDF$HK_EU27.2008   /ExDF$E_E.2008)*100)),digits=1)
ExDF$HK_EheJugchgr  <- round((((ExDF$HK_EheJug.2013   /ExDF$E_E.2013)*100) -  ((ExDF$HK_EheJug.2008 /ExDF$E_E.2008)*100)),digits=1)
ExDF$HK_EheSUchgr   <- round((((ExDF$HK_EheSU.2013    /ExDF$E_E.2013)*100) -  ((ExDF$HK_EheSU.2008  /ExDF$E_E.2008)*100)),digits=1)
names(ExDF)

JLLdataEx <- subset(JLLdata, JLLdata$Zeit>=2008)
JLLdataExwide <- reshape(JLLdataEx,                 
                         idvar = c("PLZ"),
                         v.names = c("Miete_H1",
                                     "Miete_H2"),
                         timevar = "Zeit",
                         direction = "wide")
View(JLLdataEx)

JLLdataSUMMARY <- ddply(JLLdataEx, 
                       "PLZ", summarize, 
                       MieteH1_min   = min(Miete_H1),
                       MieteH1_max   = max(Miete_H1),
                       MieteH1_range = max(Miete_H1)-min(Miete_H1),
                       MieteH1_sd    = round(sd(Miete_H1), digits = 2))

par(mfrow=c(1,1))
vioMIN   <- vioplot(na.omit(JLLdataSUMMARY$MieteH1_min)  , horizontal=F, names="min")
vioMAX   <- vioplot(na.omit(JLLdataSUMMARY$MieteH1_max)  , horizontal=F, names="max")
vioRANGE <- vioplot(na.omit(JLLdataSUMMARY$MieteH1_range), horizontal=F, names="range")
vioSD    <- vioplot(na.omit(JLLdataSUMMARY$MieteH1_sd)   , horizontal=F, names="sd")

vioplot(na.omit(JLLdataSUMMARY$MieteH1_min),
        na.omit(JLLdataSUMMARY$MieteH1_max),
        horizontal=F,
        names=c("min","max"))


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Scatter Plots mit size proportional zur EW Zahl =================

MieteE_Abubble <- gvisBubbleChart(ExDF, idvar="RAUMID_NAME", 
                          xvar="MIETE_chgr", yvar="HK_Turkchgr",
                          colorvar="BEZ_NAME", sizevar="E_E.2013",
                          options=list(width=1200, height=700))
plot(MieteE_Abubble)


sctr_PDAU5chg <- ggplot(ExDF, aes(BEZ_NAME, PDAU5chg), weight=E_E.2012) + 
                 geom_jitter(aes(colour=BEZ_NAME, size = E_E.2012), 
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

# LOR Motion Chart
names(LORdataFULL)
LORdata4ggvis <- LORdataFULL
LORdata4ggvis$ZEIT <- as.numeric(as.character(LORdata4ggvis$ZEIT))
LORgvisMotion <- gvisMotionChart(LORdata4ggvis, 
                                 idvar   = "RAUMID_NAME",
                                 timevar = "ZEIT",
                                 sizevar = "E_E",
                                 colorvar= "BEZ_NAME",
                                 options=list(width=1200, height=700))
plot(LORgvisMotion)

# PLZ Miete Motion Chart --> 12049 Schillerpromenade mal anschauen!
str(JLLdata)
JLLdata4ggvis <- JLLdata
JLLgvisMotion <- gvisMotionChart(JLLdata4ggvis, 
                                 idvar   = "PLZ",
                                 timevar = "Zeit",
                                 options=list(width=1200, height=700))
plot(JLLgvisMotion)



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