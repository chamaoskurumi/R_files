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

ExDF                <- LORdataWIDE
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

sctr_PDAU5chg <- ggplot(ExDF, aes(BEZ_NAME, PDAU5chg), weight=E_E.2012) + 
                 geom_jitter(aes(colour=BEZ_NAME, size = E_E.2012), 
                             position = position_jitter(width = .3)) +
                 scale_size_continuous(breaks=c(1000,5000,10000,20000), range=c(1,10)) +
                 geom_hline(yintercept=0, col="black") 
sctr_PDAU5chg

sctr_PDAU10chg <- ggplot(ExDF, aes(BEZ_NAME, PDAU10chg), weight=E_E.2012) + 
  geom_jitter(aes(colour=BEZ_NAME, size = E_E.2012), 
              position = position_jitter(width = .3)) +
  scale_size_continuous(breaks=c(1000,5000,10000,20000), range=c(1,10)) +
  geom_hline(yintercept=0, col="black") 
sctr_PDAU10chg

sctr_PDAU5chg <- ggplot(ExDF, aes(BEZ_NAME, PDAU5chg), weight=E_E.2012) + 
  geom_jitter(aes(colour=BEZ_NAME, size = E_E.2012), 
              position = position_jitter(width = .3)) +
  scale_size(breaks=c(1000,5000,10000,20000), range=c(1,10)) +
  geom_hline(yintercept=0, col="black") 
sctr_PDAU5chg
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

h1 <- hPlot(x = "Wr.Hnd", y = "NW.Hnd",
            data = MASS::survey,
            type = c("line", "bubble", "scatter"),
            group = "Clap",
            size = "Age")
h1


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
