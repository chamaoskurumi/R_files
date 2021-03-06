#*************************************************
#*************************************************
#
# Regressionen Binnenwandeurngen
#
#*************************************************
#*************************************************

setwd("/home/dao/Desktop/MasterArbeit/R_files/KNITR/")
load(file = "FULL_FINAL_WORKSPACE.Rdata")


# ____ Packages ______ ----------------------------------------------------------------

library("rgdal")
library("sp")
library("spdep")
library("vioplot")
library("ggplot2")
library("GGally")
require("gridExtra")
require("lattice")
require("Hmisc")
library("maptools")
library("car")
library("RColorBrewer")
library("colorspace") 
library("RANN")
library("Imap")
library("usdm")
library("corrgram")
library("weights")

#*************************************************

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 0.) Variablen zentrieren für Regressionen ------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

LOR4reg@data$MietechgrC              <- scale(LOR4reg@data$Mietechgr,scale=F)
LOR4reg@data$Miete.2007C             <- scale(LOR4reg@data$Miete.2007,scale=F)
LOR4reg@data$ArmutchgC               <- scale(LOR4reg@data$Armutchg,scale=F)
LOR4reg@data$Armut.2007C             <- scale(LOR4reg@data$Armut.2007,scale=F)
LOR4reg@data$AlosechgC               <- scale(LOR4reg@data$Alosechg,scale=F)
LOR4reg@data$Alose.2007C             <- scale(LOR4reg@data$Alose.2007,scale=F)
LOR4reg@data$PDAU5.2007C             <- scale(LOR4reg@data$PDAU5.2007,scale=F)
LOR4reg@data$PDAU10.2007C            <- scale(LOR4reg@data$PDAU10.2007,scale=F)
LOR4reg@data$E_U18R.2007C            <- scale(LOR4reg@data$E_U18R.2007,scale=F)
LOR4reg@data$E_18U35R.2007C          <- scale(LOR4reg@data$E_18U35R.2007,scale=F)
LOR4reg@data$E_65U110R.2007C         <- scale(LOR4reg@data$E_65U110R.2007,scale=F)
LOR4reg@data$AlleinerzHH.2012C       <- scale(LOR4reg@data$AlleinerzHH.2012,scale=F)
LOR4reg@data$StaedtWohnungen.2012C   <- scale(LOR4reg@data$StaedtWohnungen.2012,scale=F)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# I.) Nachbarschaftslisten ------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# beliebige Distanz berechnen
# weil gdist nur long/lat erkennt müssen wir zunächst umprojezieren auf wgs84
LORwgs84       <- spTransform(LORshape4reg,CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")) 
coordslonglat  <- coordinates(LORwgs84)
# Distanz zwischen 1. & 11. Polygon
gdist(coordslonglat[1,1], coordslonglat[1,2], coordslonglat[11,1], coordslonglat[11,2], units="m") 
# --> In LOR4reg sind die units schon Meter!

# a.) Creating Contiguity Neighbours ----

# Erstellen von Nachbarschaftslisten
coords    <- coordinates(LOR4reg)
IDs       <- as.character(LOR4reg@data$RAUMID_NAME)
LORpoly_nb<- poly2nb(LOR4reg, row.names = IDs); LORpoly_nb
summary.nb(LORpoly_nb)

# Visualisierung der Nachbarschaftslisten
plot(LOR4reg, col = "grey")
plot(LORpoly_nb,coords,add=T,col="red")
title(main = "poly Nachbarschaftsplot")

# b.) Creating Distance-Based Neighbours ----

# Erstellen von Nachbarschaftslisten
LORk1_nb <- knn2nb(knearneigh(coords, k = 1), row.names = IDs)
dsts <- unlist(nbdists(LORk1_nb, coords))
summary(dsts)
max_k1 <- max(dsts)
max_k1

LOR1000m_nb <- dnearneigh(coords, d1 = 0, d2 = 1000,
                        row.names = IDs)
LOR1500m_nb <- dnearneigh(coords, d1 = 0, d2 = 1500,
                          row.names = IDs)
LOR2000m_nb  <- dnearneigh(coords, d1 = 0, d2 = 2000,
                          row.names = IDs)
LORmax_k1_nb <- dnearneigh(coords, d1 = 0, d2 = max_k1,
                      row.names = IDs)

# Visualisierung der Nachbarschaftslisten

#par(mfrow=c(2,2))
#par(mfrow=c(1,1))
plot(LOR4reg, col = "grey")
plot(LOR1000m_nb,coords,add=T,col="red")
title(main = "1000m Nachbarschaftsplot")

plot(LOR4reg, col = "grey")
plot(LOR1500m_nb,coords,add=T,col="red")
title(main = "1500m Nachbarschaftsplot")

plot(LOR4reg, col = "grey")
plot(LOR2000m_nb,coords,add=T,col="red")
title(main = "2000m Nachbarschaftsplot")

plot(LOR4reg, col = "grey")
plot(LORmax_k1_nb,coords,add=T,col="red")
title(main = "max_k1_nb Nachbarschaftsplot")

table(card(LOR1000m_nb))
table(card(LOR1500m_nb))
table(card(LOR2000m_nb))
table(card(LORmax_k1_nb))

hist(card(LOR1000m_nb), breaks=25)
hist(card(LOR1500m_nb), breaks=25)
hist(card(LOR2000m_nb), breaks=25)
hist(card(LORmax_k1_nb), breaks=25)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# II.) Räumliche Gewichtungsmatrizen ------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Bivand p.254
dsts <- nbdists(LORpoly_nb, coordinates(LORshape4reg))
idw <- lapply(dsts, function(x) 1/x)
W_polyIDWs <- nb2listw(LORpoly_nb, glist = idw, style = "S")
summary(unlist(W_polyIDWs$weights))
summary(sapply(W_polyIDWs$weights, sum))
print(W_polyIDWs)
str(W_polyIDWs$weights)

dsts <- nbdists(LORpoly_nb, coordinates(LORshape4reg))
idw <- lapply(dsts, function(x) 1/x)
W_polyIDWc <- nb2listw(LORpoly_nb, glist = idw, style = "C")
summary(unlist(W_polyIDWc$weights))
summary(sapply(W_polyIDWc$weights, sum))
print(W_polyIDWs)
str(W_polyIDWs$weights)

dsts <- nbdists(LOR1500m_nb, coordinates(LORshape4reg))
idw <- lapply(dsts, function(x) 1/x)
W_1500mIDWs <- nb2listw(LOR1500m_nb, glist = idw, style = "S", zero.policy=T)
summary(unlist(W_1500mIDWs$weights))
summary(sapply(W_1500mIDWs$weights, sum))
print(W_1500mIDWs, zero.policy=T)
str(W_1500mIDWs$weights)

dsts <- nbdists(LOR1500m_nb, coordinates(LORshape4reg))
idw <- lapply(dsts, function(x) 1/x)
W_1500mIDWc <- nb2listw(LOR1500m_nb, glist = idw, style = "C", zero.policy=T)
summary(unlist(W_1500mIDWc$weights))
summary(sapply(W_1500mIDWc$weights, sum))
print(W_1500mIDWc, zero.policy=T)
str(W_1500mIDWc$weights)

dsts <- nbdists(LOR2000m_nb, coordinates(LORshape4reg))
idw <- lapply(dsts, function(x) 1/x)
W_2000mIDWs <- nb2listw(LOR2000m_nb, glist = idw, style = "S", zero.policy=T)
summary(unlist(W_2000mIDWs$weights))
summary(sapply(W_2000mIDWs$weights, sum))
print(W_2000mIDWs, zero.policy=T)
str(W_2000mIDWs$weights)

dsts <- nbdists(LOR2000m_nb, coordinates(LORshape4reg))
idw <- lapply(dsts, function(x) 1/x)
W_2000mIDWc <- nb2listw(LOR2000m_nb, glist = idw, style = "C", zero.policy=T)
summary(unlist(W_2000mIDWc$weights))
summary(sapply(W_2000mIDWc$weights, sum))
print(W_2000mIDWc, zero.policy=T)
str(W_2000mIDWc$weights)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# III.) LM Regression und Moran's I Test ------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

lm1 <- lm(formula=FortzuegeR ~ MietechgrC + ArmutchgC + Miete.2007C+ Armut.2007C +  
            AlleinerzHH.2012C +
            E_U18R.2007C+ E_18U35R.2007C+ E_65U110R.2007C+ 
            PDAU10.2007C+ StaedtWohnungen.2012C+ SanGebiet.2007 + STADTRAUM,
   data=LOR4reg@data,
   weights=E_E.2007)
summary(lm1)

moran.test(LOR4reg@data$FortzuegeR, listw = W_polyIDWs, na.action=na.omit)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# IV.) VIF step um Multicollinerarity innerhalb der Predictors auszuschließen ------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# PDAU5.2007C und PDAU10.2007C geht nicht --> nur PDAU10.2007C
# Alose_langzeit und Armut.2007C geht nicht --> Armut.2007C
# Altersarmut und Armut.2007C geht nicht --> Armut.2007C
# --> alle Alose/Armutschg raus ausser --> Armutchg
# Miete.2007 und Mietechgr muss geklärt werden

predictors4vifcheck <- subset(LOR4reg@data, 
                              select=c(MietechgrC, Miete.2007C,
                                       ArmutchgC, Armut.2007C,
                                       nicht_Alose_Hartzchg, Alose_langzeitchg , Alose_u25chg ,
                                       Altersarmut.2012,  Alose_langzeit.2012, 
                                       StaedtWohnungen.2012C, AlleinerzHH.2012 , 
                                       PDAU5.2007C, PDAU10.2007C, PDAU5chg, 
                                       E_U18R.2007C, E_18U35R.2007C, E_65U110R.2007C,
                                       dist2STADTMITTE))

predictors4vifcheck <- subset(LOR4reg@data, 
                              select=c(MietechgrC, Miete.2007C,
                                       ArmutchgC, Armut.2007C,
                                       StaedtWohnungen.2012C, AlleinerzHH.2012 , 
                                       PDAU10.2007C, PDAU5chg, 
                                       E_U18R.2007C, E_18U35R.2007C, E_65U110R.2007C,
                                       dist2STADTMITTE))                                                           

predictors4vifcheck <- subset(LOR4reg@data,
                              select=c(Mietechgr,Alosechg,
                                         Miete.2007,Armut.2007,
                                         E_U18R.2007,E_18U35R.2007,E_65U110R.2007,
                                         PDAU10.2007,StaedtWohnungen.2012, AlleinerzHH.2012))

corrgram(predictors4vifcheck, order=FALSE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Correlations") 

str(predictors4vifcheck)
vifstep1 <- vifstep(predictors4vifcheck, th=5) ; vifstep1
vifstep2 <- vifstep(predictors4vifcheck, th=10); vifstep2

vifcor1 <- vifcor(predictors4vifcheck, th=0.7) ; vifcor1
vifcor2 <- vifcor(predictors4vifcheck, th=0.7); vifcor2

vifcor(predictors4vifcheck, th=5)# identify collinear variables that should be excluded
predictors <- exclude(predictors4vifcheck, vifstep1)
predictors <- exclude(predictors4vifcheck, vifstep2)
names(predictors)

#Mietechgr, Armutchg, STADTRAUM, Sanierung

levels(LOR4reg@data$SanGebiet_KLASSE.2012)

lm1 <- lm(formula=FortzuegeR ~ AlosechgC + MietechgrC +   Miete.2007C +        
          StaedtWohnungen.2012C+   Alose.2007C+  E_U18R.2007C +
            E_18U35R.2007C + E_65U110R.2007C +  PDAU10.2007C +  AlleinerzHH.2012C,
          data=LOR4reg@data,
          weights=E_E.2012)
summary(lm1)

lm2 <- lm(formula=FortzuegeR ~ MietechgrC+ Miete.2007C+ ArmutchgC+ Armut.2007C+ StaedtWohnungen.2012C+ AlleinerzHH.2012C + 
PDAU10.2007C+ E_U18R.2007C+ E_18U35R.2007C+ E_65U110R.2007C+ #dist2STADTMITTE 
  + STADTRAUM + SanGebiet.2007,
data=LOR4reg@data,
weights=E_E.2007)
summary(lm2)

weight <- qsec/sum(qsec)
ggpairs(mtcars[ ,c("mpg", "wt", "disp", "qsec")], columns = 1:3, size = "qsec", weight="qsec",
        lower=list(continuous="smooth", params=c(colour="blue")),
        diag=list(continuous="bar", params=c(colour="blue")),
        upper=list(params=list(corSize=8)))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# V.) Scatterplots generieren mit Response ------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

corrDF <- subset(LOR4reg@data, select=c(FortzuegeR, Mietechgr, Armutchg, Alosechg))
names(corrDF)

corrgram(corrDF, order=FALSE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Correlations") 

corrgram(corrDF, order=FALSE, lower.panel=panel.ellipse,
         upper.panel=panel.pts, text.panel=panel.txt,
         diag.panel=panel.minmax,
         main="Correlations") 


#wpct()
#wtd.cors(x, y=NULL, weight=NULL)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# V.) SAR Spatialerror Model ------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#LOR4reg$RAUMID_NAME=="Blankenfelde" hier fehlt Miete.2007 und Miete.2008. Kicken wir raus.

dsts <- nbdists(LOR1500m_nb, coordinates(LORshape4reg))
idw <- lapply(dsts, function(x) 1/x)
W_1500mIDWs <- nb2listw(LOR1500m_nb, glist = idw, style = "S", zero.policy=T)
summary(unlist(W_1500mIDWs$weights))
summary(sapply(W_1500mIDWs$weights, sum))
print(W_1500mIDWs, zero.policy=T)
str(W_1500mIDWs$weights)

SAR_1 <- spautolm(formula=FortzuegeR ~ MietechgrC + ArmutchgC + Miete.2007C+ Armut.2007C +  
                    AlleinerzHH.2012C +
                    E_U18R.2007C+ E_18U35R.2007C+ E_65U110R.2007C+ 
                    PDAU10.2007C+ StaedtWohnungen.2012C+ SanGebiet.2007 + STADTRAUM,
                  data=LOR4reg,
                  listw=W_polyIDWs, 
                  weights=E_E.2007,
                  family="SAR")
summary(SAR_1, Nagelkerke=T)
#qqnorm(SAR_1$fit$residuals/(SAR_1$weights^2)); qqline(SAR_1$fit$residuals/(SAR_1$weights^2), col = 2)
#qqnorm(SAR_1$fit$signal_stochastic); qqline(SAR_1$fit$signal_stochastic, col = 2)

SAR_2 <- spautolm(formula=FortzuegeR ~ MietechgrC + ArmutchgC + Miete.2007C+ Armut.2007C +  
                    AlleinerzHH.2012C +
                    E_U18R.2007C+ E_18U35R.2007C+ E_65U110R.2007C+ 
                    PDAU10.2007C+ StaedtWohnungen.2012C+ SanGebiet.2007 + STADTRAUM,
                  data=LOR4reg,
                  listw=W_1500mIDWs, 
                  weights=E_E.2007,
                  family="SAR")
summary(SAR_2, Nagelkerke=T)

SAR_3 <- spautolm(formula=FortzuegeR ~ MietechgrC + ArmutchgC + Miete.2007C+ Armut.2007C +  
                   AlleinerzHH.2012C +
                   E_U18R.2007C+ E_18U35R.2007C+ E_65U110R.2007C+ 
                   PDAU10.2007C+ StaedtWohnungen.2012C+ SanGebiet.2007 + STADTRAUM,
                 data=LOR4reg,
                 listw=W_2000mIDWs, 
                 weights=E_E.2007,
                 family="SAR")
summary(SAR_3, Nagelkerke=T)
#qqnorm(SAR5$fit$residuals/(SAR5$weights^2)); qqline(SAR5$fit$residuals/(SAR5$weights^2), col = 2)
#qqnorm(SAR5$fit$signal_stochastic); qqline(SAR5$fit$signal_stochastic, col = 2)

SAR_4 <- spautolm(formula=FortzuegeUDAR ~ MietechgrC + ArmutchgC + Miete.2007C+ Armut.2007C +  
                    AlleinerzHH.2012C +
                    E_U18R.2007C+ E_18U35R.2007C+ E_65U110R.2007C+ 
                    PDAU10.2007C+ StaedtWohnungen.2012C+ SanGebiet.2007 + STADTRAUM,
                  data=LOR4reg,
                  listw=W_polyIDWs, 
                  weights=E_E.2007,
                  family="SAR")
summary(SAR_4, Nagelkerke=T)

SAR_5 <- spautolm(formula=ZuzuegeR ~ MietechgrC + ArmutchgC + Miete.2007C+ Armut.2007C +  
                    AlleinerzHH.2012C +
                    E_U18R.2007C+ E_18U35R.2007C+ E_65U110R.2007C+ 
                    PDAU10.2007C+ StaedtWohnungen.2012C+ SanGebiet.2007 + STADTRAUM,
                  data=LOR4reg,
                  listw=W_polyIDWs, 
                  weights=E_E.2007,
                  family="SAR")
summary(SAR_5, Nagelkerke=T)

SAR_6 <- spautolm(formula=ZuzuegeUDAR ~ MietechgrC + ArmutchgC + Miete.2007C+ Armut.2007C +  
                    AlleinerzHH.2012C +
                    E_U18R.2007C+ E_18U35R.2007C+ E_65U110R.2007C+ 
                    PDAU10.2007C+ StaedtWohnungen.2012C+ SanGebiet.2007 + STADTRAUM,
                  data=LOR4reg,
                  listw=W_polyIDWs, 
                  weights=E_E.2007,
                  family="SAR")
summary(SAR_6, Nagelkerke=T)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# VI.) Residuenplots ------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### SAR_1 = FortzügeB W_poly ###
data.frame(SAR_1$fit$residuals/sqrt(var(SAR_1$fit$residuals)),
           SAR_1$weights) -> SARResidPlotDF
names(SARResidPlotDF) <- c("res","E_E.2007")

data.frame(rnorm(n = 1000000,
                 mean = 0,
                 sd = 1)) -> rnormPlotDF
names(rnormPlotDF) <- c("rnorm")

SAR_1_ResPLOT <- ggplot(SARResidPlotDF, aes(x=res)) + 
  geom_histogram(aes(y=..density..),   
                 colour="grey50", fill="white", 
                 binwidth=0.1,
                 weight="E_E.2007") +
  geom_density(alpha=.3, fill="orange", colour="grey50", size=0.7, weight="E_E.2007") +
  geom_density(data=rnormPlotDF, aes(x=rnorm),alpha=.2, fill="blue",size=0.7) +
  theme(axis.title.y = element_text(face="bold",colour = "grey50"),
        axis.title.x = element_text(face="bold",colour = "grey50")) +
  scale_x_continuous(breaks=seq(-4,14,2), limits=c(-5,14))  +
  scale_y_continuous(breaks=seq(-0,1,0.2), limits=c(0,0.8))  +
  xlab("Gewichtete Residuen") + 
  ylab("Dichte") +
  geom_vline(xintercept = 0, colour="black", linetype = "longdash", size=0.5) +
  annotate("text", x = 8, y = 0.6, label = "FortzügeB (Modell 1)", size=4, face="bold",colour = "grey50") 
# SAR_1_ResPLOT

### SAR_4 = FortzügeA W_poly ###
data.frame(SAR_4$fit$residuals/sqrt(var(SAR_4$fit$residuals)),
           SAR_4$weights) -> SARResidPlotDF
names(SARResidPlotDF) <- c("res","E_E.2007")

data.frame(rnorm(n = 1000000,
                 mean = 0,
                 sd = 1)) -> rnormPlotDF
names(rnormPlotDF) <- c("rnorm")

SAR_4_ResPLOT <- ggplot(SARResidPlotDF, aes(x=res)) + 
  geom_histogram(aes(y=..density..),   
                 colour="grey50", fill="white", 
                 binwidth=0.1,
                 weight="E_E.2007") +
  geom_density(alpha=.3, fill="orange", colour="grey50", size=0.7, weight="E_E.2007") +
  geom_density(data=rnormPlotDF, aes(x=rnorm),alpha=.2, fill="blue",size=0.7) +
  theme(axis.title.y = element_text(face="bold",colour = "grey50"),
        axis.title.x = element_text(face="bold",colour = "grey50")) +
  scale_x_continuous(breaks=seq(-4,14,2), limits=c(-5,14))  +
  scale_y_continuous(breaks=seq(-0,1,0.2), limits=c(0,0.8))  +
  xlab("Gewichtete Residuen") + 
  ylab("Dichte") +  
  geom_vline(xintercept = 0, colour="black", linetype = "longdash", size=0.5) +
  annotate("text", x = 8, y = 0.6, label = "FortzügeA", size=4, face="bold",colour = "grey50") 
#SAR_4_ResPLOT

### SAR_5 = ZuzügeB W_poly ###
data.frame(SAR_5$fit$residuals/sqrt(var(SAR_5$fit$residuals)),
           SAR_5$weights) -> SARResidPlotDF
names(SARResidPlotDF) <- c("res","E_E.2007")

data.frame(rnorm(n = 1000000,
                 mean = 0,
                 sd = 1)) -> rnormPlotDF
names(rnormPlotDF) <- c("rnorm")

SAR_5_ResPLOT <- ggplot(SARResidPlotDF, aes(x=res)) + 
  geom_histogram(aes(y=..density..),   
                 colour="grey50", fill="white", 
                 binwidth=0.1,
                 weight="E_E.2007") +
  geom_density(alpha=.3, fill="orange", colour="grey50", size=0.7, weight="E_E.2007") +
  geom_density(data=rnormPlotDF, aes(x=rnorm),alpha=.2, fill="blue",size=0.7) +
  theme(axis.title.y = element_text(face="bold",colour = "grey50"),
        axis.title.x = element_text(face="bold",colour = "grey50")) +
  scale_x_continuous(breaks=seq(-4,14,2), limits=c(-5,14))  +
  scale_y_continuous(breaks=seq(-0,1,0.2), limits=c(0,0.8))  +
  xlab("Gewichtete Residuen") + 
  ylab("Dichte") +
  geom_vline(xintercept = 0, colour="black", linetype = "longdash", size=0.5) +
  annotate("text", x = 8, y = 0.6, label = "ZuzügeB", size=4, face="bold",colour = "grey50") 
#SAR_5_ResPLOT


### SAR_6 = ZuzügeA W_poly ###
data.frame(SAR_6$fit$residuals/sqrt(var(SAR_6$fit$residuals)),
           SAR_6$weights) -> SARResidPlotDF
names(SARResidPlotDF) <- c("res","E_E.2007")

data.frame(rnorm(n = 1000000,
                 mean = 0,
                 sd = 1)) -> rnormPlotDF
names(rnormPlotDF) <- c("rnorm")

SAR_6_ResPLOT <- ggplot(SARResidPlotDF, aes(x=res)) + 
  geom_histogram(aes(y=..density..),   
                 colour="grey50", fill="white", 
                 binwidth=0.1,
                 weight="E_E.2007") +
  geom_density(alpha=.3, fill="orange", colour="grey50", size=0.7, weight="E_E.2007") +
  geom_density(data=rnormPlotDF, aes(x=rnorm),alpha=.2, fill="blue",size=0.7) +
  theme(axis.title.y = element_text(face="bold",colour = "grey50"),
        axis.title.x = element_text(face="bold",colour = "grey50")) +
  scale_x_continuous(breaks=seq(-4,14,2), limits=c(-5,14))  +
  scale_y_continuous(breaks=seq(-0,1,0.2), limits=c(0,0.8))  +
  xlab("Gewichtete Residuen") + 
  ylab("Dichte") +
  geom_vline(xintercept = 0, colour="black", linetype = "longdash", size=0.5) +
  annotate("text", x = 8, y = 0.6, label = "ZuzügeA", size=4, face="bold",colour = "grey50") 
#SAR_6_ResPLOT

grid.arrange(SAR_1_ResPLOT,SAR_4_ResPLOT,SAR_5_ResPLOT,SAR_6_ResPLOT,
             ncol=2,nrow=2,widths=c(1,1,1,1))


...........................................................
###################### \\\\ Ohne Ausreißer \\\\ ####################
#...................................................................

# fiese Residuenausreisser (>abs(4))
summary(abs(SAR_1$fit$signal_stochastic)-abs(SAR_1$fit$residuals)) # durchschnittliche Verbesserung des fits durch SAR 0.58
subset(LOR4reg@data,SAR_1epsilon>4,select=c("RAUMID_NAME","SAR_1epsilon","FortzuegeR"))
subset(LOR4reg@data,SAR_1epsilon>4,select=c("RAUMID_NAME","SAR_1epsilon","FortzuegeR"))[,1]

LOR4regCLEAN<- LOR4reg[LOR4reg@data$RAUMID_NAME!="Plötzensee" &
        LOR4reg@data$RAUMID_NAME!="Park Ruhwald" &
        LOR4reg@data$RAUMID_NAME!="Marienfelder Allee Nordwest" &
        LOR4reg@data$RAUMID_NAME!="Treuenbrietzener Str",] 

spplot(LOR4regCLEAN,zcol="Gentri")
dim(LOR4regCLEAN@data)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# I.) Nachbarschaftslisten ------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# beliebige Distanz berechnen
# weil gdist nur long/lat erkennt müssen wir zunächst umprojezieren auf wgs84
LORwgs84       <- spTransform(LORshape4reg,CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")) 
coordslonglat  <- coordinates(LORwgs84)
# Distanz zwischen 1. & 11. Polygon
gdist(coordslonglat[1,1], coordslonglat[1,2], coordslonglat[11,1], coordslonglat[11,2], units="m") 
# --> In LOR4regCLEAN sind die units schon Meter!

# a.) Creating Contiguity Neighbours ----

# Erstellen von Nachbarschaftslisten
coords    <- coordinates(LOR4regCLEAN)
IDs       <- as.character(LOR4regCLEAN@data$RAUMID_NAME)
LORpoly_nb<- poly2nb(LOR4regCLEAN, row.names = IDs); LORpoly_nb
summary.nb(LORpoly_nb)

# Visualisierung der Nachbarschaftslisten
plot(LOR4regCLEAN, col = "grey")
plot(LORpoly_nb,coords,add=T,col="red")
title(main = "poly Nachbarschaftsplot")

# b.) Creating Distance-Based Neighbours ----

# Erstellen von Nachbarschaftslisten
LORk1_nb <- knn2nb(knearneigh(coords, k = 1), row.names = IDs)
dsts <- unlist(nbdists(LORk1_nb, coords))
summary(dsts)
max_k1 <- max(dsts)
max_k1

LOR1000m_nb <- dnearneigh(coords, d1 = 0, d2 = 1000,
                          row.names = IDs)
LOR1500m_nb <- dnearneigh(coords, d1 = 0, d2 = 1500,
                          row.names = IDs)
LOR2000m_nb  <- dnearneigh(coords, d1 = 0, d2 = 2000,
                           row.names = IDs)
LORmax_k1_nb <- dnearneigh(coords, d1 = 0, d2 = max_k1,
                           row.names = IDs)

# Visualisierung der Nachbarschaftslisten

#par(mfrow=c(2,2))
#par(mfrow=c(1,1))
plot(LOR4regCLEAN, col = "grey")
plot(LOR1000m_nb,coords,add=T,col="red")
title(main = "1000m Nachbarschaftsplot")

plot(LOR4regCLEAN, col = "grey")
plot(LOR1500m_nb,coords,add=T,col="red")
title(main = "1500m Nachbarschaftsplot")

plot(LOR4regCLEAN, col = "grey")
plot(LOR2000m_nb,coords,add=T,col="red")
title(main = "2000m Nachbarschaftsplot")

plot(LOR4regCLEAN, col = "grey")
plot(LORmax_k1_nb,coords,add=T,col="red")
title(main = "max_k1_nb Nachbarschaftsplot")

table(card(LOR1000m_nb))
table(card(LOR1500m_nb))
table(card(LOR2000m_nb))
table(card(LORmax_k1_nb))

hist(card(LOR1000m_nb), breaks=25)
hist(card(LOR1500m_nb), breaks=25)
hist(card(LOR2000m_nb), breaks=25)
hist(card(LORmax_k1_nb), breaks=25)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# II.) Räumliche Gewichtungsmatrizen ------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Bivand p.254
dsts <- nbdists(LORpoly_nb, coordinates(LOR4regCLEAN))
idw <- lapply(dsts, function(x) 1/x)
W_polyIDWs <- nb2listw(LORpoly_nb, glist = idw, style = "S")
summary(unlist(W_polyIDWs$weights))
summary(sapply(W_polyIDWs$weights, sum))
print(W_polyIDWs)
str(W_polyIDWs$weights)

dsts <- nbdists(LOR1500m_nb, coordinates(LOR4regCLEAN))
idw <- lapply(dsts, function(x) 1/x)
W_1500mIDWs <- nb2listw(LOR1500m_nb, glist = idw, style = "S", zero.policy=T)
summary(unlist(W_1500mIDWs$weights))
summary(sapply(W_1500mIDWs$weights, sum))
print(W_1500mIDWs, zero.policy=T)
str(W_1500mIDWs$weights)

dsts <- nbdists(LOR2000m_nb, coordinates(LOR4regCLEAN))
idw <- lapply(dsts, function(x) 1/x)
W_2000mIDWs <- nb2listw(LOR2000m_nb, glist = idw, style = "S", zero.policy=T)
summary(unlist(W_2000mIDWs$weights))
summary(sapply(W_2000mIDWs$weights, sum))
print(W_2000mIDWs, zero.policy=T)
str(W_2000mIDWs$weights)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# V.) SAR Spatialerror Model ------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dsts <- nbdists(LOR1500m_nb, coordinates(LORshape4reg))
idw <- lapply(dsts, function(x) 1/x)
W_1500mIDWs <- nb2listw(LOR1500m_nb, glist = idw, style = "S", zero.policy=T)
summary(unlist(W_1500mIDWs$weights))
summary(sapply(W_1500mIDWs$weights, sum))
print(W_1500mIDWs, zero.policy=T)
str(W_1500mIDWs$weights)

SAR_1 <- spautolm(formula=FortzuegeR ~ MietechgrC + ArmutchgC + Miete.2007C+ Armut.2007C +  
                    AlleinerzHH.2012C +
                    E_U18R.2007C+ E_18U35R.2007C+ E_65U110R.2007C+ 
                    PDAU10.2007C+ StaedtWohnungen.2012C+ SanGebiet.2007 + STADTRAUM,
                  data=LOR4regCLEAN,
                  listw=W_polyIDWs, 
                  weights=E_E.2007,
                  family="SAR")
summary(SAR_1, Nagelkerke=T)

SAR_2 <- spautolm(formula=FortzuegeR ~ MietechgrC + ArmutchgC + Miete.2007C+ Armut.2007C +  
                    AlleinerzHH.2012C +
                    E_U18R.2007C+ E_18U35R.2007C+ E_65U110R.2007C+ 
                    PDAU10.2007C+ StaedtWohnungen.2012C+ SanGebiet.2007 + STADTRAUM,
                  data=LOR4regCLEAN,
                  listw=W_1500mIDWs, 
                  weights=E_E.2007,
                  family="SAR")
summary(SAR_2, Nagelkerke=T)

SAR_3 <- spautolm(formula=FortzuegeR ~ MietechgrC + ArmutchgC + Miete.2007C+ Armut.2007C +  
                    AlleinerzHH.2012C +
                    E_U18R.2007C+ E_18U35R.2007C+ E_65U110R.2007C+ 
                    PDAU10.2007C+ StaedtWohnungen.2012C+ SanGebiet.2007 + STADTRAUM,
                  data=LOR4regCLEAN,
                  listw=W_2000mIDWs, 
                  weights=E_E.2007,
                  family="SAR")
summary(SAR_3, Nagelkerke=T)

qqnorm(SAR_1$fit$residuals/(SAR_1$weights^2)); qqline(SAR_1$fit$residuals/(SAR_1$weights^2), col = 2)
qqnorm(SAR_1$fit$signal_stochastic); qqline(SAR_1$fit$signal_stochastic, col = 2)

SAR_4 <- spautolm(formula=ZuzuegeR ~ MietechgrC + ArmutchgC + Miete.2007C+ Armut.2007C +  
                    AlleinerzHH.2012C +
                    E_U18R.2007C+ E_18U35R.2007C+ E_65U110R.2007C+ 
                    PDAU10.2007C+ StaedtWohnungen.2012C+ SanGebiet.2007 + STADTRAUM,
                  data=LOR4regCLEAN,
                  listw=W_1500mIDWs, 
                  weights=E_E.2007,
                  family="SAR")
summary(SAR_4, Nagelkerke=T)

SAR_5 <- spautolm(formula=FortzuegeUDAR ~ MietechgrC + ArmutchgC + Miete.2007C+ Armut.2007C +  
                    AlleinerzHH.2012C +
                    E_U18R.2007C+ E_18U35R.2007C+ E_65U110R.2007C+ 
                    PDAU10.2007C+ StaedtWohnungen.2012C+ SanGebiet.2007 + STADTRAUM,
                  data=LOR4regCLEAN,
                  listw=W_2000mIDWs, 
                  weights=E_E.2007,
                  family="SAR")
summary(SAR_5, Nagelkerke=T)

SAR_6 <- spautolm(formula=ZuzuegeUDAR ~ MietechgrC + ArmutchgC + Miete.2007C+ Armut.2007C +  
                    AlleinerzHH.2012C +
                    E_U18R.2007C+ E_18U35R.2007C+ E_65U110R.2007C+ 
                    PDAU10.2007C+ StaedtWohnungen.2012C+ SanGebiet.2007 + STADTRAUM,
                  data=LOR4regCLEAN,
                  listw=W_2000mIDWs, 
                  weights=E_E.2007,
                  family="SAR")
summary(SAR_6, Nagelkerke=T)

