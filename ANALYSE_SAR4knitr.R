#*************************************************
#*************************************************
#
# SAR Regressionen 4 knitr
#
#*************************************************
#*************************************************

#setwd("/home/dao/Desktop/MasterArbeit/R_files/KNITR/")
#load(file = "FULL_FINAL_WORKSPACE.Rdata")

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

# a.) Creating Contiguity Neighbours ----

# Erstellen von Nachbarschaftslisten
coords    <- coordinates(LOR4reg)
IDs       <- as.character(LOR4reg@data$RAUMID_NAME)
LORpoly_nb<- poly2nb(LOR4reg, row.names = IDs); LORpoly_nb

# b.) Creating Distance-Based Neighbours ----

# Erstellen von Nachbarschaftslisten
LOR1500m_nb <- dnearneigh(coords, d1 = 0, d2 = 1500,
                          row.names = IDs)
LOR2000m_nb  <- dnearneigh(coords, d1 = 0, d2 = 2000,
                           row.names = IDs)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# II.) Räumliche Gewichtungsmatrizen ------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Bivand p.254
dsts <- nbdists(LORpoly_nb, coordinates(LOR4reg))
idw <- lapply(dsts, function(x) 1/x)
W_polyIDWs <- nb2listw(LORpoly_nb, glist = idw, style = "S")
#summary(unlist(W_polyIDWs$weights))
#summary(sapply(W_polyIDWs$weights, sum))


dsts <- nbdists(LOR1500m_nb, coordinates(LOR4reg))
idw <- lapply(dsts, function(x) 1/x)
W_1500mIDWs <- nb2listw(LOR1500m_nb, glist = idw, style = "S", zero.policy=T)
#summary(unlist(W_1500mIDWs$weights))
#summary(sapply(W_1500mIDWs$weights, sum))


dsts <- nbdists(LOR2000m_nb, coordinates(LOR4reg))
idw <- lapply(dsts, function(x) 1/x)
W_2000mIDWs <- nb2listw(LOR2000m_nb, glist = idw, style = "S", zero.policy=T)
#summary(unlist(W_2000mIDWs$weights))
#summary(sapply(W_2000mIDWs$weights, sum))


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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# V.) SAR Spatialerror Model ------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SAR_1 <- spautolm(formula=FortzuegeR ~ Mietechgr + Armutchg + Miete.2007C+ Armut.2007C +  
                    AlleinerzHH.2012C +
                    E_U18R.2007C+ E_18U35R.2007C+ E_65U110R.2007C+ 
                    PDAU10.2007C+ StaedtWohnungen.2012C+ SanGebiet.2007 + STADTRAUM,
                  data=LOR4reg,
                  listw=W_polyIDWs, 
                  weights=E_E.2007,
                  family="SAR")
summary(SAR_1, Nagelkerke=T)


SAR_2 <- spautolm(formula=FortzuegeR ~ Mietechgr + Armutchg + Miete.2007C+ Armut.2007C +  
                    AlleinerzHH.2012C +
                    E_U18R.2007C+ E_18U35R.2007C+ E_65U110R.2007C+ 
                    PDAU10.2007C+ StaedtWohnungen.2012C+ SanGebiet.2007 + STADTRAUM,
                  data=LOR4reg,
                  listw=W_1500mIDWs, 
                  weights=E_E.2007,
                  family="SAR")
summary(SAR_2, Nagelkerke=T)

SAR_3 <- spautolm(formula=FortzuegeR ~ Mietechgr + Armutchg + Miete.2007C+ Armut.2007C +  
                    AlleinerzHH.2012C +
                    E_U18R.2007C+ E_18U35R.2007C+ E_65U110R.2007C+ 
                    PDAU10.2007C+ StaedtWohnungen.2012C+ SanGebiet.2007 + STADTRAUM,
                  data=LOR4reg,
                  listw=W_2000mIDWs, 
                  weights=E_E.2007,
                  family="SAR")
summary(SAR_3, Nagelkerke=T)

SAR_4 <- spautolm(formula=FortzuegeUDAR ~ Mietechgr + Armutchg + Miete.2007C+ Armut.2007C +  
                    AlleinerzHH.2012C +
                    E_U18R.2007C+ E_18U35R.2007C+ E_65U110R.2007C+ 
                    PDAU10.2007C+ StaedtWohnungen.2012C+ SanGebiet.2007 + STADTRAUM,
                  data=LOR4reg,
                  listw=W_polyIDWs, 
                  weights=E_E.2007,
                  family="SAR")
summary(SAR_4, Nagelkerke=T)

SAR_5 <- spautolm(formula=ZuzuegeR ~ Mietechgr + Armutchg + Miete.2007C+ Armut.2007C +  
                    AlleinerzHH.2012C +
                    E_U18R.2007C+ E_18U35R.2007C+ E_65U110R.2007C+ 
                    PDAU10.2007C+ StaedtWohnungen.2012C+ SanGebiet.2007 + STADTRAUM,
                  data=LOR4reg,
                  listw=W_polyIDWs, 
                  weights=E_E.2007,
                  family="SAR")
summary(SAR_5, Nagelkerke=T)

SAR_6 <- spautolm(formula=ZuzuegeUDAR ~ Mietechgr + Armutchg + Miete.2007C+ Armut.2007C +  
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

abschnitt <- -0.2

### SAR_1 = FortzügeB W_poly ###
data.frame(scale(SAR_1$fit$residuals),
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
  geom_density(alpha=.3, fill="orange", colour="grey50", size=0.3, weight="E_E.2007") +
  geom_density(data=rnormPlotDF, aes(x=rnorm),alpha=.2, fill="blue",size=0.3) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x= element_blank(),
        axis.ticks.x= element_blank(),
        plot.margin=unit(c(abschnitt,abschnitt,abschnitt ,abschnitt), "cm")) +
  scale_x_continuous(breaks=seq(-4,14,2), limits=c(-5,14))  +
  scale_y_continuous(breaks=seq(-0,1,0.2), limits=c(0,1))  +
  geom_vline(xintercept = 0, colour="black", linetype = "longdash", size=0.3) +
  annotate("text", x = 8, y = 0.6, label = "FortzügeB (Modell 1)", size=4, face="bold",colour = "grey50") +
  geom_rug(aes(x=res),colour="grey10",size=0.5,alpha=0.5)
#SAR_1_ResPLOT

### SAR_4 = FortzügeA W_poly ###
data.frame(scale(SAR_4$fit$residuals),
           SAR_4$weights) -> SARResidPlotDF
names(SARResidPlotDF) <- c("res","E_E.2007")

SAR_4_ResPLOT <- ggplot(SARResidPlotDF, aes(x=res)) + 
  geom_histogram(aes(y=..density..),   
                 colour="grey50", fill="white", 
                 binwidth=0.1,
                 weight="E_E.2007") +
  geom_density(alpha=.3, fill="orange", colour="grey50", size=0.3, weight="E_E.2007") +
  geom_density(data=rnormPlotDF, aes(x=rnorm),alpha=.2, fill="blue",size=0.3) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y= element_blank(),
        axis.ticks.y= element_blank(),
        axis.text.x= element_blank(),
        axis.ticks.x= element_blank(),
        plot.margin=unit(c(abschnitt,abschnitt,abschnitt ,abschnitt), "cm")) +
  scale_x_continuous(breaks=seq(-4,14,2), limits=c(-5,14))  +
  scale_y_continuous(breaks=seq(-0,1,0.2), limits=c(0,1))  +
  geom_vline(xintercept = 0, colour="black", linetype = "longdash", size=0.3) +
  annotate("text", x = 8, y = 0.6, label = "FortzügeA", size=4, face="bold",colour = "grey50") +
  geom_rug(aes(x=res),colour="grey10",size=0.5,alpha=0.5)
#SAR_4_ResPLOT

### SAR_5 = ZuzügeB W_poly ###
data.frame(scale(SAR_5$fit$residuals),
           SAR_5$weights) -> SARResidPlotDF
names(SARResidPlotDF) <- c("res","E_E.2007")

SAR_5_ResPLOT <- ggplot(SARResidPlotDF, aes(x=res)) + 
  geom_histogram(aes(y=..density..),   
                 colour="grey50", fill="white", 
                 binwidth=0.1,
                 weight="E_E.2007") +
  geom_density(alpha=.3, fill="orange", colour="grey50", size=0.3, weight="E_E.2007") +
  geom_density(data=rnormPlotDF, aes(x=rnorm),alpha=.2, fill="blue",size=0.3) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin=unit(c(abschnitt,abschnitt,abschnitt ,abschnitt), "cm")) +
  scale_x_continuous(breaks=seq(-4,14,2), limits=c(-5,14))  +
  scale_y_continuous(breaks=seq(-0,1,0.2), limits=c(0,1))  +
  geom_vline(xintercept = 0, colour="black", linetype = "longdash", size=0.3) +
  annotate("text", x = 8, y = 0.6, label = "ZuzügeB", size=4, face="bold",colour = "grey50") +
  geom_rug(aes(x=res),colour="grey10",size=0.5,alpha=0.5)
#SAR_5_ResPLOT


### SAR_6 = ZuzügeA W_poly ###
data.frame(scale(SAR_6$fit$residuals),
           SAR_6$weights) -> SARResidPlotDF
names(SARResidPlotDF) <- c("res","E_E.2007")

SAR_6_ResPLOT <- ggplot(SARResidPlotDF, aes(x=res)) + 
  geom_histogram(aes(y=..density..),   
                 colour="grey50", fill="white", 
                 binwidth=0.1,
                 weight="E_E.2007") +
  geom_density(alpha=.3, fill="orange", colour="grey50", size=0.3, weight="E_E.2007") +
  geom_density(data=rnormPlotDF, aes(x=rnorm),alpha=.2, fill="blue",size=0.3) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y= element_blank(),
        axis.ticks.y= element_blank(),
        plot.margin=unit(c(abschnitt,abschnitt,abschnitt ,abschnitt), "cm")) +
  scale_x_continuous(breaks=seq(-4,14,2), limits=c(-5,14))  +
  scale_y_continuous(breaks=seq(-0,1,0.2), limits=c(0,1))  +
  geom_vline(xintercept = 0, colour="black", linetype = "longdash", size=0.3) +
  annotate("text", x = 8, y = 0.6, label = "ZuzügeA", size=4, face="bold",colour = "grey50")  +
  geom_rug(aes(x=res),colour="grey10",size=0.5,alpha=0.5)
#SAR_6_ResPLOT

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# VII.) Residuenkarten ------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library("scales");library("plyr")
LOR4reg@data$SAR_1epsilon  <- SAR_1$fit$residuals
LOR4reg@data$SAR_1e        <- SAR_1$fit$signal_stochastic#+SAR_1$fit$residuals

LOR4regLONGLAT <- spTransform(LOR4reg,CRS("+proj=longlat")) #+ellps=WGS84 +datum=WGS84 +no_defs")) 
LOR4reg.fort <- fortify(LOR4regLONGLAT, region="RAUMID_NAME")
LOR4regdf <- LOR4reg@data
colnames(LOR4regdf)[2] <- "id"
LOR4reg.fort <- join(LOR4reg.fort, LOR4regdf, by="id")

kartenlayout <- list(geom_polygon(aes(group=id),data=BZK.fort, fill=NA, colour="grey50", alpha=0.8,size=0.5),
                     theme(legend.position =c(0.9,0.8), 
                           line = element_blank(), 
                           rect= element_blank(),
                           axis.line=element_blank(),
                           axis.text.x=element_blank(),
                           axis.text.y=element_blank(),
                           axis.ticks=element_blank(),
                           axis.title.x=element_blank(),
                           axis.title.y=element_blank(),
                           legend.title=element_text(size=rel(0.7), face="bold"),
                           legend.text=element_text(size=rel(0.5)),
                           plot.margin=unit(c(0,0,-0.7,0), "cm")),
                     guides(fill = guide_colourbar(barheight=5)),
                     geom_path(data=S_Bahn.fort, aes(x=long, y=lat), linetype=2, size=0.6))

lowCOLOR <- "blue"
midCOLOR <- "#fee0d2"
hiCOLOR  <- "red"
superhiCOLOR <- "black"

ResiduenEMAP <- ggplot(LOR4reg.fort, aes(x=long, y=lat, group = id)) + 
  geom_polygon(aes(fill=SAR_1e, group=id)) + geom_path(color="grey", alpha=0.0, size=0.5) +
  scale_fill_gradientn(colours = c(lowCOLOR,midCOLOR,hiCOLOR), 
                       values = rescale(c(-1,0,2)),
                       guide = "colorbar", limits=c(-1,2),
                       name=expression(paste('Räuml. Komponente'))) +
  coord_map("polyconic",xlim = c(13.08,13.77),ylim = c(52.33,52.69)) + kartenlayout
#ResiduenEMAP

ResiduenEpsilonMAP <- ggplot(LOR4reg.fort, aes(x=long, y=lat, group = id)) + 
  geom_polygon(aes(fill=SAR_1epsilon, group=id)) + geom_path(color="grey", alpha=0.0, size=0.5) +
  scale_fill_gradientn(colours = c(lowCOLOR,midCOLOR,hiCOLOR,superhiCOLOR), 
                       values = rescale(c(-4,0,4,20)),
                       guide = "colorbar", limits=c(-5,20),
                       name=expression(paste('Residuen',' ',epsilon))) +
  coord_map("polyconic",xlim = c(13.08,13.77),ylim = c(52.33,52.69)) + kartenlayout 
#ResiduenEpsilonMAP

summary(abs(SAR_1$fit$signal_stochastic)-abs(SAR_1$fit$residuals)) # durchschnittliche Verbesserung des fits durch SAR 0.58
subset(LOR4reg@data,SAR_1epsilon>4,select=c("RAUMID_NAME","SAR_1epsilon","FortzuegeR"))
subset(LOR4reg@data,SAR_1epsilon>4,select=c("RAUMID_NAME","SAR_1epsilon","FortzuegeR"))[,1]
