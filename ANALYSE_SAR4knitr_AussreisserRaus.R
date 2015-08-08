#*************************************************
#*************************************************
#
# SAR Regressionen 4 knitr
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

# nur gültige LORs behalten --> subset by valid=="gültig"
LOR4regA <- subset(LOR4reg, LOR4reg@data$RAUMID_NAME!="Plötzensee" & LOR4reg@data$RAUMID_NAME!="Park Ruhwald")
# update factor levels
LOR4regA@data$RAUMID      <- factor(LOR4regA@data$RAUMID)
LOR4regA@data$RAUMID_NAME <- factor(LOR4regA@data$RAUMID_NAME)
LOR4regA@data$BZR         <- factor(LOR4regA@data$BZR)
LOR4regA@data$BZR_NAME    <- factor(LOR4regA@data$BZR_NAME )
# Shape file für LOR4regA erstellen
LORshape4reg <- SpatialPolygons(LOR4regA@polygons,proj4string=zielCRS)
plot(LORshape4reg)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 0.) Variablen zentrieren für Regressionen ------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

LOR4regA@data$MietechgrC              <- scale(LOR4regA@data$Mietechgr,scale=F)
LOR4regA@data$Miete.2007C             <- scale(LOR4regA@data$Miete.2007,scale=F)
LOR4regA@data$ArmutchgC               <- scale(LOR4regA@data$Armutchg,scale=F)
LOR4regA@data$Armut.2007C             <- scale(LOR4regA@data$Armut.2007,scale=F)
LOR4regA@data$AlosechgC               <- scale(LOR4regA@data$Alosechg,scale=F)
LOR4regA@data$Alose.2007C             <- scale(LOR4regA@data$Alose.2007,scale=F)
LOR4regA@data$PDAU5.2007C             <- scale(LOR4regA@data$PDAU5.2007,scale=F)
LOR4regA@data$PDAU10.2007C            <- scale(LOR4regA@data$PDAU10.2007,scale=F)
LOR4regA@data$E_U18R.2007C            <- scale(LOR4regA@data$E_U18R.2007,scale=F)
LOR4regA@data$E_18U35R.2007C          <- scale(LOR4regA@data$E_18U35R.2007,scale=F)
LOR4regA@data$E_65U110R.2007C         <- scale(LOR4regA@data$E_65U110R.2007,scale=F)
LOR4regA@data$AlleinerzHH.2012C       <- scale(LOR4regA@data$AlleinerzHH.2012,scale=F)
LOR4regA@data$StaedtWohnungen.2012C   <- scale(LOR4regA@data$StaedtWohnungen.2012,scale=F)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# I.) Nachbarschaftslisten ------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# a.) Creating Contiguity Neighbours ----

# Erstellen von Nachbarschaftslisten
coords    <- coordinates(LOR4regA)
IDs       <- as.character(LOR4regA@data$RAUMID_NAME)
LORpoly_nb<- poly2nb(LOR4regA, row.names = IDs); LORpoly_nb

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
dsts <- nbdists(LORpoly_nb, coordinates(LOR4regA))
idw <- lapply(dsts, function(x) 1/x)
W_polyIDWs <- nb2listw(LORpoly_nb, glist = idw, style = "S")
#summary(unlist(W_polyIDWs$weights))
#summary(sapply(W_polyIDWs$weights, sum))


dsts <- nbdists(LOR1500m_nb, coordinates(LOR4regA))
idw <- lapply(dsts, function(x) 1/x)
W_1500mIDWs <- nb2listw(LOR1500m_nb, glist = idw, style = "S", zero.policy=T)
#summary(unlist(W_1500mIDWs$weights))
#summary(sapply(W_1500mIDWs$weights, sum))


dsts <- nbdists(LOR2000m_nb, coordinates(LOR4regA))
idw <- lapply(dsts, function(x) 1/x)
W_2000mIDWs <- nb2listw(LOR2000m_nb, glist = idw, style = "S", zero.policy=T)
#summary(unlist(W_2000mIDWs$weights))
#summary(sapply(W_2000mIDWs$weights, sum))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# III.) LM Regression und Moran's I Test ------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

lm1 <- lm(formula=FortzuegeR ~ Mietechgr + Armutchg + Miete.2007C+ Armut.2007C +  
            AlleinerzHH.2012C +
            E_U18R.2007C+ E_18U35R.2007C+ E_65U110R.2007C+ 
            PDAU10.2007C+ StaedtWohnungen.2012C+ SanGebiet.2007 + STADTRAUM,
          data=LOR4regA@data,
          weights=E_E.2007)
summary(lm1)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# V.) SAR Spatialerror Model ------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SAR_1 <- spautolm(formula=FortzuegeR ~ Mietechgr + Armutchg + Miete.2007C+ Armut.2007C +  
                    AlleinerzHH.2012C +
                    E_U18R.2007C+ E_18U35R.2007C+ E_65U110R.2007C+ 
                    PDAU10.2007C+ StaedtWohnungen.2012C+ SanGebiet.2007 + STADTRAUM,
                  data=LOR4regA,
                  listw=W_polyIDWs, 
                  weights=E_E.2007,
                  family="SAR")
summary(SAR_1, Nagelkerke=T)
