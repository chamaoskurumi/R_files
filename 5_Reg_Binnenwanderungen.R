#*************************************************
#*************************************************
#
# Regressionen Binnenwandeurngen
#
#*************************************************
#*************************************************

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

I#*************************************************

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# I.) Nachbarschaftslisten ------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# beliebige Distanz berechnen
# weil gdist nur long/lat erkennt müssen wir zunächst umprojezieren auf wgs84
LORwgs84       <- spTransform(LORshape,CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")) 
coordslonglat  <- coordinates(LORwgs84)
# Distanz zwischen 1 & 11 Polygon
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
dsts <- nbdists(LORpoly_nb, coordinates(LORshape))
idw <- lapply(dsts, function(x) 1/x)
W_polyIDWs <- nb2listw(LORpoly_nb, glist = idw, style = "S")
summary(unlist(W_polyIDWs$weights))
summary(sapply(W_polyIDWs$weights, sum))
print(W_polyIDWs)
str(W_polyIDWs$weights)

dsts <- nbdists(LORpoly_nb, coordinates(LORshape))
idw <- lapply(dsts, function(x) 1/x)
W_polyIDWc <- nb2listw(LORpoly_nb, glist = idw, style = "C")
summary(unlist(W_polyIDWc$weights))
summary(sapply(W_polyIDWc$weights, sum))
print(W_polyIDWs)
str(W_polyIDWs$weights)

dsts <- nbdists(LOR1500m_nb, coordinates(LORshape))
idw <- lapply(dsts, function(x) 1/x)
W_1500mIDWs <- nb2listw(LOR1500m_nb, glist = idw, style = "S")
summary(unlist(W_1500mIDWs$weights))
summary(sapply(W_1500mIDWs$weights, sum))
print(W_1500mIDWs)
str(W_1500mIDWs$weights)

dsts <- nbdists(LOR1500m_nb, coordinates(LORshape))
idw <- lapply(dsts, function(x) 1/x)
W_1500mIDWc <- nb2listw(LOR1500m_nb, glist = idw, style = "C")
summary(unlist(W_1500mIDWc$weights))
summary(sapply(W_1500mIDWc$weights, sum))
print(W_1500mIDWc)
str(W_1500mIDWc$weights)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# III.) LM Regression und Moran's I Test ------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

lm1 <- lm(formula=FortzuegeR ~ STADTRAUM + StaedtWohnungen.2012 +
                               Alosechg + Alose.2012 + Miete.2012 + SanGebiet_KLASSE.2012 + 
                               AlleinerzHH.2012 + GentriOLD,
   data=LOR4reg@data,
   weights=E_E.2012)
summary(lm1)

lm2 <- lm(formula=FortzuegeR ~ STADTRAUM + StaedtWohnungen.2012 +
            Alose.2012 + Gentri + 
            AlleinerzHH.2012,
          data=LOR4reg@data,
          weights=E_E.2012)
summary(lm2)

moran.test(LOR4reg@data$FortzuegeR, listw = W_polyIDWs, na.action=na.omit)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# IV.) VIF step um Multicollinerarity innerhalb der Predictors auszuschließen ------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

v2 <- vifstep(r, th=10) # identify collinear variables that should be excluded
v2
re2 <- exclude(r, v2) # e

ggpairs(mtcars[ ,c("mpg", "wt", "disp", "qsec")], columns = 1:3, size = "qsec")
library("GGally")
data(iris)
ggpairs(iris[, 1:4], lower=list(continuous="smooth", params=c(colour="blue")),
        diag=list(continuous="bar", params=c(colour="blue")), 
        upper=list(params=list(corSize=6)), axisLabels='show')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# V.) Scatterplots generieren mit Response ------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# V.) SAR Spatialerror Model ------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

spautolm(formula=,
         data=LOR4reg,
         weights=E_E.2012,
         family="SAR",
         correlation=T)
