#********************************************
#                                           #
#   PLZ-Blöcke Räumliche Verschneidungen    #
#                                           #
#********************************************

library("rgdal")
library("rgeos")
library("sp")

#~~~~~~~~~~~~~~~~~~~~~~~~
# GSW Daten einlesen -------------------
#~~~~~~~~~~~~~~~~~~~~~~~~

setwd(dir = "/home/dao/Desktop/MasterArbeit/R_data")
GSWdata <- read.table("GSW_-PLZ-/GSW_Daten_long.csv", header = TRUE, sep=",", fill=TRUE, dec=",",
                      na.strings="NA", stringsAsFactors=F)
GSWdata$PLZ  <- as.factor(GSWdata$PLZ); length(levels(GSWdata$PLZ))
GSWdata$Zeit <- as.factor(GSWdata$Zeit); levels(GSWdata$Zeit)
#View(GSWdata)
str(GSWdata)

#~~~~~~~~~~~~~~~~~~~~~~~~
# Postleitzahlen PLZ -------------------
#~~~~~~~~~~~~~~~~~~~~~~~~

setwd("/home/dao/Desktop/MasterArbeit/GentriMap/4 Geodaten")
PLZ1     <- readOGR(dsn="PLZ_GS/RBS_OD_PLZ_01_2014/", layer="RBS_OD_PLZ_1312", encoding = "UTF-8")
proj4string(PLZ1)    <- CRS("+proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs") # EPSG:25833
PLZ    <- spTransform(PLZ1, zielCRS)
#plot(PLZ)


#~~~~~~~~~~~~~~~~~~~~~~~~
# GSW Data & PLZ mergen -------------------
#~~~~~~~~~~~~~~~~~~~~~~~~

GSWdata <- subset(GSWdata, select=-Bezirk)
GSW2008 <- GSWdata[GSWdata$Zeit==2008,]
GSW2008 <- unique(GSW2008); length(GSW2008$PLZ)
GSW2008$PLZ <- factor(GSW2008$PLZ); length(levels(GSW2008$PLZ))
GSW2009 <- GSWdata[GSWdata$Zeit==2009,]
GSW2009 <- unique(GSW2009); length(GSW2009$PLZ)
GSW2009$PLZ <- factor(GSW2009$PLZ); length(levels(GSW2009$PLZ))
GSW2010 <- GSWdata[GSWdata$Zeit==2010,]
GSW2010 <- unique(GSW2010); length(GSW2010$PLZ)
GSW2010$PLZ <- factor(GSW2010$PLZ); length(levels(GSW2010$PLZ))
GSW2011 <- GSWdata[GSWdata$Zeit==2011,]
GSW2011 <- unique(GSW2011); length(GSW2011$PLZ)
GSW2011$PLZ <- factor(GSW2011$PLZ); length(levels(GSW2011$PLZ))
GSW2012 <- GSWdata[GSWdata$Zeit==2012,]
GSW2012 <- unique(GSW2012); length(GSW2012$PLZ)
GSW2012$PLZ <- factor(GSW2012$PLZ); length(levels(GSW2012$PLZ))
GSW2013 <- GSWdata[GSWdata$Zeit==2013,]
GSW2013 <- unique(GSW2013); length(GSW2013$PLZ)
GSW2013$PLZ <- factor(GSW2013$PLZ); length(levels(GSW2013$PLZ))

PLZ2008 <- PLZ; length(PLZ2008@data$PLZ)
PLZ2009 <- PLZ
PLZ2010 <- PLZ
PLZ2011 <- PLZ
PLZ2012 <- PLZ
PLZ2013 <- PLZ

PLZ2008@data <- merge(PLZ2008@data, GSW2008, all.x=T)
PLZ2009@data <- merge(PLZ2009@data, GSW2009, all.x=T)
PLZ2010@data <- merge(PLZ2010@data, GSW2010, all.x=T)
PLZ2011@data <- merge(PLZ2011@data, GSW2011, all.x=T)
PLZ2012@data <- merge(PLZ2012@data, GSW2012, all.x=T)
PLZ2013@data <- merge(PLZ2013@data, GSW2013, all.x=T)

GSWdata2010_2013 <- rbind(GSW2010, GSW2011, GSW2012, GSW2013)
GSWdata2010_2013$PLZ  <- as.factor(GSWdata2010_2013$PLZ)
GSWdata2010_2013$Zeit <- as.factor(GSWdata2010_2013$Zeit)
str(GSWdata2010_2013)

subset(GSWdata, Zeit=="2010" | Zeit=="2011"| Zeit=="2012" | Zeit=="2013" ) 
GSWdata2010_2013 <- arrange(GSWdata2010_2013, PLZ, Zeit)
GSWdata2010_2013wide <- reshape(GSWdata2010_2013,
                   idvar = c("PLZ"),
                   v.names = c("GSWwhgsgroesse","GSWmiete_kalt",
                               "GSWmiete_kaltMEDIAN","GSWangebote"),
                   timevar = "Zeit",
                   direction = "wide")
View(GSWdata2010_2013wide)

GSWdata2010_2013wide[GSWdata2010_2013wide$PLZ=="13166",]

which(!mapply(identical,levels(GSW2012$PLZ),levels(GSW2013$PLZ)))

View(cbind(levels(GSW2010$PLZ), levels(GSW2011$PLZ), levels(GSW2012$PLZ), levels(GSW2013$PLZ)))


str(unique(GSWdata2010_2013wide))
table(GSWdata2010_2013wide$PLZ)
spplot(PLZ2013, zcol="GSWmiete_kaltMEDIAN", col.regions = heat.colors(200))

(GSW2013$PLZ)
length(PLZ@data$PLZ)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# bloecke in SpatialPoints verwandeln -------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

zielCRS <- CRS("+proj=cass +lat_0=52.41864827777778 +lon_0=13.62720366666667 
                +x_0=40000 +y_0=10000 +datum=potsdam +units=m
                +no_defs +ellps=bessel +towgs84=598.1,73.7,418.2,0.202,0.045,-2.455,6.7 "))

bloecke08_pt    <- gCentroid(bloecke08    ,byid=TRUE); plot(bloecke08_pt)
bloecke08_ptdf  <- SpatialPointsDataFrame(coords = bloecke08_pt, 
                                          data = bloecke08@data, 
                                          proj4string = zielCRS)

bloecke09_pt    <- gCentroid(bloecke09    ,byid=TRUE)
bloecke09_ptdf  <- SpatialPointsDataFrame(coords = bloecke09_pt, 
                                          data = bloecke09@data, 
                                          proj4string = zielCRS)
                                          
bloecke10_13_pt    <- gCentroid(bloecke10_13 ,byid=TRUE)
bloecke10_13_ptdf  <- SpatialPointsDataFrame(coords = bloecke10_13_pt, 
                                          data = bloecke10_13@data, 
                                          proj4string = zielCRS)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Overlay von bloecke-Schwerpunkten in PLZ shpfile: Zuordnung bloecke <--> PLZ -------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#+++++++++++++
# 2008
#+++++++++++++

bloeckePLZ08_ptdf <- SpatialPointsDataFrame(coords = bloecke08_pt, 
                                            data   = data.frame(bloecke08_ptdf@data,
                                                                over(bloecke08_ptdf, PLZ2008)),
                                            proj4string = zielCRS) 
head(bloeckePLZ08_ptdf@data)
# Für soviele Blöcke hat die Zoordnung nicht funktioniert:
dim(bloeckePLZ08_ptdf@data[is.na(bloeckePLZ08_ptdf@data$PLZ),])[1]
# D.h. soviele Einwohner können nicht zugeordenet werden:
nichtZuordenbarEW08 <- sum(bloeckePLZ08_ptdf@data[is.na(bloeckePLZ08_ptdf@data$PLZ),]["EINWOHNER"])
nichtZuordenbarEW08 
# Das entspricht einem nicht zugeeordnetem Anteil an der Berliner Gesamtbevölkerung:
round(nichtZuordenbarEW08/sum(bloeckePLZ08_ptdf@data$EINWOHNER), digits = 3)
# --> Eh nicht schlimm, da genau andere Blöcke in unmittelbarer Nachbarschaft 
#     gültig zugeordnet werden konnten. Die daraus resultierende Verzerrung ist minimal, wie der 
#     plot gegenüber den LORs zeigt 

plot(bloeckePLZ08_ptdf[!is.na(bloeckePLZ08_ptdf@data$PLZ),], col="green", pch = 4, cex=0.5)
plot(LOR, add=T)
plot(bloeckePLZ08_ptdf[is.na(bloeckePLZ08_ptdf@data$PLZ),],add=T, cex=1, col="red", pch = 16)

#plot(PLZ2008)
#plot(bloeckePLZ08_ptdf[is.na(bloeckePLZ08_ptdf@data$PLZ),], add=T, col="red", pch = 2)
#plot(PLZ2008)
#plot(bloecke08, add=T, col="red")

# Ein Versuch per polygon-polygon overlay die bloecke den richtigen PLZs zuzuordnen per rgeos::over
#   stellte sich als sehr kompliziert heraus. 


#+++++++++++++
# 2009
#+++++++++++++

bloeckePLZ09_ptdf <- SpatialPointsDataFrame(coords = bloecke09_pt, 
                                            data   = data.frame(bloecke09_ptdf@data,
                                                                over(bloecke09_ptdf, PLZ2009)),
                                            proj4string = zielCRS) 
head(bloeckePLZ09_ptdf@data)
# Für soviele Blöcke hat die Zoordnung nicht funktioniert:
dim(bloeckePLZ09_ptdf@data[is.na(bloeckePLZ09_ptdf@data$PLZ),])[1]
# D.h. soviele Einwohner können nicht zugeordenet werden:
nichtZuordenbarEW09 <- sum(bloeckePLZ09_ptdf@data[is.na(bloeckePLZ09_ptdf@data$PLZ),]["EW_GESAMT"])
nichtZuordenbarEW09 
# Das entspricht einem nicht zugeeordnetem Anteil an der Berliner Gesamtbevölkerung:
round(nichtZuordenbarEW09/sum(bloeckePLZ09_ptdf@data$EW_GESAMT), digits = 3)
# --> Eh nicht schlimm, da genau andere Blöcke in unmittelbarer Nachbarschaft 
#     gültig zugeordnet werden konnten. Die daraus resultierende Verzerrung ist minimal, wie der 
#     plot gegenüber den LORs zeigt 

plot(bloeckePLZ09_ptdf[!is.na(bloeckePLZ09_ptdf@data$PLZ),], col="green", pch = 4, cex=0.5)
plot(LOR, add=T)
plot(bloeckePLZ09_ptdf[is.na(bloeckePLZ09_ptdf@data$PLZ),],add=T, cex=1, col="red", pch = 16)

#plot(PLZ2009)
#plot(bloeckePLZ09_ptdf[is.na(bloeckePLZ09_ptdf@data$PLZ),], add=T, col="red", pch = 2)
#plot(PLZ2009)
#plot(bloecke09, add=T, col="red")


#+++++++++++++
# 2010 -2013
#+++++++++++++

bloeckePLZ10_13_ptdf <- SpatialPointsDataFrame(coords = bloecke10_13_pt, 
                                               data   = data.frame(bloecke10_13_ptdf@data,
                                                                   over(bloecke10_13_ptdf, PLZ2010_13)),
                                               proj4string = zielCRS) 
head(bloeckePLZ10_13_ptdf@data)
# Für soviele Blöcke hat die Zoordnung nicht funktioniert:
dim(bloeckePLZ10_13_ptdf@data[is.na(bloeckePLZ10_13_ptdf@data$PLZ),])[1]
# D.h. soviele Einwohner können nicht zugeordenet werden:
nichtZuordenbarEW10_13 <- sum(bloeckePLZ10_13_ptdf@data[is.na(bloeckePLZ10_13_ptdf@data$PLZ),]["EINWOHNER"])
nichtZuordenbarEW10_13 
# Das entspricht einem nicht zugeeordnetem Anteil an der Berliner Gesamtbevölkerung:
round(nichtZuordenbarEW10_13/sum(bloeckePLZ10_13_ptdf@data$EINWOHNER), digits = 3)
# --> Eh nicht schlimm, da genau andere Blöcke in unmittelbarer Nachbarschaft 
#     gültig zugeordnet werden konnten. Die daraus resultierende Verzerrung ist minimal, wie der 
#     plot gegenüber den LORs zeigt 

plot(bloeckePLZ10_13_ptdf[!is.na(bloeckePLZ10_13_ptdf@data$PLZ),], col="green", pch = 4, cex=0.5)
plot(LOR, add=T)
plot(bloeckePLZ10_13_ptdf[is.na(bloeckePLZ10_13_ptdf@data$PLZ),],add=T, cex=1, col="red", pch = 16)

#plot(PLZ2010_13)
#plot(bloeckePLZ10_13_ptdf[is.na(bloeckePLZ10_13_ptdf@data$PLZ),], add=T, col="red", pch = 2)
#plot(PLZ2010_13)
#plot(bloecke10_13, add=T, col="red")

# Ein Versuch per polygon-polygon overlay die bloecke den richtigen PLZs zuzuordnen per rgeos::over
#   stellte sich als sehr kompliziert heraus. 



