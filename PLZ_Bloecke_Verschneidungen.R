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

PLZ2008      <- PLZ; length(PLZ2008@data$PLZ)
PLZ2009      <- PLZ
PLZ2010      <- PLZ
PLZ2011      <- PLZ
PLZ2012      <- PLZ
PLZ2013      <- PLZ
PLZ2010_2013 <- PLZ

PLZ2008@data <- merge(PLZ2008@data, GSW2008, all.x=T);PLZ2008@data$Zeit <- "2008"
PLZ2009@data <- merge(PLZ2009@data, GSW2009, all.x=T);PLZ2009@data$Zeit <- "2009"
PLZ2010@data <- merge(PLZ2010@data, GSW2010, all.x=T);PLZ2010@data$Zeit <- "2010"
PLZ2011@data <- merge(PLZ2011@data, GSW2011, all.x=T);PLZ2011@data$Zeit <- "2011"
PLZ2012@data <- merge(PLZ2012@data, GSW2012, all.x=T);PLZ2012@data$Zeit <- "2012"
PLZ2013@data <- merge(PLZ2013@data, GSW2013, all.x=T);PLZ2013@data$Zeit <- "2013"
table(PLZ2010_2013df$Zeit)
#spplot(PLZ2013, zcol="GSWmiete_kaltMEDIAN", col.regions = rev(heat.colors(200)))

PLZ2010_2013df <- data.frame(rbind(PLZ2010@data,
                                   PLZ2011@data,
                                   PLZ2012@data,
                                   PLZ2013@data))
#View(PLZ2010_2013df)

PLZ2010_2013dfwide <- reshape(PLZ2010_2013df,                 
                            idvar = c("PLZ", "FLAECHE_HA"),
                            v.names = c("GSWwhgsgroesse",
                                        "GSWmiete_kalt",
                                        "GSWmiete_kaltMEDIAN",
                                        "GSWangebote"),
                            timevar = "Zeit",
                            direction = "wide")
#View(PLZ2010_2013wide)

PLZ2010_2013@data <- merge(PLZ2010_2013@data, PLZ2010_2013dfwide, all.x=T)


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
# 2010-2013
#+++++++++++++

bloeckePLZ10_13_ptdf <- SpatialPointsDataFrame(coords = bloecke10_13_pt, 
                                               data   = data.frame(bloecke10_13_ptdf@data,
                                                                   over(bloecke10_13_ptdf, PLZ2010_2013)),
                                               proj4string = zielCRS) 
head(bloeckePLZ10_13_ptdf@data)
# Für soviele Blöcke hat die Zoordnung nicht funktioniert:
dim(bloeckePLZ10_13_ptdf@data[is.na(bloeckePLZ10_13_ptdf@data$PLZ),])[1]
# D.h. soviele Einwohner können nicht zugeordenet werden:
nichtZuordenbarEW10_13 <- sum(bloeckePLZ10_13_ptdf@data[is.na(bloeckePLZ10_13_ptdf@data$PLZ),]["EW2013"])
nichtZuordenbarEW10_13 
# Das entspricht einem nicht zugeeordnetem Anteil an der Berliner Gesamtbevölkerung:
round(nichtZuordenbarEW10_13/sum(bloeckePLZ10_13_ptdf@data$EW2013), digits = 3)
# --> Ditt is ja nuscht. 57 Leute nur!

plot(bloeckePLZ10_13_ptdf[!is.na(bloeckePLZ10_13_ptdf@data$PLZ),], col="green", pch = 4, cex=0.5)
plot(LOR, add=T)
plot(bloeckePLZ10_13_ptdf[is.na(bloeckePLZ10_13_ptdf@data$PLZ),],add=T, cex=1, col="red", pch = 16)

#plot(PLZ2010_13)
#plot(bloeckePLZ10_13_ptdf[is.na(bloeckePLZ10_13_ptdf@data$PLZ),], add=T, col="red", pch = 2)
#plot(PLZ2010_13)
#plot(bloecke10_13, add=T, col="red")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Blockdaten den LORs zuschreiben und auf LOR Ebene mit EW Zahl der Blöcke gewichtet aggregieren  -------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#+++++++++++++
# 2008
#+++++++++++++

bloecke2LOR_08 <- data.frame(bloeckePLZ08_ptdf,over(bloeckePLZ08_ptdf, LORshape))
bloecke2LOR_08 <- subset(bloecke2LOR_08, select=-c(FLAECHE_HA, FLAECHE_HA.1, x, y))
names(bloecke2LOR_08)
#str(bloecke2LOR_08)

LOR_GSWagg_08 <- ddply(bloecke2LOR_08, 
      "RAUMID", summarise, 
      Whgsgroesse_wmean.2008 = round(weighted.mean(GSWwhgsgroesse, EINWOHNER), digits=2),
      Miete_wmean.2008       = round(weighted.mean(GSWmiete_kalt, EINWOHNER), digits=2))
head(LOR_GSWagg_08)
LOR_GSWagg_08 <- subset(LOR_GSWagg_09, !is.na(LOR_GSWagg_08$RAUMID))
sum(is.na(LOR_GSWagg_08$Miete_wmean.2008)) # für soviele LORs fehlen uns die Mietpreisdaten

#+++++++++++++
# 2009
#+++++++++++++

bloecke2LOR_09 <- data.frame(bloeckePLZ09_ptdf,over(bloeckePLZ09_ptdf, LORshape))
bloecke2LOR_09 <- subset(bloecke2LOR_09, select=-c(FLAECH_HA, FLAECHE_HA, x, y))
names(bloecke2LOR_09)
#str(bloecke2LOR_09)

LOR_GSWagg_09 <- ddply(bloecke2LOR_09, 
                       "RAUMID", summarise, 
                       Whgsgroesse_wmean.2009 = round(weighted.mean(GSWwhgsgroesse, EW_GESAMT), digits=2),
                       Miete_wmean.2009       = round(weighted.mean(GSWmiete_kalt, EW_GESAMT), digits=2))
head(LOR_GSWagg_09)
LOR_GSWagg_09 <- subset(LOR_GSWagg_09, !is.na(LOR_GSWagg_09$RAUMID))
sum(is.na(LOR_GSWagg_09$Miete_wmean.2009)) # für soviele LORs fehlen uns die Mietpreisdaten

#+++++++++++++
# 2010-2013
#+++++++++++++

bloecke2LOR_10_13 <- data.frame(bloeckePLZ10_13_ptdf,over(bloeckePLZ10_13_ptdf, LORshape))
bloecke2LOR_10_13 <- subset(bloecke2LOR_10_13, select=-c(FLAECHE_HA, x, y))
names(bloecke2LOR_10_13)
#str(bloecke2LOR_10_13)

LOR_GSWagg_10_13 <- ddply(bloecke2LOR_10_13, 
                       "RAUMID", summarise, 
                       # 2010 #
                       Whgsgroesse_wmean.2010 = round(weighted.mean(GSWwhgsgroesse.2010, EW2010), digits=2),
                       MieteMEDIAN_wmean.2010 = round(weighted.mean(GSWmiete_kaltMEDIAN.2010, EW2010), digits=2),
                       # 2011 #
                       Whgsgroesse_wmean.2011 = round(weighted.mean(GSWwhgsgroesse.2011, EW2011), digits=2),
                       MieteMEDIAN_wmean.2011 = round(weighted.mean(GSWmiete_kaltMEDIAN.2011, EW2011), digits=2),
                       # 2012 #
                       Whgsgroesse_wmean.2012 = round(weighted.mean(GSWwhgsgroesse.2012, EW2012), digits=2),
                       MieteMEDIAN_wmean.2012 = round(weighted.mean(GSWmiete_kaltMEDIAN.2012, EW2012), digits=2),
                       # 2013 #
                       Whgsgroesse_wmean.2013 = round(weighted.mean(GSWwhgsgroesse.2013, EW2013), digits=2),
                       MieteMEDIAN_wmean.2013 = round(weighted.mean(GSWmiete_kaltMEDIAN.2013, EW2013), digits=2))
sum(is.na(LOR_GSWagg_10_13$MieteMEDIAN_wmean.2010)) # für soviele LORs fehlen uns die Mietpreisdaten 2010
sum(is.na(LOR_GSWagg_10_13$MieteMEDIAN_wmean.2011)) # für soviele LORs fehlen uns die Mietpreisdaten 2011
sum(is.na(LOR_GSWagg_10_13$MieteMEDIAN_wmean.2012)) # für soviele LORs fehlen uns die Mietpreisdaten 2012
sum(is.na(LOR_GSWagg_10_13$MieteMEDIAN_wmean.2013)) # für soviele LORs fehlen uns die Mietpreisdaten 2013
LOR_GSWagg_10_13 <- subset(LOR_GSWagg_10_13, !is.na(LOR_GSWagg_10_13$RAUMID))
names(LOR_GSWagg_10_13)

#merge(LOR_GSWagg08, by="RAUMID"))

LOR_GSWagg <- join_all(list(LOR_GSWagg_08,
                            LOR_GSWagg_09,
                            LOR_GSWagg_10_13), 
                            by = "RAUMID")
head(LOR_GSWagg)
str(LOR_GSWagg)

plot(LORshape, add=T, lty=2)
plot(PLZ, lwd=3)


