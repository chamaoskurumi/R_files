#********************************************
#********************************************
#********************************************
#                                           #
#       Räumliche Verschneidungen           #
#         Spatial Intersections             #
#                                           #
#********************************************
#********************************************
#********************************************

#install.packages("plyr","devtools","rgeos","reshape2")
#install.packages("gridExtra", "lattice", "latticeExtra)
library("rgdal")
library("rgeos")
library("sp")
library("plyr")
library("reshape2")
library("devtools")
require("gridExtra")
require("lattice")
library("latticeExtra") # For layer spplot

#********************************************
#                                           #
#---------- 1.) PLZ Blöcke   ----------
#                                           #
#********************************************

#~~~~~~~~~~~~~~~~~~~~~~~~
# JLL Daten einlesen -------------------
#~~~~~~~~~~~~~~~~~~~~~~~~

setwd(dir = "/home/dao/Desktop/MasterArbeit/R_data")
JLLdataWIDE <- read.table("GSW_-PLZ-/JonesLangLasalle_Mietpreise_2004-2014_modified.csv", 
                          header = TRUE, sep=";", fill=TRUE, dec=",",
                          na.strings="NA", stringsAsFactors=F)
JLLdataWIDE$PLZ <- as.factor(JLLdataWIDE$PLZ)
JLLdata         <- reshape(JLLdataWIDE,
                           idvar   = "PLZ",
                           varying = names(JLLdataWIDE)[2:23],
                           timevar = "Zeit",
                           sep = ".",
                           direction = "long")
#names(JLLdataWIDE)
#View(JLLdata)
#str(JLLdata)
JLLdata07_12 <- subset(JLLdata, 
                       JLLdata$Zeit>=2007 & JLLdata$Zeit<=2012)
JLLdata07_12$Zeit <- as.factor(JLLdata07_12$Zeit)
#str(JLLdata07_12)


#~~~~~~~~~~~~~~~~~~~~~~~~
# Postleitzahlen PLZ -------------------
#~~~~~~~~~~~~~~~~~~~~~~~~

setwd("/home/dao/Desktop/MasterArbeit/GentriMap/4 Geodaten")
PLZ1     <- readOGR(dsn="PLZ_GS/RBS_OD_PLZ_01_2014/", layer="RBS_OD_PLZ_1312", encoding = "UTF-8")
proj4string(PLZ1)    <- CRS("+proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs") # EPSG:25833
PLZ    <- spTransform(PLZ1, zielCRS)
#plot(PLZ)


#~~~~~~~~~~~~~~~~~~~~~~~~
# JLL Data & PLZ mergen -------------------
#~~~~~~~~~~~~~~~~~~~~~~~~

JLL2007 <- JLLdata[JLLdata$Zeit==2007,]
JLL2007 <- unique(JLL2007); length(JLL2007$PLZ)
JLL2007$PLZ <- factor(JLL2007$PLZ); length(levels(JLL2007$PLZ))

JLL2008 <- JLLdata[JLLdata$Zeit==2008,]
JLL2008 <- unique(JLL2008); length(JLL2008$PLZ)
JLL2008$PLZ <- factor(JLL2008$PLZ); length(levels(JLL2008$PLZ))

JLL2009 <- JLLdata[JLLdata$Zeit==2009,]
JLL2009 <- unique(JLL2009); length(JLL2009$PLZ)
JLL2009$PLZ <- factor(JLL2009$PLZ); length(levels(JLL2009$PLZ))

JLL2010 <- JLLdata[JLLdata$Zeit==2010,]
JLL2010 <- unique(JLL2010); length(JLL2010$PLZ)
JLL2010$PLZ <- factor(JLL2010$PLZ); length(levels(JLL2010$PLZ))

JLL2011 <- JLLdata[JLLdata$Zeit==2011,]
JLL2011 <- unique(JLL2011); length(JLL2011$PLZ)
JLL2011$PLZ <- factor(JLL2011$PLZ); length(levels(JLL2011$PLZ))

JLL2012 <- JLLdata[JLLdata$Zeit==2012,]
JLL2012 <- unique(JLL2012); length(JLL2012$PLZ)
JLL2012$PLZ <- factor(JLL2012$PLZ); length(levels(JLL2012$PLZ))

PLZ2007      <- PLZ; length(PLZ2007@data$PLZ)
PLZ2008      <- PLZ
PLZ2009      <- PLZ
PLZ2010      <- PLZ
PLZ2011      <- PLZ
PLZ2012      <- PLZ
PLZ2010_2012 <- PLZ

#PLZ2007@data$order      <- seq(1:190)
#PLZ2008@data$order      <- seq(1:190)
#PLZ2009@data$order      <- seq(1:190)
#PLZ2010@data$order      <- seq(1:190)
#PLZ2011@data$order      <- seq(1:190)
#PLZ2012@data$order      <- seq(1:190)
#PLZ2010-2012@data$order <- seq(1:190)

identical(levels(JLL2007$PLZ),levels(PLZ@data$PLZ))

source("/home/dao/Desktop/MasterArbeit/R_files/functions/merge_with_order_FUNCTION.R")
# sort=F Option hier extrem wichtig, sonst kommt wird der plotting order im shape file falsch 
# sorf= F würde reichen, aber wir gehen auf Nummer sicher und mergen mit keep_order
PLZ2007@data <- merge(PLZ2007@data, JLL2007, all.x=T,  by="PLZ", 
                      sort=F, keep_order=1);PLZ2007@data$Zeit <- "2007"
PLZ2008@data <- merge(PLZ2008@data, JLL2008, all.x=T,  by="PLZ", 
                      sort=F, keep_order=1);PLZ2008@data$Zeit <- "2008"
PLZ2009@data <- merge(PLZ2009@data, JLL2009, all.x=T,  by="PLZ", 
                      sort=F, keep_order=1);PLZ2009@data$Zeit <- "2009"
PLZ2010@data <- merge(PLZ2010@data, JLL2010, all.x=T,  by="PLZ", 
                      sort=F, keep_order=1);PLZ2010@data$Zeit <- "2010"
PLZ2011@data <- merge(PLZ2011@data, JLL2011, all.x=T,  by="PLZ", 
                      sort=F, keep_order=1);PLZ2011@data$Zeit <- "2011"
PLZ2012@data <- merge(PLZ2012@data, JLL2012, all.x=T,  by="PLZ", 
                      sort=F, keep_order=1);PLZ2012@data$Zeit <- "2012"
table(PLZ2010_2012df$Zeit)

PLZMiete07_plot <- spplot(PLZ2007, zcol="Miete_H2", 
                          col.regions = rev(heat.colors(200)),
                          at=seq(4,15, length.out=201),
                          xlab="Miete 2.HJahr 2007")
PLZMiete08_plot <-  spplot(PLZ2008, zcol="Miete_H2", 
                          col.regions = rev(heat.colors(200)),
                          at=seq(4,15, length.out=201),
                          xlab="Miete 2.HJahr 2008")
PLZMiete09_plot <-  spplot(PLZ2009, zcol="Miete_H2", 
                           col.regions = rev(heat.colors(200)),
                           at=seq(4,15, length.out=201),
                           xlab="Miete 2.HJahr 2009")
PLZMiete10_plot <-  spplot(PLZ2010, zcol="Miete_H2", 
                           col.regions = rev(heat.colors(200)),
                           at=seq(4,15, length.out=201),
                           xlab="Miete 2.HJahr 2010")
PLZMiete11_plot <-  spplot(PLZ2011, zcol="Miete_H2", 
                           col.regions = rev(heat.colors(200)),
                           at=seq(4,15, length.out=201),
                           xlab="Miete 2.HJahr 2011")
PLZMiete12_plot <-  spplot(PLZ2012, zcol="Miete_H2", 
                           col.regions = rev(heat.colors(200)),
                           at=seq(4,15, length.out=201),
                           xlab="Miete 2.HJahr 2012")

# grid.arrange(PLZMiete07_plot,
#              PLZMiete08_plot,
#              PLZMiete09_plot,
#              PLZMiete10_plot,
#              PLZMiete11_plot,
#              PLZMiete12_plot, 
#              nrow=3, ncol=2)

PLZ2010_2012df <- data.frame(rbind(PLZ2010@data,
                                   PLZ2011@data,
                                   PLZ2012@data))
head(PLZ2010_2012df)

PLZ2010_2012dfwide <- reshape(PLZ2010_2012df,                 
                            idvar = c("PLZ", "FLAECHE_HA"),
                            v.names = c("Miete_H1",
                                        "Miete_H2"),
                            timevar = "Zeit",
                            direction = "wide")
#View(PLZ2010_2012dfwide)

PLZ2010_2012@data <- merge.with.order(PLZ2010_2012@data, PLZ2010_2012dfwide, 
                                      all.x=T, sort=F,
                                      keep_order=1)
#head(PLZ2010_2012@data)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# bloecke in SpatialPoints verwandeln -------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

zielCRS <- CRS("+proj=cass +lat_0=52.41864827777778 +lon_0=13.62720366666667 
                +x_0=40000 +y_0=10000 +datum=potsdam +units=m
                +no_defs +ellps=bessel +towgs84=598.1,73.7,418.2,0.202,0.045,-2.455,6.7 ")

bloecke07_pt    <- gCentroid(bloecke07    ,byid=TRUE); plot(bloecke07_pt)
bloecke07_ptdf  <- SpatialPointsDataFrame(coords = bloecke07_pt, 
                                          data = bloecke07@data, 
                                          proj4string = zielCRS)

bloecke08_pt    <- gCentroid(bloecke08    ,byid=TRUE); plot(bloecke08_pt)
bloecke08_ptdf  <- SpatialPointsDataFrame(coords = bloecke08_pt, 
                                          data = bloecke08@data, 
                                          proj4string = zielCRS)

bloecke09_pt    <- gCentroid(bloecke09    ,byid=TRUE)
bloecke09_ptdf  <- SpatialPointsDataFrame(coords = bloecke09_pt, 
                                          data = bloecke09@data, 
                                          proj4string = zielCRS)
                                          
bloecke10_12_pt    <- gCentroid(bloecke10_12 ,byid=TRUE)
bloecke10_12_ptdf  <- SpatialPointsDataFrame(coords = bloecke10_12_pt, 
                                          data = bloecke10_12@data, 
                                          proj4string = zielCRS)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Overlay von bloecke-Schwerpunkten in PLZ shpfile: Zuordnung bloecke <--> PLZ -------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#+++++++++++++
# 2007
#+++++++++++++

bloeckePLZ07_ptdf <- SpatialPointsDataFrame(coords = bloecke07_pt, 
                                            data   = data.frame(bloecke07_ptdf@data,
                                                                over(bloecke07_ptdf, PLZ2007)),
                                            proj4string = zielCRS) 
head(bloeckePLZ07_ptdf@data)
# Für soviele Blöcke hat die Zoordnung nicht funktioniert:
dim(bloeckePLZ07_ptdf@data[is.na(bloeckePLZ07_ptdf@data$PLZ),])[1]
# D.h. soviele Einwohner können nicht zugeordenet werden:
nichtZuordenbarEW07 <- sum(bloeckePLZ07_ptdf@data[is.na(bloeckePLZ07_ptdf@data$PLZ),]["EINWOHNER"])
nichtZuordenbarEW07 
# Das entspricht einem nicht zugeeordnetem Anteil an der Berliner Gesamtbevölkerung:
round(nichtZuordenbarEW07/sum(bloeckePLZ07_ptdf@data$EINWOHNER), digits = 3)
# --> Eh nicht schlimm, da genau andere Blöcke in unmittelbarer Nachbarschaft 
#     gültig zugeordnet werden konnten. Die daraus resultierende Verzerrung ist minimal, wie der 
#     plot gegenüber den LORs zeigt 

plot(bloeckePLZ07_ptdf[!is.na(bloeckePLZ07_ptdf@data$PLZ),], col="green", pch = 4, cex=0.5)
plot(LOR, add=T)
plot(bloeckePLZ07_ptdf[is.na(bloeckePLZ07_ptdf@data$PLZ),],add=T, cex=1, col="red", pch = 16)

#plot(PLZ2007)
#plot(bloeckePLZ07_ptdf[is.na(bloeckePLZ07_ptdf@data$PLZ),], add=T, col="red", pch = 2)
#plot(PLZ2007)
#plot(bloecke07, add=T, col="red")

# Ein Versuch per polygon-polygon overlay die bloecke den richtigen PLZs zuzuordnen per rgeos::over
#   stellte sich als sehr kompliziert heraus. 


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
# 2010-2012
#+++++++++++++

bloeckePLZ10_12_ptdf <- SpatialPointsDataFrame(coords = bloecke10_12_pt, 
                                               data   = data.frame(bloecke10_12_ptdf@data,
                                                                   over(bloecke10_12_ptdf, PLZ2010_2012)),
                                               proj4string = zielCRS) 
head(bloeckePLZ10_12_ptdf@data)
# Für soviele Blöcke hat die Zoordnung nicht funktioniert:
dim(bloeckePLZ10_12_ptdf@data[is.na(bloeckePLZ10_12_ptdf@data$PLZ),])[1]
# D.h. soviele Einwohner können nicht zugeordenet werden:
nichtZuordenbarEW10_12 <- sum(bloeckePLZ10_12_ptdf@data[is.na(bloeckePLZ10_12_ptdf@data$PLZ),]["EW2012"])
nichtZuordenbarEW10_12 
# Das entspricht einem nicht zugeeordnetem Anteil an der Berliner Gesamtbevölkerung:
round(nichtZuordenbarEW10_12/sum(bloeckePLZ10_12_ptdf@data$EW2012), digits = 3)
# --> Ditt is ja nuscht. 57 Leute nur!

plot(bloeckePLZ10_12_ptdf[!is.na(bloeckePLZ10_12_ptdf@data$PLZ),], col="green", pch = 4, cex=0.5)
plot(LOR, add=T)
plot(bloeckePLZ10_12_ptdf[is.na(bloeckePLZ10_12_ptdf@data$PLZ),],add=T, cex=1, col="red", pch = 16)

#plot(PLZ2010_12)
#plot(bloeckePLZ10_12_ptdf[is.na(bloeckePLZ10_12_ptdf@data$PLZ),], add=T, col="red", pch = 2)
#plot(PLZ2010_12)
#plot(bloecke10_12, add=T, col="red")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Blockdaten nach EW Zahl gewichtet auf LOR Niveau aggregieren  -------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#+++++++++++++
# 2007
#+++++++++++++

bloecke2LOR_07 <- data.frame(bloeckePLZ07_ptdf,over(bloeckePLZ07_ptdf, LORshape)); head(bloecke2LOR_07)
bloecke2LOR_07 <- subset(bloecke2LOR_07, select=-c(FLAECHE_HA, FLAECHE_HA.1, x, y))
names(bloecke2LOR_07)
#str(bloecke2LOR_07)

LOR_JLLagg_07 <- ddply(bloecke2LOR_07, 
                       "RAUMID", summarise, 
                       Miete_H1_wmean.2007 = round(weighted.mean(Miete_H1, EINWOHNER), digits=2),
                       Miete_H2_wmean.2007 = round(weighted.mean(Miete_H2, EINWOHNER), digits=2))
head(LOR_JLLagg_07)
LOR_JLLagg_07 <- subset(LOR_JLLagg_07, !is.na(LOR_JLLagg_07$RAUMID))
sum(is.na(LOR_JLLagg_07$Miete_H1_wmean.2007)) # für soviele LORs fehlen uns die Mietpreisdaten
sum(is.na(LOR_JLLagg_07$Miete_H2_wmean.2007)) # für soviele LORs fehlen uns die Mietpreisdaten


#+++++++++++++
# 2008
#+++++++++++++

bloecke2LOR_08 <- data.frame(bloeckePLZ08_ptdf,over(bloeckePLZ08_ptdf, LORshape)); head(bloecke2LOR_08)
bloecke2LOR_08 <- subset(bloecke2LOR_08, select=-c(FLAECHE_HA, FLAECHE_HA.1, x, y))
names(bloecke2LOR_08)
#str(bloecke2LOR_08)

LOR_JLLagg_08 <- ddply(bloecke2LOR_08, 
      "RAUMID", summarise, 
      Miete_H1_wmean.2008 = round(weighted.mean(Miete_H1, EINWOHNER), digits=2),
      Miete_H2_wmean.2008 = round(weighted.mean(Miete_H2, EINWOHNER), digits=2))
head(LOR_JLLagg_08)
LOR_JLLagg_08 <- subset(LOR_JLLagg_08, !is.na(LOR_JLLagg_08$RAUMID))
sum(is.na(LOR_JLLagg_08$Miete_H1_wmean.2008)) # für soviele LORs fehlen uns die Mietpreisdaten
sum(is.na(LOR_JLLagg_08$Miete_H2_wmean.2008)) # für soviele LORs fehlen uns die Mietpreisdaten

#+++++++++++++
# 2009
#+++++++++++++

bloecke2LOR_09 <- data.frame(bloeckePLZ09_ptdf,over(bloeckePLZ09_ptdf, LORshape))
bloecke2LOR_09 <- subset(bloecke2LOR_09, select=-c(FLAECH_HA, FLAECHE_HA, x, y))
names(bloecke2LOR_09)
#str(bloecke2LOR_09)

LOR_JLLagg_09 <- ddply(bloecke2LOR_09, 
                       "RAUMID", summarise, 
                       Miete_H1_wmean.2009 = round(weighted.mean(Miete_H1, EW_GESAMT), digits=2),
                       Miete_H2_wmean.2009 = round(weighted.mean(Miete_H2, EW_GESAMT), digits=2))
head(LOR_JLLagg_09)
LOR_JLLagg_09 <- subset(LOR_JLLagg_09, !is.na(LOR_JLLagg_09$RAUMID))
sum(is.na(LOR_JLLagg_09$Miete_H1_wmean.2009)) # für soviele LORs fehlen uns die Mietpreisdaten
sum(is.na(LOR_JLLagg_09$Miete_H2_wmean.2009)) # für soviele LORs fehlen uns die Mietpreisdaten

#+++++++++++++
# 2010-2012
#+++++++++++++

bloecke2LOR_10_12 <- data.frame(bloeckePLZ10_12_ptdf,over(bloeckePLZ10_12_ptdf, LORshape))
bloecke2LOR_10_12 <- subset(bloecke2LOR_10_12, select=-c(FLAECHE_HA, x, y))
names(bloecke2LOR_10_12)
str(bloecke2LOR_10_12)

LOR_JLLagg_10_12 <- ddply(bloecke2LOR_10_12, 
                       "RAUMID", summarise, 
                       # 2010 #
                       Miete_H1_wmean.2010 = round(weighted.mean(Miete_H1.2010, EW2010), digits=2),
                       Miete_H2_wmean.2010 = round(weighted.mean(Miete_H2.2010, EW2010), digits=2),
                       # 2011 #
                       Miete_H1_wmean.2011 = round(weighted.mean(Miete_H1.2011, EW2011), digits=2),
                       Miete_H2_wmean.2011 = round(weighted.mean(Miete_H2.2011, EW2011), digits=2),
                       # 2012 #
                       Miete_H1_wmean.2012 = round(weighted.mean(Miete_H1.2012, EW2012), digits=2),
                       Miete_H2_wmean.2012 = round(weighted.mean(Miete_H2.2012, EW2012), digits=2))

sum(is.na(LOR_JLLagg_10_12$Miete_H1_wmean.2010)) # für soviele LORs fehlen uns die Mietpreisdaten 2010
sum(is.na(LOR_JLLagg_10_12$Miete_H2_wmean.2010)) # für soviele LORs fehlen uns die Mietpreisdaten 2010
sum(is.na(LOR_JLLagg_10_12$Miete_H1_wmean.2011)) # für soviele LORs fehlen uns die Mietpreisdaten 2011
sum(is.na(LOR_JLLagg_10_12$Miete_H2_wmean.2011)) # für soviele LORs fehlen uns die Mietpreisdaten 2011
sum(is.na(LOR_JLLagg_10_12$Miete_H1_wmean.2012)) # für soviele LORs fehlen uns die Mietpreisdaten 2012
sum(is.na(LOR_JLLagg_10_12$Miete_H2_wmean.2012)) # für soviele LORs fehlen uns die Mietpreisdaten 2012

LOR_JLLagg_10_12 <- subset(LOR_JLLagg_10_12, !is.na(LOR_JLLagg_10_12$RAUMID))
names(LOR_JLLagg_10_12)
head(LOR_JLLagg_08)
head(LOR_JLLagg_10_12)

identical(LOR_JLLagg_07$RAUMID, LOR_JLLagg_09$RAUMID)    # passt
identical(LOR_JLLagg_08$RAUMID, LOR_JLLagg_09$RAUMID)    # passt
identical(LOR_JLLagg_08$RAUMID, LOR_JLLagg_10_12$RAUMID) # passt. Ein einfacher "cbind" reicht um die Datensätze miteinander zu mergen
LOR_JLLagg <- data.frame(cbind(LOR_JLLagg_07,
                               LOR_JLLagg_08,
                               LOR_JLLagg_09,
                               LOR_JLLagg_10_12))
LOR_JLLagg <- subset(LOR_JLLagg, select=-c(RAUMID.1,RAUMID.2,RAUMID.3))

str(LOR_JLLagg)
#LOR_JLLagg <- join_all(list(LOR_JLLagg_07,
#                            LOR_JLLagg_08,
#                            LOR_JLLagg_09,
#                            LOR_JLLagg_10_12), 
#                       by = "RAUMID")

str(LOR_JLLagg)

LOR_JLLaggWIDE <- LOR_JLLagg
#View(LOR_JLLaggWIDE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Aggregierte Daten mit LOR Shapefile assoziieren & den LOR FULL long Datensatz erstellen ------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Mit LOR Shapefile assoziieren

source("/home/dao/Desktop/MasterArbeit/R_files/functions/merge_with_order_FUNCTION.R")
colnames(LORslim@data)[1]  <- "RAUMID"
LORslim_df             <- as(LORslim, "data.frame")
LORattrFULLwide <- merge(LORattr, LOR_JLLaggWIDE, by="RAUMID", 
                         all.x=T, all.y=T,
                         sort=F)
LORattrFULLwide4shape        <- merge.with.order(LORslim_df, LORattrFULLwide, 
                                           by.x="RAUMID", by.y="RAUMID", 
                                           all.x=T, all.y=T,
                                           sort=F, keep_order=1)
LOR@data <- LORattrFULLwide4shape
names(LOR)

# Test ob Zuordnung zu LORs korrekt war
spplot(LOR, zcol="Miete_H2_wmean.2012",
       col.regions = rev(heat.colors(200)),
       at=seq(4,15, length.out=201),
       xlab="Miete 2.HJahr 2012")

### Long Mietpreisdatensatz auf LOR Niveau
LOR_JLL    <- reshape(LOR_JLLaggWIDE,
                      idvar   = "RAUMID",
                      varying = names(LOR_JLLaggWIDE)[2:13],
                      timevar = "ZEIT",
                      sep = ".",
                      direction = "long")
#View(LOR_JLL)

### Long VOLLSTÄNDIGER LOR Datensatz
LORdataFULLv1     <- merge(LORdata, LOR_JLL, sort=F, 
                           by.x=c("RAUMID","ZEIT"), 
                           by.y=c("RAUMID","ZEIT"), 
                           all.x=T, all.y=T)


#********************************************
#                                           #
#---------- 2.) Sanierungsgebiete-LORs ------
#                                           #
#********************************************

plot(LOR)
plot(SanGebiete, add=T, col="red")

zielCRS <- CRS("+proj=cass +lat_0=52.41864827777778 +lon_0=13.62720366666667 
                +x_0=40000 +y_0=10000 +datum=potsdam +units=m
                +no_defs +ellps=bessel +towgs84=598.1,73.7,418.2,0.202,0.045,-2.455,6.7 ")

SanGebiete_pt    <- gCentroid(SanGebiete,byid=TRUE); plot(SanGebiete_pt)
SanGebiete_ptdf  <- SpatialPointsDataFrame(coords = SanGebiete_pt, 
                                          data = SanGebiete@data, 
                                          proj4string = zielCRS)

LORSanGebieteDF <- data.frame(LOR@data, over(LOR, SanGebiete_ptdf))
levels(LORSanGebieteDF$SanGebiet_KLASSE) <- c(levels(LORSanGebieteDF$SanGebiet_KLASSE), 
                                              "nein")
LORSanGebieteDF$SanGebiet_KLASSE[is.na(LORSanGebieteDF$SanGebiet_KLASSE)] <- "nein"
head(LORSanGebieteDF)

LOR@data <- LORSanGebieteDF

spplot(LOR, zcol="SanGebiet_KLASSE", 
       col.regions=c("green","red","yellow","grey"))
spplot(SanGebiete, zcol="SanGebiet_KLASSE", 
       col.regions=c("green","red","yellow"))

spplot(LOR, zcol="SanGebiet_KLASSE",
       col.regions=c("green","red","yellow","grey")) + layer(sp.polygons(SanGebiete, fill="black"))

SanGebietsData <- subset(LOR@data, select=c(SanGebiet, 
                                            SanGebiet_NAME, 
                                            SanGebiet_KLASSE)) 

# LOR LONG FULL Datensatz ergänzen
# --> SenGebietsData Datensatz 6 mal replizieren damit er an long rangemerged werden kann
LORdataFULLv1 <- cbind(LORdataFULLv1, SanGebietsData[rep(seq_len(nrow(SanGebietsData)),6), ])
tail(LORdataFULLv1)

