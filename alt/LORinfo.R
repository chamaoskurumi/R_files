##################################################
# LORinfo File Grundlagen schreiben
##################################################

### LOR Shape file einlesen
#----------------------------

library("rgdal")
setwd("/home/dao/Desktop/MasterArbeit/GentriMap/4 Geodaten")
LOR1     <- readOGR(dsn="LOR/LORneu/LOR_SHP_EPSG_3068/", layer="Planungsraum_EPSG_3068")
proj4string(LOR1) <- CRS("+proj=cass +lat_0=52.41864827777778 +lon_0=13.62720366666667 
                         +x_0=40000 +y_0=10000 +ellps=bessel +datum=potsdam +units=m +no_defs ")
LOR <- spTransform(LOR1, zielCRS)
#LOR@data$order <- seq(1:length(LOR@data$SCHLUESSEL))


### LORinfo von WFS Fis Broker einlesen
#--------------------------------------

library("foreign")
LORinfo <- read.dbf("/home/dao/Desktop/MasterArbeit/R_data/LOR_Systematik_PLR-BZR-PRR_-LOR-/LORinfo_WFS-FisBroker.dbf")
LORinfo <- subset(LORinfo, select=-c(gml_id,spatial_ty))
LORinfo <- subset(LORinfo, !is.na(spatial_na))
colnames(LORinfo)[1] <- "SCHLUESSEL"
colnames(LORinfo)[6] <- "BEZIRK_NAME"
LORinfo <- subset(LORinfo, select=-c(spatial_al))
#View(LORinfo)
#library(sp)
#LOR@data <- merge(LOR@data, LORinfo, by="SCHLUESSEL"); View(LOR@data)

### EW2013 Daten
#----------------

EW2013info <- subset(EW, ZEIT==2013)
str(EW2013info)
EW2013info <- subset(EW2013info, select=c(RAUMID, BEZ, PGR, BZR, PLR, STADTRAUM, E_E))
as.factor(EW2013info[,1]) -> EW2013info[,1]
EW2013info$E_E <- as.numeric(gsub(",","", EW2013info$E_E))
(as.numeric(EW2013info$E_E)/100) -> EW2013info$E_E
colnames(EW2013info)[1] <- "SCHLUESSEL"
colnames(EW2013info)[2] <- "BEZIRK"
colnames(EW2013info)[7] <- "EW2013"
str(EW2013info)


#identical(EW2013info$SCHLUESSEL, LORinfo$SCHLUESSEL)
LORinfo$SCHLUESSEL <- as.factor(as.numeric(as.character(LORinfo$SCHLUESSEL)))

#LORinfoFULL <- (data.frame(cbind(LORinfo, EW2013info)))
#LORinfoFULL <- subset(LORinfoFULL, select=-c(PLR, SCHLUESSEL.1))

View(data.frame(LORinfo$SCHLUESSEL, EW2013info$SCHLUESSEL))

LORinfoFULL <- cbind(LORinfo, EW2013info)
names(LORinfoFULL)
View(LORinfoFULL)

LORinfoFULL <- subset(LORinfoFULL, select=c(SCHLUESSEL,PLR_NAME,BZR,BZR_NAME, PGR, PRG_NAME, 
                                              BEZIRK, BEZIRK_NAME, STADTRAUM, FL_HA, EW2013))

View(LORinfoFULL)
LORinfoFULL -> LORinfo
LORinfo <- LORinfo[order(as.numeric(as.character(LORinfo$SCHLUESSEL))),]
View(LORinfo)
LORinfo
LOR@data <- cbind(LOR@data, LORinfoFULL)
View(LOR@data)
names(LOR@data)
colnames(LOR@data)[3] <- "SCHLUESSEL.1"
LOR@data <- subset(LOR@data, select=-c(SCHLUESSEL.1))
View(LOR@data)
LOR@data$EWdichte2013 <- (LOR@data$EW2013/LOR@data$FL_HA)*100
write.dbf(dataframe = LOR@data, file = "/home/dao/Desktop/MasterArbeit/GentriMap/4 Geodaten/LOR/LORinfo.dbf")
View(LOR@data)
spplot(LOR, zcol="EWdichte2013")


####### neuer versuch #############

LORdf  <- as(LOR, "data.frame")
LORdf1 <- merge(LORdf, LORinfoFULL , sort=F, by.x="SCHLUESSEL", by.y="SCHLUESSEL", all.x=T, all.y=T) ; View(LORdf1)

LOR@data <- LORdf1
LOR@data$EWdichte2013 <- (LOR@data$EW2013/LOR@data$FL_HA.x)*100
#spplot(LOR, zcol="EWdichte2013")
#names(LOR@data)

