#******************************************
#
# "Räumliche Konvertierung"
#
# by Guido Schulz
#******************************************
Sys.setlocale("LC_MESSAGES", 'en_GB.UTF-8')
Sys.setenv(LANG = "en_US.UTF-8")

# Packages ----------------------------------------------------------------

install.packages(c("spdep",      "sp",      "maptools", "lattice", 
                   "rgdal",      "rgeos",   "foreign",  "PBSmapping",
                   "reshape",    "plyr",    "reshape2"))
library("spdep")
library("sp")
library("maptools")
library("lattice")
library("rgdal")
library("rgeos")
library("foreign")
library("PBSmapping")
library("reshape")
library("plyr")
library("reshape2")

#                   "car",        "ggplot2", "spatstat", "RColorBrewer",
#                   "colorspace", "ggplot2", "hexbin",   "vioplot",
#                   "vcd",        "ncf",     "spgwr",    "leaps",
#                   "RANN",       "lmtest"))
#install.packages("gdal")
#library("gdal")


# Daten -------------------------------------------------------------------

#~~~~~~~~~~~~~~~~~~~~~~~~
# Einwohner
#~~~~~~~~~~~~~~~~~~~~~~~~

setwd(dir = "/home/dao/Desktop/MasterArbeit/R_data")
EW_files <- dir(path="EW_-LOR-/", pattern = glob2rx("*.csv"))
setwd(dir = "/home/dao/Desktop/MasterArbeit/R_data/EW_-LOR-/")
EW <- lapply(EW_files, FUN = read.table, header = TRUE, sep=";",fill=TRUE,
             dec=",", stringsAsFactors =F, colClasses=c(RAUMID="factor"))
EW <- do.call("rbind", EW) # aus Liste von data.frames einen long Datensatz machen
EW$ZEIT[EW$ZEIT == 200712] <- 2008
EW$ZEIT[EW$ZEIT == 200812] <- 2009
EW$ZEIT[EW$ZEIT == 200912] <- 2010
EW$ZEIT[EW$ZEIT == 201012] <- 2011
EW$ZEIT[EW$ZEIT == 201112] <- 2012
EW$ZEIT[EW$ZEIT == 201212] <- 2013
EW$ZEIT[EW$ZEIT == 201312] <- 2014
#str(EW)

#~~~~~~~~~~~~~~~~~~~~~~~~
# Wohndauer
#~~~~~~~~~~~~~~~~~~~~~~~~

setwd(dir = "/home/dao/Desktop/MasterArbeit/R_data")
WHNDAUER_files <- dir(path="EW_Wohndauer_-LOR-/", pattern = glob2rx("*.csv"))
setwd(dir = "/home/dao/Desktop/MasterArbeit/R_data/EW_Wohndauer_-LOR-/")
WHNDAUER <- lapply(WHNDAUER_files, FUN = read.table, header = TRUE, sep=";",fill=TRUE,
                   dec=",", stringsAsFactors =F, colClasses=c(RAUMID="factor"))
WHNDAUER <- do.call("rbind", WHNDAUER) # aus Liste von data.frames einen long Datensatz machen
WHNDAUER$ZEIT[WHNDAUER$ZEIT == 2013] <- 2014
WHNDAUER$ZEIT[WHNDAUER$ZEIT == 2012] <- 2013
WHNDAUER$ZEIT[WHNDAUER$ZEIT == 2011] <- 2012
WHNDAUER$ZEIT[WHNDAUER$ZEIT == 2010] <- 2011
WHNDAUER$ZEIT[WHNDAUER$ZEIT == 2009] <- 2010
WHNDAUER$ZEIT[WHNDAUER$ZEIT == 2008] <- 2009
WHNDAUER$ZEIT[WHNDAUER$ZEIT == 2007] <- 2008

WHNDAUER <- WHNDAUER[WHNDAUER$RAUMID!="",] # 2 leere Zeilen löschen
WHNDAUER$RAUMID <- factor(WHNDAUER$RAUMID) # leeres Level "" für RAUMID droppen
str(WHNDAUER)

#~~~~~~~~~~~~~~~~~~~~~~~~
# Wohnlage
#~~~~~~~~~~~~~~~~~~~~~~~~

setwd(dir = "/home/dao/Desktop/MasterArbeit/R_data")
WHNLAGE_files <- dir(path="EW_Wohnlage_-LOR-/", pattern = glob2rx("*.csv"))
setwd(dir = "/home/dao/Desktop/MasterArbeit/R_data/EW_Wohnlage_-LOR-/")
WHNLAGE <- lapply(WHNLAGE_files, FUN = read.table, header = TRUE, sep=";",fill=TRUE,
                  dec=",", stringsAsFactors =F, colClasses=c(RAUMID="factor"))
names(WHNLAGE[[4]]) <- names(WHNLAGE[[5]])
WHNLAGE <- lapply(WHNLAGE, FUN = function(x) {names(x) <- toupper(names(x))
                                              x}) # Variablennamen vereinheitlichen (alle upper case)
WHNLAGE <- do.call("rbind", WHNLAGE) # aus Liste von data.frames einen long Datensatz machen
WHNLAGE$WLEINFOL <- as.numeric(WHNLAGE$WLEINFOL)
WHNLAGE$WLEINFML <- as.numeric(WHNLAGE$WLEINFML)
WHNLAGE$WLMITOL  <- as.numeric(WHNLAGE$WLMITOL)
WHNLAGE$WLMITML  <- as.numeric(WHNLAGE$WLMITML)
WHNLAGE$WLGUTOL  <- as.numeric(WHNLAGE$WLGUTOL)
WHNLAGE$WLGUTML  <- as.numeric(WHNLAGE$WLGUTML)
WHNLAGE$WLNZORD  <- as.numeric(WHNLAGE$WLNZORD)
WHNLAGE$ZEIT[WHNLAGE$ZEIT == 200712] <- 2008
WHNLAGE$ZEIT[WHNLAGE$ZEIT == 200812] <- 2009
WHNLAGE$ZEIT[WHNLAGE$ZEIT == 200912] <- 2010
WHNLAGE$ZEIT[WHNLAGE$ZEIT == 201012] <- 2011
WHNLAGE$ZEIT[WHNLAGE$ZEIT == 201112] <- 2012
WHNLAGE$ZEIT[WHNLAGE$ZEIT == 201212] <- 2013
WHNLAGE$ZEIT[WHNLAGE$ZEIT == 201312] <- 2014
#View(WHNLAGE)
#str(WHNLAGE)

#~~~~~~~~~~~~~~~~~~~~~~~~
# Ausländer und Alter
#~~~~~~~~~~~~~~~~~~~~~~~~

setwd(dir = "/home/dao/Desktop/MasterArbeit/R_data")
ALTERAUSLAENDER_files <- dir(path="EW_Alter_Auslaender_-LOR-/", pattern = glob2rx("*.csv"))
setwd(dir = "/home/dao/Desktop/MasterArbeit/R_data/EW_Alter_Auslaender_-LOR-/")
ALTERAUSLAENDER <- lapply(ALTERAUSLAENDER_files, FUN = read.table, header = TRUE, sep=";",fill=TRUE,
                          dec=",", stringsAsFactors =F, colClasses=c(RAUMID="factor"))
ALTERAUSLAENDER <- do.call("rbind", ALTERAUSLAENDER) # aus Liste von data.frames einen long Datensatz machen
ALTERAUSLAENDER$ZEIT[ALTERAUSLAENDER$ZEIT == 200712] <- 2008
ALTERAUSLAENDER$ZEIT[ALTERAUSLAENDER$ZEIT == 200812] <- 2009
ALTERAUSLAENDER$ZEIT[ALTERAUSLAENDER$ZEIT == 200912] <- 2010
ALTERAUSLAENDER$ZEIT[ALTERAUSLAENDER$ZEIT == 201012] <- 2011
ALTERAUSLAENDER$ZEIT[ALTERAUSLAENDER$ZEIT == 201112] <- 2012
ALTERAUSLAENDER$ZEIT[ALTERAUSLAENDER$ZEIT == 201212] <- 2013
ALTERAUSLAENDER$ZEIT[ALTERAUSLAENDER$ZEIT == 201312] <- 2014
#View(ALTERAUSLAENDER)
#str(ALTERAUSLAENDER)

#~~~~~~~~~~~~~~~~~~~~~~~~
# Migrationshintergrund
#~~~~~~~~~~~~~~~~~~~~~~~~

setwd(dir = "/home/dao/Desktop/MasterArbeit/R_data")
MIGHINTER_files <- dir(path="EW_Migrationshintergrund_-LOR-/", pattern = glob2rx("*.csv"))
setwd(dir = "/home/dao/Desktop/MasterArbeit/R_data/EW_Migrationshintergrund_-LOR-/")
MIGHINTER <- lapply(MIGHINTER_files, FUN = read.table, header = TRUE, sep=";",fill=TRUE,
                    dec=",", stringsAsFactors =F, colClasses=c(RAUMID="factor"))
MIGHINTER[[7]][,10] <- NA # für 2014 gibt es nur EU28 und nicht EU27
colnames(MIGHINTER[[7]])[10] <- "HK_EU27"
MIGHINTER <- do.call("rbind", MIGHINTER) # aus Liste von data.frames einen long Datensatz machen
MIGHINTER$ZEIT[MIGHINTER$ZEIT == 200712] <- 2008
MIGHINTER$ZEIT[MIGHINTER$ZEIT == 200812] <- 2009
MIGHINTER$ZEIT[MIGHINTER$ZEIT == 200912] <- 2010
MIGHINTER$ZEIT[MIGHINTER$ZEIT == 201012] <- 2011
MIGHINTER$ZEIT[MIGHINTER$ZEIT == 201112] <- 2012
MIGHINTER$ZEIT[MIGHINTER$ZEIT == 201212] <- 2013
MIGHINTER$ZEIT[MIGHINTER$ZEIT == 201312] <- 2014
#View(MIGHINTER)
#str(MIGHINTER)

#~~~~~~~~~~~~~~~~~~~~~~~~
# Monitoring SozStadtentwicklung
#~~~~~~~~~~~~~~~~~~~~~~~~

setwd(dir = "/home/dao/Desktop/MasterArbeit/R_data")
MONITORING_files <- dir(path="MonitoringSozStadtEnt_-LOR-/", pattern = glob2rx("*.csv"))
setwd(dir = "/home/dao/Desktop/MasterArbeit/R_data/MonitoringSozStadtEnt_-LOR-/")
MONITORING <- lapply(MONITORING_files, FUN = read.table, header = TRUE, sep=",",fill=TRUE,
                     dec=",", stringsAsFactors =F, colClasses=c(RAUMID="factor"))
MONITORING <- do.call("rbind", MONITORING) # aus Liste von data.frames einen long Datensatz machen
# hier stimmen die Jahreszahlen bereits
# Es gab aber eine Änderung der LOR-Einteilung im Bezirk Reinickendorf (Schlüsseltabelle Stand: März 2009)
# Die Änderungen werden nun gemäss dem Mitteilungs PDF des Amtes für Statistik umgesetzt
MONITORING$RAUMID[MONITORING$RAUMID == "12103015"] <- "12103115"
MONITORING$RAUMID[MONITORING$RAUMID == "12103016"] <- "12103116"
MONITORING$RAUMID[MONITORING$RAUMID == "12103017"] <- "12103117"
MONITORING$RAUMID[MONITORING$RAUMID == "12103018"] <- "12103218"
MONITORING$RAUMID[MONITORING$RAUMID == "12103019"] <- "12103219"
MONITORING$RAUMID[MONITORING$RAUMID == "12103020"] <- "12103220"
MONITORING$RAUMID[MONITORING$RAUMID == "12214121"] <- "12214421"
MONITORING$RAUMID[MONITORING$RAUMID == "12214122"] <- "12214422"
MONITORING$RAUMID[MONITORING$RAUMID == "12214123"] <- "12214423"
MONITORING$RAUMID[MONITORING$RAUMID == "12214124"] <- "12214424"
MONITORING$RAUMID[MONITORING$RAUMID == "12214127"] <- "12214527"
MONITORING$RAUMID[MONITORING$RAUMID == "12214128"] <- "12214528"
MONITORING$RAUMID[MONITORING$RAUMID == "12302007"] <- "12302107"
MONITORING$RAUMID[MONITORING$RAUMID == "12302008"] <- "12302108"
MONITORING$RAUMID[MONITORING$RAUMID == "12302009"] <- "12302109"
MONITORING$RAUMID[MONITORING$RAUMID == "12302010"] <- "12302110"
MONITORING$RAUMID[MONITORING$RAUMID == "12302011"] <- "12302211"
MONITORING$RAUMID[MONITORING$RAUMID == "12302012"] <- "12302212"
MONITORING$RAUMID <- factor(MONITORING$RAUMID)
MONITORING$GEBIET <- factor(MONITORING$GEBIET)
#View(MONITORING)
#str(MONITORING)



#~~~~~~~~~~~~~~~~~~~~~~~~
# Kontext Indikatoren
#~~~~~~~~~~~~~~~~~~~~~~~~

setwd(dir = "/home/dao/Desktop/MasterArbeit/R_data")
KONTEXTIND_files <- dir(path="KontextIndikatoren_-LOR-/", pattern = glob2rx("*.csv"))
setwd(dir = "/home/dao/Desktop/MasterArbeit/R_data/KontextIndikatoren_-LOR-/")
KONTEXTIND <- lapply(KONTEXTIND_files, FUN = read.table, header = TRUE, sep=",",fill=TRUE,
                     dec=",", stringsAsFactors =F, colClasses=c(RAUMID="factor"))
KONTEXTIND <- do.call("rbind", KONTEXTIND) # aus Liste von data.frames einen long Datensatz machen
str(KONTEXTIND)
#View(KONTEXTIND)
#names(KONTEXTIND)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ImmoScout Mieten, Whg Kauf Preise , Haus Kauf Preise
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

setwd(dir = "/home/dao/Desktop/MasterArbeit/R_data/ImmoScout_-ISOT-/")
ISmieten        <- read.table(file = "ImmoScout_Mieten.csv", header = TRUE, sep=",", fill=TRUE, 
                              dec=",", na.strings="NA", stringsAsFactors=F)
colnames(ISmieten)  <- c("Bezirk","BGID","2007","2008","2009","2010","2011","2012","2013")
ISmieten <-  melt(data = ISmieten, id.vars=c("Bezirk","BGID"), 
                  variable.name = "Zeit", 
                  value.name= "ISmiete",
                  na.rm= FALSE)
#View(ISmieten)

ISwhgpreise     <- read.table(file = "ImmoScout_WhgKauf.csv", header = TRUE, sep=",", fill=TRUE,
                              dec=",", na.strings="NA", stringsAsFactors=F)
colnames(ISwhgpreise)  <- c("Bezirk","BGID","2007","2008","2009","2010","2011","2012","2013")
ISwhgpreise <-  melt(data = ISwhgpreise, id.vars=c("Bezirk","BGID"), 
                  variable.name = "Zeit", 
                  value.name= "ISwhgpreis",
                  na.rm= FALSE)
#View(ISwhgpreise)

IShauspreise    <- read.table(file = "ImmoScout_HausKauf.csv", header = TRUE, sep=",", fill=TRUE,
                              dec=",", na.strings="NA", stringsAsFactors=F)
colnames(IShauspreise)  <- c("Bezirk","BGID","2007","2008","2009","2010","2011","2012","2013")
IShauspreise <-  melt(data = IShauspreise, id.vars=c("Bezirk","BGID"), 
                     variable.name = "Zeit", 
                     value.name= "IShauspreis",
                     na.rm= FALSE)
#View(IShauspreise)

# Merge all 3 datasets
ISmieten_whgpreise <- merge(x=ISmieten, y=ISwhgpreise, all.x=T, all.y=T)
ISdata             <- merge(x=ISmieten_whgpreise, y=IShauspreise, all.x=T, all.y=T)
remove(ISmieten, ISwhgpreise, IShauspreise, ISmieten_whgpreise)
#View(ISdata)

#~~~~~~~~~~~~~~~~~~~~~~~~
# GSW Daten
#~~~~~~~~~~~~~~~~~~~~~~~~

setwd(dir = "/home/dao/Desktop/MasterArbeit/R_data")
GSWdata <- read.table("GSW_-PLZ-/GSW_Daten_long.csv", header = TRUE, sep=",", fill=TRUE, dec=",",
                      na.strings="NA", stringsAsFactors=F)
#View(GSWdata)

#~~~~~~~~~~~~~~~~~~~~~~~~
# Binnenwanderung LOR 
#~~~~~~~~~~~~~~~~~~~~~~~~

setwd(dir = "/home/dao/Desktop/MasterArbeit/R_data")
BINNENWAND_files <- dir(path="Binnenwanderungen_-LOR-/", pattern = glob2rx("*.csv"))
setwd(dir = "/home/dao/Desktop/MasterArbeit/R_data/Binnenwanderungen_-LOR-/")
BINNENWAND <- lapply(BINNENWAND_files, FUN = read.table, header = TRUE, sep=",",fill=TRUE,
                     dec=",", stringsAsFactors =F, colClasses=c("factor","factor","integer"))
colnames(BINNENWAND[[6]])[1] <- "VonLOR"
colnames(BINNENWAND[[6]])[2] <- "NachLOR"
colnames(BINNENWAND[[7]])[1] <- "VonLOR"
colnames(BINNENWAND[[7]])[2] <- "NachLOR"
  
head(BINNENWAND[[2]])
str(BINNENWAND)
names(BINNENWAND[[1]])
BINNENWANDnew <- lapply(BINNENWAND, function(x) {cast(data = x, VonLOR ~ NachLOR)
                                              x})
BINNENWANDnew <- lapply(BINNENWAND, function(x) {dcast(data = x, VonLOR ~ NachLOR)
                                              x})


BINNENWAND[[1]] <- cast(data = BINNENWAND[[1]], formula = VonLOR ~ NachLOR)
BINNENWAND[[1]][is.na(BINNENWAND[[1]])] <- 0
diag(BINNENWAND[[1]]) <- NA
head(BINNENWAND[[1]])
names(BINNENWAND[[1]])

init matrix (12000,12000)
1 12000
 1 12000

matrix[i,j] <- df$umzug[df$von=i, df$zu=j]

# Verschneidungen -------------------------------------------

#~~~~~~~~~~~~~~~~~~~~~~~~
# GSW Data & PLZ mergen
#~~~~~~~~~~~~~~~~~~~~~~~~

GSWdata <- subset(GSWdata, select=-Bezirk)
GSW2008 <- GSWdata[GSWdata$Zeit==2008,]
GSW2008 <- unique(GSW2008); length(GSW2008$PLZ)
GSW2009 <- GSWdata[GSWdata$Zeit==2009,]
GSW2009 <- unique(GSW2009); length(GSW2009$PLZ)
GSW2010 <- GSWdata[GSWdata$Zeit==2010,]
GSW2010 <- unique(GSW2010); length(GSW2010$PLZ)
GSW2011 <- GSWdata[GSWdata$Zeit==2011,]
GSW2011 <- unique(GSW2011); length(GSW2011$PLZ)
GSW2012 <- GSWdata[GSWdata$Zeit==2012,]
GSW2012 <- unique(GSW2012); length(GSW2012$PLZ)
GSW2013 <- GSWdata[GSWdata$Zeit==2013,]
GSW2013 <- unique(GSW2013); length(GSW2013$PLZ)

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

cuts = seq(from=4, to=11, by=0.1)
spplot(PLZ2013, zcol="GSWmiete_kaltMEDIAN", col.regions = heat.colors(200))

PLZ2013@data$GSWmiete_kaltMEDIAN



names(PLZ2013@data)

# Shape files --------------------------------------------------------------

#~~~~~~~~~~~~~~~~~~~~~~~~
# ImmoScout Ortsteile
#~~~~~~~~~~~~~~~~~~~~~~~~

setwd("/home/dao/Desktop/MasterArbeit/GentriMap/4 Geodaten")
IS       <- readOGR(dsn="ImmoScout", layer="Berlin_BGID_projected")
zielCRS  <- IS@proj4string
# EPSG 3068 SOLDNER BERLIN
#zielCRS <- CRS("+proj=cass +lat_0=52.41864827777778 +lon_0=13.62720366666667 +x_0=40000 +y_0=10000 +datum=potsdam +units=m
#+no_defs +ellps=bessel +towgs84=598.1,73.7,418.2,0.202,0.045,-2.455,6.7 ")

#~~~~~~~~~~~~~~~~~~~~~~~~
# Postleitzahlen PLZ
#~~~~~~~~~~~~~~~~~~~~~~~~

setwd("/home/dao/Desktop/MasterArbeit/GentriMap/4 Geodaten")
PLZ1     <- readOGR(dsn="PLZ_GS/RBS_OD_PLZ_01_2014/", layer="RBS_OD_PLZ_1312", encoding = "UTF-8")
proj4string(PLZ1)    <- CRS("+proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs") # EPSG:25833
PLZ    <- spTransform(PLZ1, zielCRS)
#plot(PLZ)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Stat. Landesamt LOR (Prognose-, Planungsräume, Bezirksregionen)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

setwd("/home/dao/Desktop/MasterArbeit/GentriMap/4 Geodaten")
LOR1     <- readOGR(dsn="LOR/LORneu/LOR_SHP_EPSG_3068/", layer="Planungsraum_EPSG_3068")
PGR1     <- readOGR(dsn="LOR/LORneu/LOR_SHP_EPSG_3068/", layer="Prognoseraum_EPSG_3068")
BZR1     <- readOGR(dsn="LOR/LORneu/LOR_SHP_EPSG_3068/", layer="Bezirksregion_EPSG_3068")
#SG       <- readOGR(dsn="Geoinstitut/Geoinstitut/", layer="Digk5_StatGeb")

proj4string(LOR1) <- CRS("+proj=cass +lat_0=52.41864827777778 +lon_0=13.62720366666667 
                         +x_0=40000 +y_0=10000 +ellps=bessel +datum=potsdam +units=m +no_defs ")
proj4string(PGR1) <- CRS("+proj=cass +lat_0=52.41864827777778 +lon_0=13.62720366666667 
                         +x_0=40000 +y_0=10000 +ellps=bessel +datum=potsdam +units=m +no_defs ")
proj4string(BZR1) <- CRS("+proj=cass +lat_0=52.41864827777778 +lon_0=13.62720366666667 
                         +x_0=40000 +y_0=10000 +ellps=bessel +datum=potsdam +units=m +no_defs ")
#SG@proj4string
#proj4string(SG)   <- zielCRS

LOR <- spTransform(LOR1, zielCRS)
PGR <- spTransform(PGR1, zielCRS)
BZR <- spTransform(BZR1, zielCRS)
#plot(LOR)
#plot(BZR)

#~~~~~~~~~~~~~~~~~~~~~~~~
# Sanierungsgebiete
#~~~~~~~~~~~~~~~~~~~~~~~~

setwd("/home/dao/Desktop/MasterArbeit/GentriMap/4 Geodaten")
SanGebiete1                <- readOGR(dsn="Sanierungsgebiete_GS/Sanierungsgebiete_EPSG_25833/",
                                      layer="Sanierungsgebiete_EPSG_25833")
proj4string(SanGebiete1)   <- CRS("+proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs") # EPSG:25833
SanGebiete                 <- spTransform(SanGebiete1, zielCRS)

SanGebiete@data$KLASSENNAM <- revalue(SanGebiete@data$KLASSENNAM, c("Verfahren_aufgehoben"  = "aufgehoben",
                                                                    "Verfahren_Umfassend"   = "umfassend",
                                                                    "Verfahren_Vereinfacht" = "vereinfacht"))
#SanGebiete@data$KLASSENNAM

#plot(PLZ)
#plot(SanGebiete, col="red", add=T)

# Bloecke ----------------------------------------------------------------

#*******************
# Bloecke 2007
#*******************

setwd(dir="/home/dao/Desktop/MasterArbeit/GentriMap/4 Geodaten/Bloecke_GS/")
EW_07_raw <- read.dbf(file = "2007_EPSG3068/Sachdaten/e06_06ewdichte2007.dbf")
head(EW_07_raw)
bloecke07_attributes <- read.dbf(file = "2007_EPSG3068/06_06ewdichte2007_Flaechen.DBF")
bloecke07_attributes$order <- seq(1:length(bloecke07_attributes$SCHLUESSEL))
head(bloecke07_attributes)

EW_07 <- merge(bloecke07_attributes, EW_07_raw, by="SCHLUESSEL", all.x=T, all.y=T)
head(EW_07)
length(EW_07_raw$SCHLUESSEL)
length(bloecke07_attributes$SCHLUESSEL)
length(EW_07$SCHLUESSEL)
subset(EW_07_raw, EW_07_raw$EINWOHNER>0 & is.na(EW_07_raw$EW_PRO_HA)) # diese 38 Fälle sind das Problem


table(EW_07$KLASSENNAM, useNA=c("always"))
subset(EW_07,is.na(EW_07$KLASSENNAM))
subset(EW_07, EW_07$EINWOHNER>0 & (is.na(EW_07$FLAECHE_IN)))
is.na(EW_07$KLASSENNAM)

bloecke07  <- readOGR(dsn="/home/dao/Desktop/MasterArbeit/GentriMap/4 Geodaten/Bloecke_GS/2007_EPSG3068/", layer="06_06ewdichte2007_Flaechen.shp")
bloecke07  <- spTransform(bloecke07, zielCRS)
#sum(as.numeric(bloecke07@data$EW2013), na.rm=T)
#str(bloecke07@data)
#plot(bloecke07)

#*******************
# Bloecke 2008
#*******************

EW_08_raw <- read.dbf(file = "2008_EPSG3068/Sachdaten/06_06EWdichte2008.DBF")
head(EW_08_raw)
bloecke08_attributes <- read.dbf(file = "2008_EPSG3068/alt/06_06ewdichte2008.DBF")
bloecke08_attributes$order <- seq(1:length(bloecke08_attributes$SCHLUESSEL))
head(bloecke08_attributes)

EW_08 <- merge(bloecke08_attributes, EW_08_raw, by="SCHLUESSEL", all.x=T, all.y=T)
head(EW_08)
length(EW_08_raw$SCHLUESSEL)
length(bloecke08_attributes$SCHLUESSEL)
length(EW_08$SCHLUESSEL)
EW_08 <- EW_08[with(EW_08, order(order)), ]
head(EW_08)
#sum(EW_08$EINWOHNER, na.rm=T)
setwd(dir="2008_EPSG3068/")
write.dbf(EW_08, file="06_06ewdichte2008", factor2char = TRUE)

bloecke08  <- readOGR(dsn="/home/dao/Desktop/MasterArbeit/GentriMap/4 Geodaten/Bloecke_GS/2008_EPSG3068/", layer="06_06ewdichte2008")
proj4string(bloecke08) <- CRS("+proj=cass +lat_0=52.41864827777778 +lon_0=13.62720366666667 
                              +x_0=40000 +y_0=10000 +ellps=bessel +datum=potsdam +units=m +no_defs")
bloecke08  <- spTransform(bloecke08, zielCRS)
bloecke08@data$EINWOHNER[is.na(bloecke08@data$EINWOHNER)] <- 0
bloecke08@data$EW_PRO_HA[is.na(bloecke08@data$EW_PRO_HA)] <- 0
bloecke08 <- bloecke08[bloecke08@data$EINWOHNER>0, ] # alle Blöcke löschen, wo niemand wohnt
#length(bloecke08@data$SCHLUESSEL)
#sum(as.numeric(bloecke08@data$EW2013), na.rm=T)
#str(bloecke08@data)
#plot(bloecke08)
#View(bloecke08@data)

#*******************
# Bloecke 2009
#*******************

setwd(dir="/home/dao/Desktop/MasterArbeit/GentriMap/4 Geodaten/Bloecke_GS/")
EW_09_raw <- read.dbf(file = "2009_EPSG3068/Sachdaten/s06_06EWdichte2009.DBF")
head(EW_09_raw)
bloecke09_attributes <- read.dbf(file = "2009_EPSG3068/alt/06_06ewdichte2009.DBF")
bloecke09_attributes$order <- seq(1:length(bloecke09_attributes$SCHLUESSEL))
head(bloecke09_attributes)

EW_09 <- merge(bloecke09_attributes, EW_09_raw, by="SCHLUESSEL", all.x=T, all.y=T)
length(EW_09_raw$SCHLUESSEL)
length(bloecke09_attributes$SCHLUESSEL)
length(EW_09$SCHLUESSEL)
EW_09 <- EW_09[with(EW_09, order(order)), ]
head(EW_09)
sum(EW_09$EW_GESAMT, na.rm=T)
write.dbf(EW_09, file="06_06ewdichte2009", factor2char = TRUE)

bloecke09  <- readOGR(dsn="/home/dao/Desktop/MasterArbeit/GentriMap/4 Geodaten/Bloecke_GS/2009_EPSG3068/", layer="06_06ewdichte2009")
proj4string(bloecke09) <- CRS("+proj=cass +lat_0=52.41864827777778 +lon_0=13.62720366666667 
                              +x_0=40000 +y_0=10000 +ellps=bessel +datum=potsdam +units=m +no_defs")
bloecke09  <- spTransform(bloecke09, zielCRS)
bloecke09@data$EW_GESAMT[is.na(bloecke09@data$EW_GESAMT)] <- 0
bloecke09@data$EW_PRO_HA[is.na(bloecke09@data$EW_PRO_HA)] <- 0
bloecke09@data <- bloecke09@data[,-(3)] # leere LOR Variable löschen
bloecke09 <- bloecke09[bloecke09@data$EW_GESAMT>0, ] # alle Blöcke löschen, wo niemand wohnt
# length(bloecke09@data$SCHLUESSEL)
#sum(as.numeric(bloecke09@data$EW2013), na.rm=T)
#str(bloecke09@data)
#View(bloecke09@data)
#plot(bloecke09)

#*******************
# Bloecke 2010-2013
#*******************

setwd(dir="/home/dao/Desktop/MasterArbeit/GentriMap/4 Geodaten/Bloecke_GS")
EW_10_13 <- read.table(file ="EW_2010-2013.csv", header=T, sep=",")
EW_10_13$spatial_na <- as.factor(data$spatial_na)
EW_10_13$spatial_al <- as.factor(data$spatial_al)
EW_10_13[is.na(EW_10_13)] <- 0
write.dbf(EW_10_13, file="bloecke_EW", factor2char = TRUE)

bloecke10_13  <- readOGR(dsn="/home/dao/Desktop/MasterArbeit/GentriMap/4 Geodaten/Bloecke_GS", layer="bloecke_EW")
bloecke10_13  <- spTransform(bloecke10_13, zielCRS)
bloecke10_13  <- bloecke10_13[bloecke10_13@data$EW2013>0 | 
                                bloecke10_13@data$EW2012>0 |
                                bloecke10_13@data$EW2011>0 |
                                bloecke10_13@data$EW2010>0, ] # alle Blöcke löschen, wo niemand wohnt
#length(bloecke10_13@data$EW2013)
#sum(as.numeric(bloecke10_13@data$EW2012), na.rm=T)
#sum(EW_10_13$EW2012) # die beiden Summen sollten gleich sein für alle Jahre - passt!
#str(bloecke10_13@data)
#plot(bloecke10_13)
#View(bloecke10_13@data)
