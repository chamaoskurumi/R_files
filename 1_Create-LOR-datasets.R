#********************************************
#                                           #
#         LOR - DATENSÄTZE BASTELN          #
#                                           #
#********************************************


# ____ Packages ______ ------------------------------------------
library("plyr")
library("reshape2")


#§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# I.) LONG Datensatz ------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# a.) ALTERAUSLAENDER =================

ALTERAUSLAENDER4merge <- subset(ALTERAUSLAENDER, select=-c(BEZ,PGR,BZR,PLR,STADTRAUM))
DF1 <- merge(x = EW, y = ALTERAUSLAENDER4merge, by = c("RAUMID", "ZEIT"))
# unnütze/zu detailierte Altersvariablen löschen
DF1 <- subset(DF1, select=-c(E_EM,      E_EW,      E_E00_01,  E_E01_02,  E_E02_03, 
                             E_E03_05,  E_E05_06,  E_E06_07,  E_E07_08,  E_E08_10,  E_E10_12,  
                             E_E12_14,  E_E14_15, 
                             E_E35_40,  E_E40_45,  E_E45_50,  E_E50_55,  E_E55_60,  E_E60_63,  
                             E_E63_65,  E_E65_67,  E_E67_70,  E_E70_75,  E_E75_80,  E_E80_85, 
                             E_E85_90,  E_E90_95,  E_E95_110,
                             E_AM,      E_AW,      E_A00_01,  E_A01_02,  E_A02_03,  E_A03_05,  E_A05_06,  E_A06_07,  E_A07_08,  E_A08_10, 
                             E_A10_12,  E_A12_14,  E_A14_15,  E_A15_18,  E_A18_21,  E_A21_25,  E_A25_27,  E_A27_30,  E_A30_35,  E_A35_40,  
                             E_A40_45,  E_A45_50,  E_A50_55,  E_A55_60,  E_A60_63,  E_A63_65,  E_A65_67,  E_A67_70,  E_A70_75,  E_A75_80,  
                             E_A80_85,  E_A85_90,  E_A90_95,  E_A95_110))
str(DF1)
names(DF1)
table(DF1$ZEIT)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# b.) MIGRATIONSHINTERGRUND E =======================

MIGHINTERE4merge <- subset(MIGHINTERE, select=-c(BEZ,PGR,BZR,PLR,STADTRAUM))
DF1b <- merge(x = DF1, y = MIGHINTERE4merge, by = c("RAUMID", "ZEIT"))
# unnütze/zu detailierte Altersvariablen löschen
DF1b <- subset(DF1b, select=-c(MH_E,
                               MH_EM,     MH_EW,     MH_E00_01, MH_E01_02, MH_E02_03, MH_E03_05, MH_E05_06, MH_E06_07, MH_E07_08, MH_E08_10,
                               MH_E10_12, MH_E12_14, MH_E14_15, MH_E15_18, MH_E18_21, MH_E21_25, MH_E25_27, MH_E27_30, MH_E30_35, MH_E35_40,
                               MH_E40_45, MH_E45_50, MH_E50_55, MH_E55_60, MH_E60_63, MH_E63_65, MH_E65_67, MH_E67_70, MH_E70_75, MH_E75_80,
                               MH_E80_85, MH_E85_90, MH_E90_95, MH_E95_110))
str(DF1b)
names(DF1b)
table(DF1b$ZEIT)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# c.) MIGRATIONSHINTERGRUND H =======================

MIGHINTERH4merge <- subset(MIGHINTERH, select=-c(BEZ,PGR,BZR,PLR,STADTRAUM))
DF2 <- merge(x = DF1b, y = MIGHINTERH4merge, by = c("RAUMID", "ZEIT"))
str(DF2)
names(DF2)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# d.) WOHNDAUER =================

WHNDAUER4merge <- subset(WHNDAUER, select=-c(BEZ,PGR,BZR,PLR,STADTRAUM))
DF3 <- merge(x = DF2, y = WHNDAUER4merge, by = c("RAUMID", "ZEIT"))
str(DF3)
names(DF3)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# e.) WOHNLAGE =================

WHNLAGE4merge <- subset(WHNLAGE, select=-c(BEZ,PGR,BZR,PLR,STADTRAUM))
DF4 <- merge(x = DF3, y = WHNLAGE4merge, by = c("RAUMID", "ZEIT"))
str(DF4)
names(DF4)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# f.) MONITORING =================

MONITORING4merge <- subset(MONITORING, select=-c(EW), ZEIT>=2007)
DF5 <- merge(x = DF4, y = MONITORING4merge, by = c("RAUMID", "ZEIT"))
# Variablenanordnung ändern
DF5 <- DF5[c(2,1,66,3:65,67:78)]
Gebietsnamen <- DF5[!duplicated(DF5$RAUMID, fromLast=F),"GEBIET"]
Gebietsnamen <- factor(rep(Gebietsnamen, each=6)); str(Gebietsnamen)
DF5$GEBIET   <- Gebietsnamen
colnames(DF5)[3] <- "RAUMID_NAME"
str(DF5)
names(DF5)
#View(DF5)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# g.) LORinfo von WFS Fis Broker einlesen ======================================

library("foreign")
LORinfo <- read.dbf("/home/dao/Desktop/MasterArbeit/R_data/LOR_Systematik_PLR-BZR-PRR_-LOR-/LORinfo_WFS-FisBroker.dbf")
LORinfo <- subset(LORinfo, select=-c(gml_id,spatial_ty))
LORinfo <- subset(LORinfo, !is.na(spatial_na))
colnames(LORinfo)[1] <- "RAUMID"
colnames(LORinfo)[6] <- "BEZ_NAME"
LORinfo       <- subset(LORinfo, select=-c(spatial_al))
LORinfo4merge <- LORinfo[rep(seq_len(nrow(LORinfo)), each=6),]; str(LORinfo4merge)
LORinfo4merge <- data.frame(DF5$ZEIT, LORinfo4merge)
colnames(LORinfo4merge)[1] <- "ZEIT"
DF6a <- merge(x = DF5, y = LORinfo4merge, by = c("RAUMID", "ZEIT"))
DF6  <- DF6a[c("ZEIT",       
             "RAUMID",     "RAUMID_NAME",
             "BZR",        "BZR_NAME", 
             "PGR",        "PRG_NAME",
             "BEZ",        "BEZ_NAME",       
             "STADTRAUM",  "FL_HA",
             "E_E",     
             "E_E15_18",   "E_E18_21",   "E_E21_25",  "E_E25_27",  "E_E27_30", "E_E30_35",     
             "E_U1",       "E_1U6",      "E_6U15",     
             "E_15U18",    "E_18U25",    "E_25U55",    "E_55U65",   
             "E_65U80",    "E_80U110",   
             "E_A",        
             "E_AU1",      "E_A1U6",     "E_A6U15",    
             "E_A15U18",   "E_A18U25",   "E_A25U55",   "E_A55U65",   
             "E_A65U80",   "E_A80U110", 
             "MH_E",       
             "MH_U1",      "MH_1U6",     "MH_6U15",    
             "MH_15U18",   "MH_18U25",   "MH_25U55",  "MH_55U65",
             "MH_65U80",   "MH_80U110",             
             "HK_EU15",    "HK_EU27",    "HK_Polen",  
             "HK_EheJug",  "HK_EheSU",   "HK_Turk",    "HK_Arab",   
             "HK_Sonst",   "HK_NZOrd",   
             "EINW10",     "EINW5",     
             "DAU10",      "DAU5",       
             "PDAU10",     "PDAU5",     
             "WLEINFOL",   "WLEINFML",   
             "WLMITOL",    "WLMITML",   
             "WLGUTOL",    "WLGUTML",    
             "WLNZORD",    
             "Alose",      "Alose_u25",  "Alose_langzeit", "nicht_Alose_Hartz", "Hartz_u15", 
             "MigHinter_u18",           "WanderVol",       "WanderSaldo",       "WanderSaldo_u6",        
             "Veraend_HartzEmpf_D",    "Veraend_HartzEmpf_Ausl",                "Veraend_Hartz_u15")]

# STADTRAUM in Factor umwandln und labeln
DF6$STADTRAUM         <- as.factor(DF6$STADTRAUM)
levels(DF6$STADTRAUM) <- c("innere Stadt", "äußere Stadt")
str(DF6)
#View(DF6)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# h.) KONTEXTINDIKATOREN 2012 ======================================

KONTEXTIND4merge <- subset(KONTEXTIND, select=-c(GEBIET, EW))
source("/home/dao/Desktop/MasterArbeit/R_files/functions/merge_with_order_FUNCTION.R")
DF7 <- merge(x = DF6, 
             y = KONTEXTIND4merge, 
             by = c("ZEIT","RAUMID"), 
             all.x=T, 
             all.y=T,
             sort=T,
             keep_order=1)
#head(DF7)
#View(DF7)
str(DF7) # Order jetzt falsch.
DF7 <- DF7[order(DF7[,"RAUMID"],DF7[,"ZEIT"]), ] # alte Reihenfolge wieder herstellen

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# i.) BINNENWANDERUNGEN ======================================

FORTZUEGEZUZUEGEdf4long <- subset(FORTZUEGEZUZUEGEdf, select=-c(Fortzuege,Zuzuege))
str(FORTZUEGEZUZUEGEdf4long)

FORTZUEGEZUZUEGEdflong <- data.frame(rep(FORTZUEGEdf$RAUMID, times=6),
                                     rep(2007:2012, each=447),
                                     c(FORTZUEGEdf$Fortzuege.2007,
                                       FORTZUEGEdf$Fortzuege.2008,
                                       FORTZUEGEdf$Fortzuege.2009,
                                       FORTZUEGEdf$Fortzuege.2010,
                                       FORTZUEGEdf$Fortzuege.2011,
                                       FORTZUEGEdf$Fortzuege.2012),
                                     c(ZUZUEGEdf$Zuzuege.2007,
                                       ZUZUEGEdf$Zuzuege.2008,
                                       ZUZUEGEdf$Zuzuege.2009,
                                       ZUZUEGEdf$Zuzuege.2010,
                                       ZUZUEGEdf$Zuzuege.2011,
                                       ZUZUEGEdf$Zuzuege.2012))
colnames(FORTZUEGEZUZUEGEdflong) <- c("RAUMID","ZEIT","Fortzuege","Zuzuege")
FORTZUEGEZUZUEGEdflong <- FORTZUEGEZUZUEGEdflong[order(FORTZUEGEZUZUEGEdflong[,"RAUMID"],
                                                       FORTZUEGEZUZUEGEdflong[,"ZEIT"]), ] 
#str(FORTZUEGEZUZUEGEdflong)

source("/home/dao/Desktop/MasterArbeit/R_files/functions/merge_with_order_FUNCTION.R")
DF8 <- merge(x = DF7, 
             y = FORTZUEGEZUZUEGEdflong, 
             by = c("RAUMID","ZEIT"), 
             all.x=T, 
             all.y=T,
             sort=T,
             keep_order=1)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# j.) AUSSENWANDERUNGEN =================================

AUSSENWANDdflong <- data.frame(rep(AUSSENWANDdf$RAUMID, times=6),
                               rep(2007:2012, each=447),
                               c(AUSSENWANDdf$FortzuegeU.2007,
                                 AUSSENWANDdf$FortzuegeU.2008,
                                 AUSSENWANDdf$FortzuegeU.2009,                                       
                                 AUSSENWANDdf$FortzuegeU.2010,
                                 AUSSENWANDdf$FortzuegeU.2011,
                                 AUSSENWANDdf$FortzuegeU.2012),
                               c(AUSSENWANDdf$FortzuegeD.2007,
                                 AUSSENWANDdf$FortzuegeD.2008,
                                 AUSSENWANDdf$FortzuegeD.2009,                                       
                                 AUSSENWANDdf$FortzuegeD.2010,
                                 AUSSENWANDdf$FortzuegeD.2011,
                                 AUSSENWANDdf$FortzuegeD.2012),
                               c(AUSSENWANDdf$FortzuegeA.2007,
                                 AUSSENWANDdf$FortzuegeA.2008,
                                 AUSSENWANDdf$FortzuegeA.2009,                                       
                                 AUSSENWANDdf$FortzuegeA.2010,
                                 AUSSENWANDdf$FortzuegeA.2011,
                                 AUSSENWANDdf$FortzuegeA.2012),
                               c(AUSSENWANDdf$ZuzuegeU.2007,
                                 AUSSENWANDdf$ZuzuegeU.2008,
                                 AUSSENWANDdf$ZuzuegeU.2009,                                       
                                 AUSSENWANDdf$ZuzuegeU.2010,
                                 AUSSENWANDdf$ZuzuegeU.2011,
                                 AUSSENWANDdf$ZuzuegeU.2012),
                               c(AUSSENWANDdf$ZuzuegeD.2007,
                                 AUSSENWANDdf$ZuzuegeD.2008,
                                 AUSSENWANDdf$ZuzuegeD.2009,                                       
                                 AUSSENWANDdf$ZuzuegeD.2010,
                                 AUSSENWANDdf$ZuzuegeD.2011,
                                 AUSSENWANDdf$ZuzuegeD.2012),
                               c(AUSSENWANDdf$ZuzuegeA.2007,
                                 AUSSENWANDdf$ZuzuegeA.2008,
                                 AUSSENWANDdf$ZuzuegeA.2009,                                       
                                 AUSSENWANDdf$ZuzuegeA.2010,
                                 AUSSENWANDdf$ZuzuegeA.2011,
                                 AUSSENWANDdf$ZuzuegeA.2012))

colnames(AUSSENWANDdflong) <- c("RAUMID","ZEIT",
                                "FortzuegeU","FortzuegeD","FortzuegeA",
                                "ZuzuegeU","ZuzuegeD","ZuzuegeA")
AUSSENWANDdflong <- AUSSENWANDdflong[order(AUSSENWANDdflong[,"RAUMID"],
                                           AUSSENWANDdflong[,"ZEIT"]), ] 
str(AUSSENWANDdflong)

source("/home/dao/Desktop/MasterArbeit/R_files/functions/merge_with_order_FUNCTION.R")
DF9 <- merge.with.order(x = DF8, 
                        y = AUSSENWANDdflong, 
                        by = c("RAUMID","ZEIT"), 
                        all.x=T, 
                        all.y=T,
                        sort=T,
                        keep_order=1)
str(DF9)

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# k.) Entfernunung zur STADTMITTE (RAUMID_NAME==Alexanderplatzviertel =================================

LORslim_pt    <- gCentroid(LORslim,byid=TRUE); plot(LORslim_pt)
LORslim_ptdf  <- SpatialPointsDataFrame(coords = LORslim_pt@coords, 
                                         data = LORslim@data, 
                                         proj4string = zielCRS)
colnames(LORslim_ptdf@data)[1] <- "RAUMID"
# Koordinaten des Alexanderplatzviertels (RAUMID==01011303) definieren wir als BERLINS STADTMITTE
AlexCoords <- LORslim_ptdf@coords[LORslim_ptdf@data$RAUMID=="01011303",]
# Entfernungsmessung zwischen BERLINS STADTMITTE und dem Schwepunkt eines jeden LORs
dist2STADTMITTE   <- round(spDistsN1(LORslim_pt@coords,AlexCoords),0)
dist2STADTMITTEdf <- data.frame(LORslim_ptdf@data$RAUMID,dist2STADTMITTE)
colnames(dist2STADTMITTEdf)[1] <- "RAUMID"
dist2STADTMITTEdf <- dist2STADTMITTEdf[order(dist2STADTMITTEdf[,"RAUMID"]), ]

DF10 <- data.frame(DF9, rep(dist2STADTMITTEdf$dist2STADTMITTE,each=6))
colnames(DF10)[length(colnames(DF10))] <- "dist2STADTMITTE"

DF10$ZEIT <- as.factor(DF10$ZEIT)
LORdata <- DF10 # das ist der vollständige LOR long Datensatz
names(LORdata)

#§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# II.) WIDE Datensatz und merge mit LOR Shape File ------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# a.) Long to wide LOR Datensatz ===================================

DF10 <- arrange(DF10, RAUMID, ZEIT)
DF10wide <- reshape(DF10,
                  idvar = c("RAUMID",  "RAUMID_NAME", "BZR",
                            "BZR_NAME","PGR",     "PRG_NAME","BEZ",    
                            "BEZ_NAME","STADTRAUM","FL_HA", "dist2STADTMITTE"),
                  v.names = c("E_E" ,    
                              "E_E15_18",               "E_E18_21",               "E_E21_25",               "E_E25_27",  
                              "E_E27_30",               "E_E30_35",    
                              "E_U1",                   "E_1U6"           ,       "E_6U15",                 "E_15U18"               ,
                              "E_18U25",                "E_25U55"         ,       "E_55U65",                "E_65U80"               ,
                              "E_80U110",               
                              "E_A",       
                              "E_AU1"   ,               "E_A1U6"          ,       "E_A6U15",                "E_A15U18"              ,
                              "E_A18U25" ,              "E_A25U55"        ,       "E_A55U65",               "E_A65U80"              ,
                              "E_A80U110" ,             
                              "MH_E",
                              "MH_U1",                  "MH_1U6"          ,       "MH_6U15"   ,             "MH_15U18"              ,
                              "MH_18U25",               "MH_25U55"        ,       "MH_55U65"  ,             "MH_65U80"              ,
                              "MH_80U110",
                              "HK_EU15",                "HK_EU27"         ,       "HK_Polen"   ,            "HK_EheJug"             ,
                              "HK_EheSU",               "HK_Turk"         ,       "HK_Arab"     ,           "HK_Sonst"              ,
                              "HK_NZOrd",               "EINW10"          ,       "EINW5"        ,          "DAU10"                 ,
                              "DAU5",                   "PDAU10"          ,       "PDAU5"         ,         "WLEINFOL"              ,
                              "WLEINFML",               "WLMITOL"         ,       "WLMITML"         ,       "WLGUTOL"               ,
                              "WLGUTML",                "WLNZORD"         ,       "Alose"            ,      "Alose_u25"             ,
                              "Alose_langzeit",         "nicht_Alose_Hartz",      "Hartz_u15"      ,        "MigHinter_u18"         ,
                              "WanderVol",              "WanderSaldo"      ,      "WanderSaldo_u6",         "Veraend_HartzEmpf_D"   ,
                              "Veraend_HartzEmpf_Ausl", "Veraend_Hartz_u15",      "StaedtWohnungen",        "EinfWhnlageLaerm",
                              "AlleinerzHH",            "Altersarmut",
                              "Fortzuege",              "Zuzuege",
                              "FortzuegeU",             "FortzuegeD",             "FortzuegeA",            
                              "ZuzuegeU",               "ZuzuegeD",               "ZuzuegeA"),
                  timevar = "ZEIT",
                  direction = "wide")
#View(DF10wide)
LORdata_wide <- DF10wide

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# b.) Merge LOR Shape file mit LOR Wide Datensatz ==================

library("rgdal")
setwd("/home/dao/Desktop/MasterArbeit/GentriMap/4 Geodaten")
LOR1     <- readOGR(dsn="LOR/LORneu/LOR_SHP_EPSG_3068/", layer="Planungsraum_EPSG_3068")
proj4string(LOR1) <- CRS("+proj=cass +lat_0=52.41864827777778 +lon_0=13.62720366666667 
                         +x_0=40000 +y_0=10000 +ellps=bessel +datum=potsdam +units=m +no_defs ")
zielCRS <- CRS("+proj=cass +lat_0=52.41864827777778 +lon_0=13.62720366666667 +x_0=40000 +y_0=10000 +datum=potsdam +units=m
                +no_defs +ellps=bessel +towgs84=598.1,73.7,418.2,0.202,0.045,-2.455,6.7 ")
LORshape <- spTransform(LOR1, zielCRS)

colnames(LORshape@data)[1]  <- "RAUMID"
LORdf         <- as(LORshape, "data.frame")
source("/home/dao/Desktop/MasterArbeit/R_files/functions/merge_with_order_FUNCTION.R")
LORattr       <- merge.with.order(
                       LORdf, LORdata_wide, sort=F,
                       by.x="RAUMID", by.y="RAUMID",
                       all.x=T, all.y=T,
                       keep_order=1)
#View(LORattr)

rbind(LORshape@data$RAUMID, LORattr$RAUMID)

LOR@data <- LORattr
names(LORattr)

#LOR@data$EWdichte.2012 <- (LOR@data$E_E.2012/LOR@data$FL_HA)*100
#spplot(LOR, zcol="EWdichte.2012")

library(foreign)
write.dbf(dataframe = LOR@data, file = "/home/dao/Desktop/MasterArbeit/GentriMap/4 Geodaten/LOR/LORinfo.dbf")
