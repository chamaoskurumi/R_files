#********************************************
#                                           #
#         LOR - DATENSÄTZE BASTELN          #
#                                           #
#********************************************

#********************************************
# LOR Long Datensatz  ***********************
#********************************************

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ALTERAUSLAENDER =================

ALTERAUSLAENDER4merge <- subset(ALTERAUSLAENDER, select=-c(BEZ,PGR,BZR,PLR,STADTRAUM))
DF1 <- merge(x = EW, y = ALTERAUSLAENDER4merge, by = c("RAUMID", "ZEIT"))
# unnütze/zu detailierte Altersvariablen löschen
DF1 <- subset(DF1, select=-c(E_EM,      E_EW,      E_E00_01,  E_E01_02,  E_E02_03, 
                             E_E03_05,  E_E05_06,  E_E06_07,  E_E07_08,  E_E08_10,  E_E10_12,  
                             E_E12_14,  E_E14_15,  E_E15_18,  E_E18_21,  E_E21_25,  E_E25_27,  E_E27_30, 
                             E_E30_35,  E_E35_40,  E_E40_45,  E_E45_50,  E_E50_55,  E_E55_60,  E_E60_63,  
                             E_E63_65,  E_E65_67,  E_E67_70,  E_E70_75,  E_E75_80,  E_E80_85, 
                             E_E85_90,  E_E90_95,  E_E95_110,
                             E_AM,      E_AW,      E_A00_01,  E_A01_02,  E_A02_03,  E_A03_05,  E_A05_06,  E_A06_07,  E_A07_08,  E_A08_10, 
                             E_A10_12,  E_A12_14,  E_A14_15,  E_A15_18,  E_A18_21,  E_A21_25,  E_A25_27,  E_A27_30,  E_A30_35,  E_A35_40,  
                             E_A40_45,  E_A45_50,  E_A50_55,  E_A55_60,  E_A60_63,  E_A63_65,  E_A65_67,  E_A67_70,  E_A70_75,  E_A75_80,  
                             E_A80_85,  E_A85_90,  E_A90_95,  E_A95_110))
str(DF1)
names(DF1)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# MIGRATIONSHINTERGRUND =======================

MIGHINTER4merge <- subset(MIGHINTER, select=-c(BEZ,PGR,BZR,PLR,STADTRAUM))
DF2 <- merge(x = DF1, y = MIGHINTER4merge, by = c("RAUMID", "ZEIT"))
str(DF2)
names(DF2)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# WOHNDAUER =================

WHNDAUER4merge <- subset(WHNDAUER, select=-c(BEZ,PGR,BZR,PLR,STADTRAUM))
DF3 <- merge(x = DF2, y = WHNDAUER4merge, by = c("RAUMID", "ZEIT"))
str(DF3)
names(DF3)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# WOHNLAGE =================

WHNLAGE4merge <- subset(WHNLAGE, select=-c(BEZ,PGR,BZR,PLR,STADTRAUM))
DF4 <- merge(x = DF3, y = WHNLAGE4merge, by = c("RAUMID", "ZEIT"))
str(DF4)
names(DF4)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# MONITORING =================

MONITORING4merge <- subset(MONITORING, select=-c(EW), ZEIT>=2008)
DF5 <- merge(x = DF4, y = MONITORING4merge, by = c("RAUMID", "ZEIT"))
# Variablenanordnung ändern
DF5 <- DF5[c(2,1,51,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37
             ,38,39,40,41,42,43,44,45,46,47,48,49,50,52,53,54,55,56,57,58,59,60,61,62,63)]
Gebietsnamen <- DF5[!duplicated(DF5$RAUMID, fromLast=F),"GEBIET"]
Gebietsnamen <- factor(rep(Gebietsnamen, each=6)); str(Gebietsnamen)
DF5$GEBIET   <- Gebietsnamen
colnames(DF5)[3] <- "RAUMID_NAME"
str(DF5)
names(DF5)
View(DF5)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# LORinfo von WFS Fis Broker einlesen ======================================

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
             "E_E",        "E_U1",       "E_1U6",      "E_6U15",    
             "E_15U18",    "E_18U25",    "E_25U55",    "E_55U65",   
             "E_65U80",    "E_80U110",   "E_A",        "E_AU1",     
             "E_A1U6",     "E_A6U15",    "E_A15U18",   "E_A18U25",  
             "E_A25U55",   "E_A55U65",   "E_A65U80",   "E_A80U110", 
             "MH_E",       "HK_EU15",    "HK_EU27",    "HK_Polen",  
             "HK_EheJug",  "HK_EheSU",   "HK_Turk",    "HK_Arab",   
             "HK_Sonst",   "HK_NZOrd",   "EINW10",     "EINW5",     
             "DAU10",      "DAU5",       "PDAU10",     "PDAU5",     
             "WLEINFOL",   "WLEINFML",   "WLMITOL",    "WLMITML",   
             "WLGUTOL",    "WLGUTML",    "WLNZORD",    "Alose",     
             "Alose_u25",  "Alose_langzeit",           "nicht_Alose_Hartz",      "Hartz_u15", 
             "MigHinter_u18",            "WanderVol",  "WanderSaldo",            "WanderSaldo_u6",        
             "Veraend_HartzEmpf_D",    "Veraend_HartzEmpf_Ausl",                 "Veraend_Hartz_u15")]
names(DF6)
#View(DF6)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# KONTEXTINDIKATOREN 2013 ======================================

KONTEXTIND4merge <- subset(KONTEXTIND, select=-c(GEBIET, EW))
DF7 <- merge(x = DF6, y = KONTEXTIND4merge, by = c("ZEIT","RAUMID"), all.x=T)
names(DF7)
#View(DF7)

DF7$ZEIT <- as.factor(DF7$ZEIT)
LORdata <- DF7
names(LORdata) # das ist der vollständige LOR long Datensatz

#********************************************
# Merge LOR Shape mit LOR Wide Datensatz  ***
#********************************************

#install.packages("reshape2")
library("reshape2")

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Long to wide LOR Datensatz ===================================

DF7 <- arrange(DF7, RAUMID, ZEIT)
DF7wide <- reshape(DF7,
                  idvar = c("RAUMID",  "RAUMID_NAME", "BZR",
                            "BZR_NAME","PGR",     "PRG_NAME","BEZ",    
                            "BEZ_NAME","STADTRAUM","FL_HA"),
                  v.names = c("E_E" ,                  
                              "E_U1",                   "E_1U6"           ,       "E_6U15",                 "E_15U18"               ,
                              "E_18U25",                "E_25U55"         ,       "E_55U65",                "E_65U80"               ,
                              "E_80U110",               "E_A"             ,       "E_AU1"   ,               "E_A1U6"                ,
                              "E_A6U15",                "E_A15U18"        ,       "E_A18U25" ,              "E_A25U55"              ,
                              "E_A55U65",               "E_A65U80"        ,       "E_A80U110" ,             "MH_E"                  ,
                              "HK_EU15",                "HK_EU27"         ,       "HK_Polen"   ,            "HK_EheJug"             ,
                              "HK_EheSU",               "HK_Turk"         ,       "HK_Arab"     ,           "HK_Sonst"              ,
                              "HK_NZOrd",               "EINW10"          ,       "EINW5"        ,          "DAU10"                 ,
                              "DAU5",                   "PDAU10"          ,       "PDAU5"         ,         "WLEINFOL"              ,
                              "WLEINFML",               "WLMITOL"         ,       "WLMITML"         ,       "WLGUTOL"               ,
                              "WLGUTML",                "WLNZORD"         ,       "Alose"            ,      "Alose_u25"             ,
                              "Alose_langzeit",         "nicht_Alose_Hartz",      "Hartz_u15"      ,        "MigHinter_u18"         ,
                              "WanderVol",              "WanderSaldo"      ,      "WanderSaldo_u6",         "Veraend_HartzEmpf_D"   ,
                              "Veraend_HartzEmpf_Ausl", "Veraend_Hartz_u15",      "StaedtWohnungen",        "EinfWhnlageLaerm"),
                  timevar = "ZEIT",
                  direction = "wide")
#View(DF7wide)
LORdata_wide <- DF7wide

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Merge LOR Shape file mit LOR Wide Datensatz ==================

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
LORattr       <- merge(LORdf, LORdata_wide, sort=F, by.x="RAUMID", by.y="RAUMID", all.x=T, all.y=T) ; View(LORattr)

LOR@data <- LORattr
names(LORattr)

#LOR@data$EWdichte.2013 <- (LOR@data$E_E.2013/LOR@data$FL_HA)*100
#spplot(LOR, zcol="EWdichte.2013")

write.dbf(dataframe = LOR@data, file = "/home/dao/Desktop/MasterArbeit/GentriMap/4 Geodaten/LOR/LORinfo.dbf")
