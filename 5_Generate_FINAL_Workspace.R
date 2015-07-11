#****************************************************
#****************************************************
#****************************************************
#
# Generierung des fertigen Workspaces für die Analyse
#
#*****************************************************
#*****************************************************

#install.packages("gdata")
library("gdata")
library("ggplot2")
library("sp")
library("rgdal")

#*****************************************************************************************
#
##### ___ Vorbereitung der S-Bahn und BZK Shapes für Analyse mit ggplot später _____####
#
#*****************************************************************************************

LOR4regLONGLAT <- spTransform(LOR4reg,CRS("+proj=longlat")) #+ellps=WGS84 +datum=WGS84 +no_defs")) 
LOR4reg.fort <- fortify(LOR4regLONGLAT, region="RAUMID_NAME")
LOR4regdf <- LOR4reg@data
colnames(LOR4regdf)[2] <- "id"
LOR4reg.fort <- join(LOR4reg.fort, LOR4regdf, by="id")

BZKLONGLAT <- spTransform(BZK,CRS("+proj=longlat"))
BZK.fort  <- fortify(BZKLONGLAT, region="BezName")
BZKdf     <- BZK@data
colnames(BZKdf)[2] <- "id"
BZK.fort <- join(BZK.fort, BZKdf, by="id")

S_BahnLONGLAT <- spTransform(S_Bahn,CRS("+proj=longlat"))
S_Bahn.fort  <- fortify(S_BahnLONGLAT)
S_Bahndf     <- S_Bahn@data
#colnames(S_Bahndf)[2] <- "id"
S_Bahn.fort <- join(S_Bahn.fort, S_Bahndf)#, by="id")

#*****************************************************************************************
#
##### ___ INTRA LOR Umzüge Datensatz vorbereiten _____####
#
#*****************************************************************************************

INTRAdfpre      <-  subset(ODdf,VonLOR==NachLOR,select=-c(NachLOR,dyad,BinnenWand.2007,
                                                          BinnenWand.2013,BinnenWand.Sum))
names(INTRAdfpre)[1] <- "RAUMID"
bpDF4INTRAdf <-  subset(LOR4reg@data, select=c("RAUMID","RAUMID_NAME","STADTRAUM","Gentri","GentriA",
                                       "E_E.2008","E_E.2009","E_E.2010","E_E.2011","E_E.2012",
                                       "Fortzuege.2008","Fortzuege.2009","Fortzuege.2010",
                                       "Fortzuege.2011","Fortzuege.2012"))

source("/home/dao/Desktop/MasterArbeit/R_files/functions/merge_with_order_FUNCTION.R")
INTRAdf  <- merge.with.order(bpDF4INTRAdf, INTRAdfpre, sort=F,
                             by.x="RAUMID", by.y="RAUMID",
                             all.x=T, all.y=F,
                             keep_order=1)

INTRAdf$IntraR.2008 <- round((INTRAdf$BinnenWand.2008/INTRAdf$E_E.2008)*100,digits=1)
INTRAdf$IntraR.2009 <- round((INTRAdf$BinnenWand.2009/INTRAdf$E_E.2009)*100,digits=1)
INTRAdf$IntraR.2010 <- round((INTRAdf$BinnenWand.2010/INTRAdf$E_E.2010)*100,digits=1)
INTRAdf$IntraR.2011 <- round((INTRAdf$BinnenWand.2011/INTRAdf$E_E.2011)*100,digits=1)
INTRAdf$IntraR.2012 <- round((INTRAdf$BinnenWand.2012/INTRAdf$E_E.2012)*100,digits=1)

INTRAdf$IntraFR.2008 <- round((INTRAdf$BinnenWand.2008/INTRAdf$Fortzuege.2008)*100,digits=1)
INTRAdf$IntraFR.2009 <- round((INTRAdf$BinnenWand.2009/INTRAdf$Fortzuege.2009)*100,digits=1)
INTRAdf$IntraFR.2010 <- round((INTRAdf$BinnenWand.2010/INTRAdf$Fortzuege.2010)*100,digits=1)
INTRAdf$IntraFR.2011 <- round((INTRAdf$BinnenWand.2011/INTRAdf$Fortzuege.2011)*100,digits=1)
INTRAdf$IntraFR.2012 <- round((INTRAdf$BinnenWand.2012/INTRAdf$Fortzuege.2012)*100,digits=1)

INTRAdflong <- reshape(data = INTRAdf, direction="long", 
                       varying=list(c("E_E.2008","E_E.2009","E_E.2010","E_E.2011","E_E.2012"),
                                    c("Fortzuege.2008","Fortzuege.2009","Fortzuege.2010","Fortzuege.2011","Fortzuege.2012"),
                                    c("BinnenWand.2008","BinnenWand.2009","BinnenWand.2010","BinnenWand.2011","BinnenWand.2012"),
                                    c("IntraR.2008","IntraR.2009","IntraR.2010","IntraR.2011","IntraR.2012"),
                                    c("IntraFR.2008","IntraFR.2009","IntraFR.2010","IntraFR.2011","IntraFR.2012")),  
                       idvar=c("RAUMID","RAUMID_NAME","STADTRAUM","Gentri","GentriA"), 
                       timevar="ZEIT",
                       v.names = c("E_E", "Fortzuege","BinnenWand","IntraR","IntraFR"),
                       times = c(2008:2012))

# Speichern des VOLLEN Workspaces
setwd("/home/dao/Desktop/MasterArbeit/R_files/")
save.image(file = "FULL_FINAL_WORKSPACE.Rdata")

#****************************************************************************
#
##### ___ Für Analyse unnütze Objekte aus Environment löschen _____####
#
#****************************************************************************

keep(LOR4reg.fort, LOR4reg, 
     LORdataFULLvalid, 
     BZK, BZK.fort, 
     S_Bahn.fort, S_Bahn, 
     GentriA_Armut.2007_0.05,
     GentriA_Armut.2007_0.10,
     GentriA_Armut.2007_0.25,
     GentriA_Armut.2007_0.75,
     GentriA_Armut.2007_0.90,
     GentriA_Armut.2007_0.95,
     GentriA_Miete.2007_0.05,
     GentriA_Miete.2007_0.10,
     GentriA_Miete.2007_0.25,
     GentriA_Miete.2007_0.75,
     GentriA_Miete.2007_0.90,
     GentriA_Miete.2007_0.95,
     Gentri_Armut.2007_0.05,
     Gentri_Armut.2007_0.10,
     Gentri_Armut.2007_0.25,
     Gentri_Armut.2007_0.75,
     Gentri_Armut.2007_0.90,
     Gentri_Armut.2007_0.95,
     Gentri_Miete.2007_0.05,
     Gentri_Miete.2007_0.10,
     Gentri_Miete.2007_0.25,
     Gentri_Miete.2007_0.75,
     Gentri_Miete.2007_0.90,
     Gentri_Miete.2007_0.95,
     zielCRS, 
     INTRAdflong,
     sure=TRUE)

setwd("/home/dao/Desktop/MasterArbeit/R_files/KNITR/")
save.image(file = "FINAL_WORKSPACE.Rdata")
