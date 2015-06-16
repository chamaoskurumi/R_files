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
     zielCRS, sure=TRUE)
