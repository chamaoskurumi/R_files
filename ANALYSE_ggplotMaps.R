
#*************************************************
#*************************************************
#*************************************************
#
# GGPLOT Karten
#
#*************************************************
#*************************************************

setwd("/home/dao/Desktop/MasterArbeit/R_files/KNITR/")
load(file = "FULL_FINAL_WORKSPACE.Rdata")

library("ggplot2")
library("ggmap")
library("rgdal")
library("sp")
library("plyr")
library("scales")

#### 1.) Daten vorbereiten ####
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

ggplot(LOR4reg.fort, aes(x=long, y=lat, group = id)) + geom_polygon(aes(fill=Mietechgr, group=id)) + geom_path(data=S_Bahn.fort, aes(x=long, y=lat))

LOR4reg.fort$Gentri   <- factor(LOR4reg.fort$Gentri,levels(LOR4reg.fort$Gentri)[c(2,3,1)])
LOR4reg.fort$GentriA  <- factor(LOR4reg.fort$GentriA,levels(LOR4reg.fort$GentriA)[c(2,3,1)])

#### 2.) GGPLOT 

##### **** Kartenlayouts ****#####
kartenlayout <- list(geom_polygon(aes(group=id),data=BZK.fort, fill=NA, colour="grey50", alpha=0.8,size=1.2),
                     theme(legend.position =c(0.9,0.8), 
                           line = element_blank(), 
                           rect= element_blank(),
                           axis.line=element_blank(),
                           axis.text.x=element_blank(),
                           axis.text.y=element_blank(),
                           axis.ticks=element_blank(),
                           axis.title.x=element_blank(),
                           axis.title.y=element_blank(),
                           legend.title=element_text(size=rel(1.4), face="bold"),
                           legend.text=element_text(size=rel(1))),
                     guides(fill = guide_colourbar(barheight=7)),
                     geom_path(data=S_Bahn.fort, aes(x=long, y=lat), linetype=2, size=1))

kartenlayoutCAT <- list(geom_polygon(aes(group=id),data=BZK.fort, fill=NA, colour="grey50", alpha=0.8, size=1.2),
                        theme(legend.position =c(0.9,0.8), 
                              line = element_blank(), 
                              rect= element_blank(),
                              axis.line=element_blank(),
                              axis.text.x=element_blank(),
                              axis.text.y=element_blank(),
                              axis.ticks=element_blank(),
                              axis.title.x=element_blank(),
                              axis.title.y=element_blank(),
                              legend.title=element_text(size=rel(1.4), face="bold"),
                              legend.text=element_text(size=rel(1)),
                              legend.background = element_rect()),
                        geom_path(data=S_Bahn.fort, aes(x=long, y=lat), linetype=2, size=1))

kartenlayoutCATo <- list(geom_polygon(aes(group=id),data=BZK.fort, fill=NA, colour="grey50", alpha=0.8, size=0.5),
                         theme(legend.position ='none', 
                               line = element_blank(), 
                               rect= element_blank(),
                               axis.line=element_blank(),
                               axis.text.x=element_blank(),
                               axis.text.y=element_blank(),
                               axis.ticks=element_blank(),
                               axis.title.x=element_blank(),
                               axis.title.y=element_blank(),
                               legend.title=element_text(size=rel(0.8), face="bold"),
                               legend.text=element_text(size=rel(0.6)),
                               legend.background = element_rect()),
                         geom_path(data=S_Bahn.fort, aes(x=long, y=lat), linetype=2, size=0.6))

lowCOLOR <- "blue"
midCOLOR <- "#fee0d2"
hiCOLOR  <- "red"
superhiCOLOR <- "black"


##### **** Maps ****#####
MietechgrMAP <- ggplot(LOR4reg.fort, aes(x=long, y=lat, group = id)) + 
  geom_polygon(aes(fill=Mietechgr, group=id)) + geom_path(color="grey", alpha=0.0, size=0.5) +
  scale_fill_gradient2(low = lowCOLOR, mid = midCOLOR, high =hiCOLOR,midpoint = 0, 
                       name=expression(paste(Delta[r],'Miete (%)'))) +
  coord_map("polyconic",xlim = c(13.08,13.77),ylim = c(52.33,52.69)) + kartenlayout
MietechgrMAP


ArmutchgMAP <- ggplot(LOR4reg.fort, aes(x=long, y=lat, group = id)) + 
  geom_polygon(aes(fill=Armutchg, group=id)) + geom_path(color="grey", alpha=0.0, size=0.5) +
  scale_fill_gradient2(low = lowCOLOR, mid = midCOLOR, high =hiCOLOR,midpoint = 0, 
                       name=expression(paste(Delta,'Armut (%)'))) +
  coord_map("polyconic",xlim = c(13.08,13.77),ylim = c(52.33,52.69)) + kartenlayout
ArmutchgMAP

GentriMAP <- ggplot(LOR4reg.fort, aes(x=long, y=lat, group = id)) + 
  geom_polygon(aes(fill=Gentri, group=id)) + geom_path(color="grey20", alpha=1, size=0.3) +
  scale_fill_discrete(name=expression(paste('Kategorie'))) +  
  coord_map("polyconic",xlim = c(13.08,13.77),ylim = c(52.33,52.69)) + kartenlayoutCATo
GentriMAP

ArmutchgMAP <- ggplot(LOR4reg.fort, aes(x=long, y=lat, group = id)) + 
  geom_polygon(aes(fill=Armutchg, group=id)) + geom_path(color="grey", alpha=0.0, size=0.5) +
  scale_fill_gradient2(low = lowCOLOR, mid = midCOLOR, high =hiCOLOR,midpoint = 0, 
                       name=expression(paste(Delta,'Armut (%)'))) +
  coord_map("polyconic",xlim = c(13.08,13.77),ylim = c(52.33,52.69)) + kartenlayout
ArmutchgMAP

Armut.2007MAP <- ggplot(LOR4reg.fort, aes(x=long, y=lat, group = id)) + 
  geom_polygon(aes(fill=Armut.2007, group=id)) + geom_path(color="grey", alpha=0.0, size=0.5) +
  scale_fill_gradientn(colours = c(lowCOLOR,midCOLOR,hiCOLOR), 
                       values = rescale(c(1,20,55)),
                       guide = "colorbar", limits=c(1,55),
                       name=expression(paste('Armut'[2007],' ','(%)'))) +
  coord_map("polyconic",xlim = c(13.08,13.77),ylim = c(52.33,52.69)) + kartenlayout 
Armut.2007MAP

Armut.2012MAP <- ggplot(LOR4reg.fort, aes(x=long, y=lat, group = id)) + 
  geom_polygon(aes(fill=Armut.2012, group=id)) + geom_path(color="grey", alpha=0.0, size=0.5) +
  scale_fill_gradientn(colours = c(lowCOLOR,midCOLOR,hiCOLOR), 
                       values = rescale(c(1,20,55)),
                       guide = "colorbar", limits=c(1,55),
                       name=expression(paste('Armut'[2012],' ','(%)'))) +
  coord_map("polyconic",xlim = c(13.08,13.77),ylim = c(52.33,52.69)) + kartenlayout 
Armut.2012MAP

Miete.2007MAP <- ggplot(LOR4reg.fort, aes(x=long, y=lat, group = id)) + 
  geom_polygon(aes(fill=Miete.2007, group=id)) + geom_path(color="grey", alpha=0.0, size=0.5) +
  scale_fill_gradientn(colours = c(lowCOLOR,midCOLOR,hiCOLOR), 
                       values = rescale(c(4,7,14)),
                       guide = "colorbar", limits=c(4,14),
                       name=expression(paste('Miete'[2007],' ','(%)'))) +
  coord_map("polyconic",xlim = c(13.08,13.77),ylim = c(52.33,52.69)) + kartenlayout 
Miete.2007MAP

Miete.2012MAP <- ggplot(LOR4reg.fort, aes(x=long, y=lat, group = id)) + 
  geom_polygon(aes(fill=Miete.2012, group=id)) + geom_path(color="grey", alpha=0.0, size=0.5) +
  scale_fill_gradientn(colours = c(lowCOLOR,midCOLOR,hiCOLOR), 
                       values = rescale(c(4,7,14)),
                       guide = "colorbar", limits=c(4,14),
                       name=expression(paste('Miete'[2012],' ','(%)'))) +
  coord_map("polyconic",xlim = c(13.08,13.77),ylim = c(52.33,52.69)) + kartenlayout 
Miete.2012MAP

Miete.2012MAP <- ggplot(LOR4reg.fort, aes(x=long, y=lat, group = id)) + 
  geom_polygon(aes(fill=Miete.2012, group=id)) + geom_path(color="grey", alpha=0.0, size=0.5) +
  scale_fill_gradientn(colours = c(lowCOLOR,midCOLOR,hiCOLOR), 
                       values = rescale(c(4,7,14)),
                       guide = "colorbar", limits=c(4,14),
                       name=expression(paste('Miete'[2012],' ','(%)'))) +
  coord_map("polyconic",xlim = c(13.08,13.77),ylim = c(52.33,52.69)) + kartenlayout 
Miete.2012MAP

FortzuegeRMAP <- ggplot(LOR4reg.fort, aes(x=long, y=lat, group = id)) + 
  geom_polygon(aes(fill=FortzuegeR, group=id)) + geom_path(color="grey", alpha=0.0, size=0.5) +
  scale_fill_gradientn(colours = c(lowCOLOR,midCOLOR,hiCOLOR,superhiCOLOR), 
                       values = rescale(c(3.9,12,15,25)),
                       guide = "colorbar", limits=c(3.4,25),
                       name=expression(paste('Fortzüge',' ','(%)'))) +
  coord_map("polyconic",xlim = c(13.08,13.77),ylim = c(52.33,52.69)) + kartenlayout 
FortzuegeRMAP

ZuzuegeRMAP <- ggplot(LOR4reg.fort, aes(x=long, y=lat, group = id)) + 
  geom_polygon(aes(fill=ZuzuegeR, group=id)) + geom_path(color="grey", alpha=0.0, size=0.5) +
  scale_fill_gradientn(colours = c(lowCOLOR,midCOLOR,hiCOLOR,superhiCOLOR), 
                       values = rescale(c(3.8,7,13,22)),
                       guide = "colorbar", limits=c(3.8,22),
                       name=expression(paste('Zuzüge',' ','(%)'))) +
  coord_map("polyconic",xlim = c(13.08,13.77),ylim = c(52.33,52.69)) + kartenlayout 
ZuzuegeRMAP

FortzuegeUDARMAP <- ggplot(LOR4reg.fort, aes(x=long, y=lat, group = id)) + 
  geom_polygon(aes(fill=FortzuegeUDAR, group=id)) + geom_path(color="grey", alpha=0.0, size=0.5) +
  scale_fill_gradientn(colours = c(lowCOLOR,midCOLOR,hiCOLOR,superhiCOLOR), 
                       values = rescale(c(0,4,8,26.5)),
                       guide = "colorbar", limits=c(0,26.5),
                       name=expression(paste('FortzügeA',' ','(%)'))) +
  coord_map("polyconic",xlim = c(13.08,13.77),ylim = c(52.33,52.69)) + kartenlayout 
FortzuegeUDARMAP


FortzuegeRMAP <- ggplot(LOR4reg.fort, aes(x=long, y=lat, group = id)) + 
  geom_polygon(aes(fill=FortzuegeR, group=id)) + geom_path(color="grey", alpha=0.0, size=0.5) +
  scale_fill_gradientn(colours = rev(c("#ffffcc", "#ffeda0",
                                   "#fed976","#feb24c","#fd8d3c",
                                   "#fc4e2a","#e31a1c","#bd0026","#800026")), 
                       #values = rescale(c(3.9,12,15,25)),
                       guide = "colorbar", limits=c(3.4,25),
                       name=expression(paste('Fortzüge',' ','(%)'))) +
  coord_map("polyconic",xlim = c(13.08,13.77),ylim = c(52.33,52.69)) + kartenlayout 
FortzuegeRMAP


FortzuegeRMAP <- ggplot(LOR4reg.fort, aes(x=long, y=lat, group = id)) + 
  geom_polygon(aes(fill=FortzuegeR, group=id)) + geom_path(color="grey", alpha=0.0, size=0.5) +
  scale_fill_gradientn(colours = rev(c("#d53e4f","#f46d43","#fdae61",
                         "#fee08b","#ffffbf","#e6f598",
                         "#abdda4","#66c2a5","#3288bd")), 
                       #values = rescale(c(3.9,12,15,25)),
                       guide = "colorbar", limits=c(3.4,25),
                       name=expression(paste('Fortzüge',' ','(%)'))) +
  coord_map("polyconic",xlim = c(13.08,13.77),ylim = c(52.33,52.69)) + kartenlayout 
FortzuegeRMAP


ZuzuegeUDARAMAP <- ggplot(LOR4reg.fort, aes(x=long, y=lat, group = id)) + 
  geom_polygon(aes(fill=ZuzuegeUDAR, group=id)) + geom_path(color="grey", alpha=0.0, size=0.5) +
  scale_fill_gradientn(colours = c(lowCOLOR,midCOLOR,hiCOLOR,superhiCOLOR), 
                       values = rescale(c(0,3,8,30)),
                       #guide = "colorbar", limits=c(3.8,22),
                       name=expression(paste('ZuzügeA',' ','(%)'))) +
  coord_map("polyconic",xlim = c(13.08,13.77),ylim = c(52.33,52.69)) + kartenlayout 
ZuzuegeUDARAMAP
