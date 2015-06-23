library("ggplot2")
library("ggmap")
library("rgdal")
library("sp")
library("plyr")

#### 0.) Daten einlesen #####

#load("/KISSLASPFAD/daten_fuer_kissla.Rdata")

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


#### 2.) GGPLOT

LOR4reg.fort$Gentri   <- factor(LOR4reg.fort$Gentri,levels(LOR4reg.fort$Gentri)[c(2,3,1)])
LOR4reg.fort$GentriA  <- factor(LOR4reg.fort$GentriA,levels(LOR4reg.fort$GentriA)[c(2,3,1)])

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


#### 3.) Jetzt gehts endlich um GGMAP. Als erstes muss man da ne Karte holen mit get_map. 
# Das funktioniert auch:

bbox <- c(min(LOR4reg.fort$long),min(LOR4reg.fort$lat),max(LOR4reg.fort$long),max(LOR4reg.fort$lat))

BerlinGMAP <- get_map(location = bbox,
                      maptype = "terrain", 
                      source = c("osm"),
                      filename = "BerlinGMAP",
                      color= c("color"))

BerlinGMAP <- get_map(location = bbox,
                      maptype = "roadmap", 
                      source = c("google"),
                      filename = "BerlinGMAP",
                      color= c("color"))

BerlinGMAP <- get_map(location = bbox,
                      maptype = "toner-background", 
                      source = c("stamen"),
                      filename = "BerlinGMAP",
                      color= c("color"),
                      zoom=11)

BerlinGMAP <- get_map(location = bbox,
                      maptype = "toner", 
                      source = c("stamen"),
                      filename = "BerlinGMAP",
                      color= c("color"),
                      zoom=11)


# Karte passt eigentlich
ggmap(BerlinGMAP)


# so jetzt machen wir die ggmaps. Sieht aber alles noch nich so fein aus.
# Im prinzip müssen wir das jetzt optimieren...ich weiss noch nicht welche source wir nehmen sollen (gmap, osm oder stamen) und auch nicht wie man
# die plots dann am schönsten macht...das könnten wir zusammen in einer nerdsession optimieren.

ggmap(ggmap = BerlinGMAP, extend="device") + geom_polygon(data=LOR4reg.fort, aes(x=long, y=lat,fill=Armutchg, group=id), alpha=0.6, size=0.2)

ggmap(ggmap = BerlinGMAP, darken=0.6, legend = 'topright') +  geom_polygon(data=LOR4reg.fort, aes(x=long, y=lat,fill=Armutchg, group=id),
                                          alpha=0.6, color="grey") + 
  scale_fill_gradientn(limits = c(min(LOR4reg.fort$Armutchg),
                                  max(LOR4reg.fort$Armutchg)),
                       colours=c("blue","white","red"),
                       breaks=breaks) 

ggmap(ggmap = BerlinGMAP, darken=0.6, legend = 'topright') + 
  stat_density2d(aes(x = long, y = lat, fill = Armutchg), data = LOR4reg.fort, bins = 10,geom = "density2d")

ggmap(ggmap = BerlinGMAP, darken=0.6, legend = 'topright') + geom_polygon(data=LOR4reg.fort, aes(x=long, y=lat,fill=Armutchg, group=id), alpha=0.6, color="transparent") + 
 # scale_fill_gradientn(limits = c(min(LOR4reg.fort$Armutchg), max(LOR4reg.fort$Armutchg)), colours=c("blue","white","red"), breaks=breaks)
  stat_density2d(aes(x = long, y = lat, fill = ..level.., alpha = ..level..), 
                 size = 0.5, bins = 20, data = LOR4reg.fort, geom = "polygon", contour=T) +
 scale_alpha(range = c(.4, .75), guide = FALSE) +
 guides(fill = guide_colorbar(barwidth = 1.5, barheight = 10)) +
 scale_fill_gradient('Armutsänderung')

################################
################################
# Versuche LABELS in die Bezirke zu bekommen


p <- ggplot() + geom_text(data=test, aes(x=x, y=y, label=BEZ_NAME))
+ geom_polygon(LOR4reg.fort, aes(x=long, y=lat, group = id))
geom_polygon(aes(fill=Mietechgr, group=id)) + geom_path(color="grey", alpha=0.0, size=0.5) + 
  scale_fill_gradient2(low = "#0570b0", mid = "#fee0d2", high ="#e31a1c",midpoint = 0) +
  geom_polygon(aes(group=id),data=BZK.fort, fill=NA, colour="grey50", alpha=0.8) + 
  theme(legend.position =c(0.9,0.8), 
        line = element_blank(), 
        rect= element_blank(),
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) + 
  coord_map("polyconic") + 
  guides(fill = guide_colourbar(barheight=6)) +
  geom_path(data=S_Bahn.fort, aes(x=long, y=lat), linetype=2, size=0.7) 
p + geom_text(data=test, aes(label = BEZ_NAME, x = x, y = y),
              hjust=0.5, vjust=-0.5, colour="gold2", size=4)

setwd("/home/dao/Desktop/MasterArbeit/GentriMap/4 Geodaten")
BZK1     <- readOGR(dsn="Bezirke_GS/", layer="RBS_OD_BEZ_1412")
BZK1@proj4string
BZK      <- spTransform(BZK1, zielCRS)

BZKLONGLAT_pt    <- gCentroid(BZKLONGLAT,byid=TRUE); plot(BZKLONGLAT_pt)
BZKLONGLAT_ptdf  <- SpatialPointsDataFrame(coords = BZKLONGLAT_pt, 
                                           data = BZKLONGLAT@data)

test <- data.frame(BZKLONGLAT_pt@coords,BZKLONGLAT_ptdf@data$BezName)
names(test)[3] <- "BEZ_NAME"

ggplot() +  geom_text(data=test, aes(x=x, y=y, label=BEZ_NAME), color="red")

