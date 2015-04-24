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


#### 2.) GGPLOT ohne Map drunter - funktioniert.
# außer das mit den Farben und so, das kann man noch schöner machen. Und weiss auf 0 setzen und so...hatten wir ja schon
# mal kurz besprochen
# könnten wir mal in einer nerdsession optimieren
#breaks <- cut2(x, m=50))
p <- ggplot(LOR4reg.fort, aes(x=long, y=lat, group = id)) + 
  geom_polygon(aes(fill=Mietechgr, group=id)) + geom_path(color="grey", alpha=0.0, size=0.5) +   
  theme_bw() + 
  scale_fill_gradient2(low = "blue", mid = "white", high ="red",
                       midpoint = 0) #colours=c("yellow","orange","red")) + coord_equal(ratio=1.5)
p <- p + geom_polygon(aes(group=id),data=BZK.fort, fill=NA, colour="grey", alpha=0.8) + theme_bw() + 
  theme(legend.position =c(0.9,0.8), 
        line = element_blank(), 
        rect= element_blank(),
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) + coord_map("polyconic") + guides(fill = guide_colourbar(barheight=6))
p1 <- p + geom_path(data=S_Bahn.fort, aes(x=long, y=lat), linetype=2)
p1

p1 <- p1 + scaleBar(lon = 13.15, lat = 52.35, distanceLon = 500,distanceLat = 100, distanceLegend = 200, dist.unit = "km")

p <- ggplot(LOR4reg.fort, aes(x=long, y=lat, group = id)) + 
  geom_polygon(aes(fill=Armut.2012, group=id)) + geom_path(color="grey", alpha=0.0, size=0.5) +   
  theme_bw() + 
  scale_fill_gradientn(limits = c(min(LOR4reg.fort$Armut.2012),
                                  max(LOR4reg.fort$Armut.2012)),
                       colours=c("yellow","orange","red")) + coord_equal(ratio=1.5)
p <- p + geom_polygon(aes(group=id),data=BZK.fort, fill=NA, colour="grey", alpha=0.8) + theme_bw() + 
  theme(legend.position =c(0.9,0.8), 
        line = element_blank(), 
        rect= element_blank(),
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) + coord_map("polyconic") + guides(fill = guide_colourbar(barheight=6))
p1 <- p + geom_path(data=S_Bahn.fort, aes(x=long, y=lat), linetype=2)
p1

p <- ggplot(LOR4reg.fort, aes(x=long, y=lat, group = id)) + 
  geom_polygon(aes(fill=Gentri, group=id)) + geom_path(color="white", alpha=1, size=0.3) +   
  theme_bw() + coord_equal(ratio=1.5)
p <- p + geom_polygon(aes(group=id),data=BZK.fort, fill=NA, colour="grey", alpha=0.8, size=1) + theme_bw() + 
  theme(legend.position =c(0.9,0.8), 
        line = element_blank(), 
        rect= element_blank(),
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) + coord_map("polyconic") + guides(fill = guide_legend(barheight=6))
p1 <- p + geom_path(data=S_Bahn.fort, aes(x=long, y=lat), linetype=2, size=1)
p1



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



