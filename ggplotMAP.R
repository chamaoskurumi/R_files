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

BZKLONGLAT <- spTransform(BZKt,CRS("+proj=longlat"))
BZK.fort  <- fortify(BZKLONGLAT, region="BezName")
BZKdf     <- BZK@data
colnames(BZKdf)[2] <- "id"
BZK.fort <- join(BZK.fort, BZKdf, by="id")


#### 2.) GGPLOT ohne Map drunter - funktioniert.
# außer das mit den Farben und so, das kann man noch schöner machen. Und weiss auf 0 setzen und so...hatten wir ja schon
# mal kurz besprochen
# könnten wir mal in einer nerdsession optimieren
breaks <- c(-5,0,5,10,15)
p <- ggplot(LOR4reg.fort, aes(x=long, y=lat, group = id)) + 
  geom_polygon(aes(fill=Mietechgr, group=id)) + geom_path(color="grey", alpha=0.0) +   
  theme_bw() + 
  scale_fill_gradientn(limits = c(min(LOR4reg.fort$Mietechgr),
                                               max(LOR4reg.fort$Mietechgr)),
                                    colours=c("yellow","orange","red"),
                                    breaks=breaks) + coord_equal(ratio=1.5)
p + geom_polygon(aes(group=id),data=BZK.fort, fill=NA, colour="black") 

#### 3.) Jetzt gehts endlich um GGMAP. Als erstes muss man da ne Karte holen mit get_map. 
# Das funktioniert auch:

bbox <- c(min(LOR4reg.fort$long),min(LOR4reg.fort$lat),max(LOR4reg.fort$long),max(LOR4reg.fort$lat))

BerlinGMAP <- get_map(location = bbox,
                      maptype = "terrain", 
                      source = c("google"),
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



