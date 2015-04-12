library("sp")
library("maptools")

# Block Point Samples -------------------------------------------------------------

Sr1 = Polygon(cbind(c(2,1,4,10,5),c(2,4,5,5,2)))

Berlin = Polygons(list(Sr1), "Berlin")

Berlinsample = SpatialPolygons(list(Berlin), 1:1)
Berlinsample <- SpatialPolygonsDataFrame(Berlinsample,data.frame(c("Berlin")), match.ID=F)

plot(Berlinsample)

x1 = c(2,2.5,3.5)
y1 = c(3.5,2.5,4.1)
points(x=x1,y=y1)

x2 = c(4.3,2.8)
y2 = c(2.3,2.2)
points(x=x2,y=y2)

x3 = c(5,7.2,8)
y3 = c(4.4,4,4.5)
points(x=x3,y=y3)

xALL <- c(x1,x2,x3)
yALL <- c(y1,y2,y3)

par(mfrow=c(3,1)) 
plot(Berlinsample)
points(x=xALL,y=yALL, pch=8)
BlockPointLabels <- c("B 1", "B 2", "B 3", "B 4" ,"B 5", "B 6", "B 7", "B 8")
pointLabel(x=xALL, y=yALL, labels = paste("  ", BlockPointLabels, "  ", sep=""), cex=0.8)



# PLZ Samples -------------------------------------------------------------

Sr1 = Polygon(cbind(c(2,1,4,6,3),c(2,4,5,5,2)))
Sr2 = Polygon(cbind(c(3,6,10,5),c(2,5,5,2)))

PLZ_1 = Polygons(list(Sr1), "PLZ_1")
PLZ_2 = Polygons(list(Sr2), "PLZ_2")

PLZsample = SpatialPolygons(list(PLZ_1,PLZ_2), 1:2)
PLZsample <- SpatialPolygonsDataFrame(PLZsample,data.frame(c("PLZ 1","PLZ 2")), match.ID=F)
colnames(PLZsample@data) <- "ID"

plot(PLZsample, col= 2:3)
invisible(text(getSpPPolygonsLabptSlots(PLZsample), labels=as.character(PLZsample$ID), cex=0.8, font=2))
points(x=xALL,y=yALL, pch=8)
#pointLabel(x=xALL, y=yALL, labels = paste("  ", BlockPointLabels, "  ", sep=""), cex=0.7)


# LOR Samples -------------------------------------------------------------

Sr1 = Polygon(cbind(c(2,4,4,1,2),c(2,3,5,4,2)))
Sr2 = Polygon(cbind(c(5,4,2,5),c(2,3,2,2)))
Sr3 = Polygon(cbind(c(4,4,5,10,4),c(5,3,2,5,5)))

LOR_1 = Polygons(list(Sr1), "LOR_1")
LOR_2 = Polygons(list(Sr2), "LOR_2")
LOR_3 = Polygons(list(Sr3), "LOR_3")
LORsample = SpatialPolygons(list(LOR_1,LOR_2,LOR_3), 1:3)
LORsample <- SpatialPolygonsDataFrame(LORsample,data.frame(c("LOR 1","LOR 2","LOR 3")), match.ID=F)
colnames(LORsample@data) <- "ID"

plot(LORsample, col = 5:8)
invisible(text(getSpPPolygonsLabptSlots(LORsample), labels=as.character(LORsample$ID), cex=0.8, font=2))
points(x=xALL,y=yALL, pch=8)
#pointLabel(x=xALL, y=yALL, labels = paste("  ", BlockPointLabels, "  ", sep=""), cex=0.7)



