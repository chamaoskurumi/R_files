setwd("/home/dao/Desktop/MasterArbeit/R_files/KNITR/")
load(file = "FULL_FINAL_WORKSPACE.Rdata")

#### packages ####
library("ggplot2")
library("rgdal")
library("sp")
library("plyr")
library("ggplot2")
library("scales")
library("ENmisc")
library("gridExtra")
library("Hmisc")
library("dplyr")
library("car")
library("vcd")
library("plyr")
library("maptools")
library("sp")
library("rgdal")
library("classInt")
library("rstudio")

if(!require(devtools)) { install.packages('devtools'); require(devtools) }
devtools::install_github('chgrl/leafletR')
library("leafletR")

source("/home/dao/Desktop/MasterArbeit/R_files/functions/myleaflet_FUNCTION.R")

LOR4leaflet <- LOR4reg
LOR4leaflet@data <- subset(LOR4leaflet@data, select=c(RAUMID, RAUMID_NAME,BZR,BZR_NAME,
                                                      PGR,PRG_NAME,BEZ,BEZ_NAME,                   
                                                      STADTRAUM,FL_HA,
                                                      Miete.2012, Miete.2007))
LORjson <- toGeoJSON(data=LOR4leaflet, dest=tempdir())
brksIntervalls <- classIntervals(LOR4leaflet@data$Miete.2012, n=10); brksIntervalls
brks           <- round(brksIntervalls$brks, digits=1); brks
#brks <- seq(3, max(LOR@data$EWdichte2012, na.rm=T), by=1000); length(brks)
clrs <- colorRampPalette(c("yellow", "red"))(length(brks))
stlMiete.2007 <- styleGrad(prop="Miete.2007", breaks=brks, style.val=clrs, 
                 out=1, leg="Median Angebotsmiete 2007", 
                 lwd=1, col="white", alpha=0.4)
SPleaflet1  <- leaflet(data=LORjson, dest="/home/dao/Desktop",
                      title="Median Angebotsmiete 2012", base.map="positron",
                      style=stlMiete.2007, popup="*", incl.data = TRUE)
SPleaflet1
viewer(SPleaflet1)

stlMiete.2007 <- styleGrad(prop="Miete.2007", breaks=brks, style.val=clrs, 
                           out=1, leg="Median Angebotsmiete 2007", 
                           lwd=1, col="white", alpha=0.4)