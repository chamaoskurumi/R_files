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
LOR4leaflet@data <- subset(LOR4leaflet@data, select=c(RAUMID, RAUMID_NAME,BEZ_NAME,
                                                      Miete.2012, Miete.2007))
LORjson <- toGeoJSON(data=LOR4leaflet, dest=tempdir())
brksIntervalls <- classIntervals(LOR4leaflet@data$Miete.2012, n=10); brksIntervalls
brks           <- round(brksIntervalls$brks, digits=1); brks
clrs <- colorRampPalette(c("blue","yellow", "red"))(length(brks))
stlMiete.2007 <- styleGrad(prop="Miete.2007", breaks=brks, style.val=clrs, 
                 out=1, leg="Median Angebotsmiete 2007", 
                 lwd=1, col="white", alpha=0.4)
SPleaflet1  <- leaflet(data=LORjson, dest="/home/dao/Desktop",
                      title="Median Angebotsmiete 2007 JSON", base.map="darkmatter",
                      style=stlMiete.2007, popup="*", incl.data = FALSE)
SPleaflet1
viewer(SPleaflet1)
write.csv(LORdataFULL, file="/home/dao/Desktop/LONGdata.csv")
write.csv(LOR4reg@data, file="/home/dao/Desktop/WIDEdata.csv")
