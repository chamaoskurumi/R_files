#********************************************
#                                           #
#         Explorative - Datenanalyse        #
#                                           #
#********************************************

install.packages("googleVis","ggplot2", "beanplot","rgdal","sp",
                 "leafletR","plotGoogleMaps","GeoXp",
                 "gridExtra")
require(devtools)
install_github('rCharts', 'ramnathv')
library("rCharts")
library("googleVis")
library("ggplot2")
library("beanplot")
library("rgdal")
library("sp")
library("leafletR")
library("plotGoogleMaps")
library("GeoXp")
library("gridExtra")

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Variablen für Explorative Datenanalys generieren  =================

ExDF           <- LORattrFULLwide
ExDF$PDAU5chg  <- ExDF$PDAU5.2008-ExDF$PDAU5.2013
ExDF$PDAU10chg <- ExDF$PDAU10.2008-ExDF$PDAU10.2013

names(ExDF)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Violin Plots  =================

vio_PDAU5chg <- ggplot(ExDF, 
                       aes(BEZ_NAME, PDAU5chg)) + 
                geom_violin(weights=ExDF$E.E.2013)

vio_PDAU5chg2 <- ggplot(ExDF, 
                       aes(BEZ_NAME, PDAU5chg))+
                geom_violin() + geom_jitter(height = 0)
vio_PDAU5chg2

grid.arrange(vio_PDAU5chg, vio_PDAU5chg2, nrow=2)

vio_PDAU5chg <- ggplot(ExDF, 
                       aes(BEZ_NAME, PDAU10chg))
vio_PDAU5chg + geom_violin()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# BEISPIELE rCHARTS

names(iris) = gsub("\\.", "", names(iris))
rPlot(SepalLength ~ SepalWidth | Species, data = iris, color = 'Species', type = 'point')

hair_eye = as.data.frame(HairEyeColor)
rPlot(Freq ~ Hair | Eye, color = 'Eye', data = hair_eye, type = 'bar')

r1 <- rPlot(mpg ~ wt | am + vs, data = mtcars, type = "point", color = "gear");r1

hair_eye_male <- subset(as.data.frame(HairEyeColor), Sex == "Male")
n1 <- nPlot(Freq ~ Hair, group = "Eye", data = hair_eye_male, type = "multiBarChart");n1
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ??? =================

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ??? =================