#********************************************
#                                           #
#         Explorative - Datenanalyse        #
#                                           #
#********************************************

install.packages("googleVis","ggplot2", "beanplot","rgdal","sp","leafletR","plotGoogleMaps","GeoXp")
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

#********************************************
# ???  ***********************
#********************************************

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

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ??? =================