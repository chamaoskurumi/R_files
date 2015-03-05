
# install.packages(c("spdep",      "sp",      "maptools", "lattice",
#                   "car",        "ggplot2", "spatstat", "RColorBrewer",
#                   "colorspace", "ggplot2", "hexbin",   "vioplot",
#                   "vcd",        "ncf",     "spgwr",    "leaps",
#                   "RANN",       "lmtest"))

#### packages ####
library("spdep")
library("sp")
library("maptools")
library("lattice")
library("car")
library("ggplot2")
library("RColorBrewer")
library("colorspace") 
library("hexbin")
library("vioplot")
library("vcd")
library("ncf")
install.packages("spgwr")
library("spgwr")
library("leaps")
library("RANN")
library("lmtest")
library('MASS')
library('lattice')
library('coda')
library('MCMCpack')
library('spam')
library('truncdist')
library('stats4')
library('evd')
library('CARBayes')
library('LearnBayes')
library('sp')
library('spdep')
library('splines')
library('boot')
library('deldir')
library('foreign')
library('grid')
library('maptools')
library('Matrix')
library('nlme')
library('shapefiles')
library('sp')

###########


# Erstellen von Nachbarschaftslisten
coords    <- coordinates(LOR)
nlist     <- poly2nb(LOR, row.names = rownames(LOR@data$RAUMID)); nlist

# Visualisierung der Nachbarschaftslisten
plot(LOR, col = "grey")
plot(nlist,coords,add=T)

# title(main = "poly2nb Nachbarschaftsplot")
table(card(nlist))

# Erstellen der Gewichtungsmatrizen
W_1       <- nb2listw(nlist, style="W")
W_1mat    <- nb2mat(nlist,   style="W")

# Herrscht Autokorrelation in unseren Daten vor?
# Moran (exact) saddlepoint approx. Test

names(LOR@data)


no.na.LMdata <- na.omit(subset(LOR@data, select=c(WanderSaldosum, DAU5chg, DAU10chg,     
                                                  PDAU5chg,PDAU10chg,     WLEINFchg,     WLMITchg,     
                                                  WLGUTchg,WLEINFRchg,    WLMITRchg,     WLGUTRchg,    
                                                  Alosechg,Alose_langzeitchg ,          Alose_u25chg,  Hartz_u15chg, 
                                                  MH_Echg, MH_ERchg,MH_U18chg,     MH_U18RU18chg,
                                                  HK_EU15chg,    HK_EU27hg,     HK_Turkchg,    HK_Arabchg,   
                                                  HK_EheJugchg,  HK_EU15Rchg,   HK_EU27Rhg,    HK_TurkRchg,  
                                                  HK_ArabRchg,   HK_EheJugRchg, HK_EU15RMHchg, HK_EU27RMHhg, 
                                                  HK_TurkRMHchg, HK_ArabRMHchg, HK_EheJugRMHchg, 
                                                  Mietechg,Mietechgr,E_E.2013)))#; View(no.na.LMdata)

summary(lm.1 <- lm(WanderSaldosum ~  DAU5chg+ DAU10chg+     
  PDAU5chg+PDAU10chg+     WLEINFchg+     WLMITchg+     
  WLGUTchg+WLEINFRchg+    WLMITRchg+     WLGUTRchg+    
  Alosechg+Alose_langzeitchg +          Alose_u25chg+  Hartz_u15chg+ 
  MH_Echg+ MH_ERchg+MH_U18chg+     MH_U18RU18chg+
  HK_EU15chg+    HK_EU27hg+     HK_Turkchg+    HK_Arabchg+   
  HK_EheJugchg+  HK_EU15Rchg+   HK_EU27Rhg+    HK_TurkRchg+  
  HK_ArabRchg+   HK_EheJugRchg+ HK_EU15RMHchg+ HK_EU27RMHhg+ 
  HK_TurkRMHchg+ HK_ArabRMHchg+ HK_EheJugRMHchg +            
  Mietechg+Mietechgr, data=no.na.LMdata, weights=(1/sqrt(E_E.2013))))

library("MASS")
steplm.1 <- step(lm.1, data= no.na.LMdata,
             direction = "both")
steplm.1$anova

step <- stepAIC(lm.1, direction="both",na.action = na.omit)
step$anova # display results 

summary(WanderSaldosum ~ DAU5chg + DAU10chg + PDAU5chg + PDAU10chg + 
  WLEINFchg + WLMITchg + WLGUTchg + WLMITRchg + Alosechg + 
  Alose_langzeitchg + Alose_u25chg + MH_Echg + MH_ERchg + HK_EU15Rchg + 
  HK_EU27Rhg + HK_EU27RMHhg + Mietechg + Mietechgr
  
summary(lm2 <- lm(WanderSaldosum ~ PDAU10chg + WLEINFchg + WLMITchg + WLGUTchg + 
          WLMITRchg + Alosechg + Alose_langzeitchg + MH_ERchg + HK_EU27hg + 
          HK_EheJugchg + HK_EU15Rchg + HK_EU27Rhg + HK_ArabRchg + HK_EheJugRchg + 
          HK_EU15RMHchg + HK_EU27RMHhg + HK_ArabRMHchg + HK_EheJugRMHchg + 
          Mietechg + Mietechgr,data=no.na.LMdata, weights=(1/sqrt(E_E.2013))))

summary(lm3 <- lm(Alosechg ~ Mietechg + Mietechgr,data=no.na.LMdata, weights=(1/sqrt(E_E.2013))))

install.packages("GGally")
library(GGally)
source("/home/dao/Desktop/MasterArbeit/R_files/functions/ggally_cor_FUNCTION.R")
assignInNamespace("ggally_cor", ggally_cor, "GGally")
ggpairs(LOR@data[ ,c("Miete.2013","Mietechgr",
                     "Alosechg","Alose_langzeitchg")],
        lower=list(continuous="smooth", params=c(colour="blue")),
        diag=list(continuous="density",params=c(colour="blue")), 
        upper=list(params=list(corSize=15)), axisLabels='show') 


data <- data.frame()





model <- lm(formula=as.formula(paste(paste(response,'~', sep=''),
                                     paste(predictors,collapse='+'), sep='')),
            no.na.data)
step(model)

names(LOR@data)

lm.morantest.sad(lm.1, W_1, zero.policy=TRUE)
lm.morantest.exact(lm.1, W_1, zero.policy=TRUE)

moran.plot(price, W_3, zero.policy=TRUE, pch=19)

lm.LMtests(lm.1, W_1, test="all", zero.policy=T)


# Fitting SAR Models

lm.SAR1 <- lagsarlm(price ~ rooms+type+sales+driveshop+rooms,
                    data = DATA, listw = W_1, zero.policy=TRUE, na.omit)
print(lm.SAR1$rho, digits=3)
summary(lm.SAR1)


# Fitting SMA Models

lm.SMA1 <- errorsarlm(price ~ rooms+type+sales+driveshop,
                      data = DATA, listw = W_1, zero.policy=TRUE, na.omit)
print(lm.SMA1$lambda, digits=3)
summary(lm.SMA1)

# Test auf Heteroskedastie in SAR oder SMA

bptest.sarlm(lm.SAR1)

bptest.sarlm(lm.SMA1)


# §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
# GWR
# §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§

DistanzMatrix     <- spDists(DATA, longlat = FALSE) # Distanzen Matrix
QuadDistanzMatrix <- DistanzMatrix*DistanzMatrix
W   <- gwr.bisquare(QuadDistanzMatrix, 1500)   # 1.Argument: vector of sqrd distances between obs

# 2.Argument: distance at which weights are set to zero

# Wir schauen uns mal ein Beispiel eines Punktes und seiner Gewichte an
select.spatial(DATA, digitize = TRUE, pch = "+", rownames = F)
plot((DistanzMatrix[100,]),gwr.bisquare(QuadDistanzMatrix,1500)[100,], type="p", main="Kernel um i-te Beobachtung")
abline(v = 1500, col="red")

# Das ist ein Befehl zu Optimierung der bandwith h
h.bisq <- gwr.sel(price ~ type+sales+driveshop+rooms, 
                  data=DATA, gweight=gwr.bisquare);h.bisq
# Da ändert sich nichts wenn man relcrime rausnimmt

# Nun das Fitting der gwr Modelle ohne relcrime
gwr1 <- gwr(price ~ type+sales+driveshop+rooms,
            data=DATA, bandwidth=h.bisq, gweight=gwr.bisquare, hatmatrix=TRUE)
gwr1

gwr.morantest(gwr1, W_1, zero.policy = FALSE)
########################################################
# Diagnostik der gwr Modelle
########################################################
# Zur Erinnerung unser SAR Modell (Coef und p-Wert von "relcrime")
summary(lm.SAR1)
gwr1

SPPLOT.gwr1.relcrime  <- spplot(gwr1$SDF, "relcrime",
                                sp.layout=list(northarrow, scalebar, text1, text2),
                                at=seq(-4, 1, length.out=101),
                                col.regions=heat.colors(n=101, alpha=1))
SPPLOT.gwr1.relcrime

SPPLOT.gwr1.rooms   <- spplot(gwr1$SDF, "rooms",
                              sp.layout=list(northarrow, scalebar, text1, text2),
                              at=seq(23, 45, length.out=101),
                              col.regions=heat.colors(n=101, alpha=1))
SPPLOT.gwr1.rooms

SPPLOT.gwr1.sales   <- spplot(gwr1$SDF, "sales",
                              sp.layout=list(northarrow, scalebar, text1, text2),
                              at=seq(-0.1, 0.4, length.out=101),
                              col.regions=heat.colors(n=101, alpha=1))
SPPLOT.gwr1.sales

SPPLOT.gwr1.driveshop   <- spplot(gwr1$SDF, "driveshop",
                                  sp.layout=list(northarrow, scalebar, text1, text2),
                                  at=seq(-22, 5, length.out=101),
                                  col.regions=heat.colors(n=101, alpha=1))
SPPLOT.gwr1.driveshop