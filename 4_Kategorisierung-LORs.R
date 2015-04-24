#*************************************************
#*************************************************
#*************************************************
#
# Kategorisierung der LORs in Gentri-Kategorien 
#
#*************************************************
#*************************************************

# ____ Packages ______ ----------------------------------------------------------------

#install.packages("vioplot","ggplot2","sp","gridExtra","lattice",
#                 "scales","ENmisc","Hmisc","dplyr","grid")

library("vioplot")
library("ggplot2")
library("scales")
library("sp")
library("ENmisc")
library("gridExtra")
library("lattice")
library("Hmisc")
library("dplyr")
library("grid") # needed for arrow() function
library("quantreg")
library("latticeExtra")
library("car")

#*************************************************

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# I.) Quartilsbildung für MIETECHG, MIETECHGR, ALOSECHG, NICHT_ALOSE_HARTZCHG, ARMUT ------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

qntl <- quantile(LOR4reg@data$Mietechg); qntl 
LOR4reg@data$MietechgQNTL <- cut(LOR4reg@data$Mietechg, 
                                 qntl,
                                 labels=c("1.Quartil",
                                          "2.Quartil",
                                          "3.Quartil",
                                          "4.Quartil"),
                                 include.lowest = TRUE)
table(LOR4reg@data$MietechgQNTL)
PLOT_MietechgQNTL <- spplot(LOR4reg, zcol="MietechgQNTL", 
                           col.regions=c("darkblue","lightblue","orange","red"))


qntl <- quantile(LOR4reg@data$Mietechgr); qntl 
LOR4reg@data$MietechgrQNTL <- cut(LOR4reg@data$Mietechgr, 
                                  qntl,
                                  labels=c("1.Quartil",
                                           "2.Quartil",
                                           "3.Quartil",
                                           "4.Quartil"),
                                  include.lowest = TRUE)
table(LOR4reg@data$MietechgrQNTL, useNA="ifany")
PLOT_MietechgrQNTL <- spplot(LOR4reg, zcol="MietechgrQNTL", 
                             col.regions=c("darkblue","lightblue","orange","red"))

qntl <- quantile(LOR4reg@data$Alosechg, na.rm=T); qntl 
LOR4reg@data$AlosechgQNTL <- cut(LOR4reg@data$Alosechg, 
                                 qntl,
                                 labels=c("1.Quartil",
                                          "2.Quartil",
                                          "3.Quartil",
                                          "4.Quartil"),
                                 include.lowest = TRUE)
table(LOR4reg@data$AlosechgQNTL, useNA="ifany")

PLOT_AlosechgQNTL <- spplot(LOR4reg, zcol="AlosechgQNTL", 
                            col.regions=c("darkblue","lightblue","orange","red"))

qntl <- quantile(LOR4reg@data$nicht_Alose_Hartzchg, na.rm=T); qntl 
LOR4reg@data$nicht_Alose_HartzchgQNTL <- cut(LOR4reg@data$nicht_Alose_Hartzchg, 
                                             qntl,
                                             labels=c("1.Quartil",
                                                      "2.Quartil",
                                                      "3.Quartil",
                                                      "4.Quartil"),
                                             include.lowest = TRUE)
table(LOR4reg@data$nicht_Alose_HartzchgQNTL, useNA="ifany")
PLOT_nicht_Alose_HartzchgQNTL <- spplot(LOR4reg, zcol="nicht_Alose_HartzchgQNTL", 
                                        col.regions=c("darkblue","lightblue","orange","red"))

qntl <- quantile(LOR4reg@data$Armutchg, na.rm=T); qntl 
LOR4reg@data$ArmutchgQNTL <- cut(LOR4reg@data$Armutchg, 
                                             qntl,
                                             labels=c("1.Quartil",
                                                      "2.Quartil",
                                                      "3.Quartil",
                                                      "4.Quartil"),
                                 include.lowest = TRUE)
table(LOR4reg@data$ArmutchgQNTL, useNA="ifany")
PLOT_ArmutchgQNTL <- spplot(LOR4reg, zcol="ArmutchgQNTL", 
                                        col.regions=c("darkblue","lightblue","orange","red"))
#PLOT_ArmutchgQNTL

qntl <- quantile(LOR4reg@data$Armut.2007, na.rm=T); qntl 
LOR4reg@data$Armut.2007QNTL <- cut(LOR4reg@data$Armut.2007, 
                                 qntl,
                                 labels=c("1.Quartil",
                                          "2.Quartil",
                                          "3.Quartil",
                                          "4.Quartil"),
                                 include.lowest = TRUE)
table(LOR4reg@data$Armut.2007QNTL, useNA="ifany")
PLOT_Armut.2007QNTL <- spplot(LOR4reg, zcol="Armut.2007QNTL", 
                            col.regions=c("darkblue","lightblue","orange","red"))
#PLOT_Armut.2007QNTL

#grid.arrange(PLOT_MietechgQNTL,PLOT_MietechgrQNTL, nrow=2)
#grid.arrange(PLOT_AlosechgQNTL,PLOT_AlosechgQNTL, nrow=2)
#grid.arrange(PLOT_MietechgQNTL,PLOT_AlosechgQNTL, nrow=2)
#grid.arrange(PLOT_MietechgrQNTL,PLOT_AlosechgQNTL, nrow=2)
#grid.arrange(PLOT_nicht_Alose_HartzchgQNTL,PLOT_AlosechgQNTL, nrow=2)
#grid.arrange(PLOT_ArmutchgQNTL,PLOT_AlosechgQNTL, nrow=2)
#grid.arrange(PLOT_AlosechgQNTL,PLOT_MietechgrQNTL, nrow=2)

#grid.arrange(PLOT_MietechgQNTL,
#             PLOT_MietechgrQNTL,
#             PLOT_AlosechgQNTL,
#             PLOT_nicht_Alose_HartzchgQNTL,
#             nrow=2, ncol=2)

plot(LOR4reg@data$Alose.2012, LOR4reg@data$nicht_Alose_Hartz.2012)
plot(LOR4reg@data$Alosechg, LOR4reg@data$nicht_Alose_Hartzchg)
plot(LOR4reg@data$Alosechg, LOR4reg@data$Armutchg)
abline(a = 0, b = 1, col="red")
plot(LOR4reg@data$Alose.2007, LOR4reg@data$nicht_Alose_Hartz.2007)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# II.) NEUE Kategorisierung der LORs in GENTRI, NON-GENTRI, Andere ------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

###  0.10 < "Kontroll" < 0.90 = ENDGÜLTIGE DEFINITION 

LOR4reg@data$Gentri <- -1
LOR4reg@data$Gentri[LOR4reg@data$ArmutchgQNTL=="1.Quartil" & LOR4reg@data$MietechgrQNTL=="4.Quartil"] <- "Gentri"

# Gewichtete deskriptive Statistiken von Miete.2007 nur für LORs der Kategorie "Gentri hi" 
Gentri_Miete.2007_DESCR <- describe(x=LOR4reg@data$Miete.2007[LOR4reg@data$Gentri=="Gentri"],
                                    weights=LOR4reg@data$E_E.2007[LOR4reg@data$Gentri=="Gentri"],
                                    exclude.missing=TRUE)
Gentri_Miete.2007_DESCR
# Zum Vergleich die ungewichteten deskriptiven Statistiken
describe(x=LOR4reg@data$Miete.2007[LOR4reg@data$Gentri=="Gentri"],
         #weights=LOR4reg@data$E_E.2007[LOR4reg@data$Gentri=="Gentri"],
         exclude.missing=TRUE)

Gentri_Miete.2007_0.05 <- as.numeric(Gentri_Miete.2007_DESCR$counts[6])  # 0.05 Quantil
Gentri_Miete.2007_0.10 <- as.numeric(Gentri_Miete.2007_DESCR$counts[7])  # 0.10 Quantil
Gentri_Miete.2007_0.25 <- as.numeric(Gentri_Miete.2007_DESCR$counts[8])  # 0.25 Quantil
Gentri_Miete.2007_0.75 <- as.numeric(Gentri_Miete.2007_DESCR$counts[10]) # 0.75 Quantil
Gentri_Miete.2007_0.90 <- as.numeric(Gentri_Miete.2007_DESCR$counts[11]) # 0.90 Quantil
Gentri_Miete.2007_0.95 <- as.numeric(Gentri_Miete.2007_DESCR$counts[12]) # 0.95 Quantil

# Gewichtete deskriptive Statistiken von Armut.2007 nur für LORs der Kategorie "Gentri hi" 
Gentri_Armut.2007_DESCR <- describe(x=LOR4reg@data$Armut.2007[LOR4reg@data$Gentri=="Gentri"],
                                    weights=LOR4reg@data$E_E.2007[LOR4reg@data$Gentri=="Gentri"],
                                    exclude.missing=TRUE)
Gentri_Armut.2007_DESCR
# Zum Vergleich die ungewichteten deskriptiven Statistiken
describe(x=LOR4reg@data$Armut.2007[LOR4reg@data$Gentri=="Gentri"],
         #weights=LOR4reg@data$E_E.2007[LOR4reg@data$Gentri=="Gentri"],
         exclude.missing=TRUE)

Gentri_Armut.2007_0.05 <- as.numeric(Gentri_Armut.2007_DESCR$counts[6])  # 0.05 Quantil
Gentri_Armut.2007_0.10 <- as.numeric(Gentri_Armut.2007_DESCR$counts[7])  # 0.10 Quantil
Gentri_Armut.2007_0.25 <- as.numeric(Gentri_Armut.2007_DESCR$counts[8])  # 0.25 Quantil
Gentri_Armut.2007_0.75 <- as.numeric(Gentri_Armut.2007_DESCR$counts[10]) # 0.75 Quantil
Gentri_Armut.2007_0.90 <- as.numeric(Gentri_Armut.2007_DESCR$counts[11]) # 0.90 Quantil
Gentri_Armut.2007_0.95 <- as.numeric(Gentri_Armut.2007_DESCR$counts[12]) # 0.95 Quantil

LOR4reg@data$Gentri[LOR4reg@data$Armut.2007      >= Gentri_Armut.2007_0.10 & 
                         LOR4reg@data$Armut.2007 <= Gentri_Armut.2007_0.90 & 
                         LOR4reg@data$Miete.2007 >= Gentri_Miete.2007_0.10 & 
                         LOR4reg@data$Miete.2007 <= Gentri_Miete.2007_0.90 &
                         LOR4reg@data$Gentri!="Gentri"] <- "Kontroll"
LOR4reg@data$Gentri[(LOR4reg@data$Gentri!="Gentri" & LOR4reg@data$Gentri!="Kontroll")] <- "Andere"
LOR4reg@data$Gentri[is.na(LOR4reg@data$MietechgrQNTL) |
                         is.na(LOR4reg@data$ArmutchgQNTL)  |
                         LOR4reg@data$valid=="ungültig"] <- NA
LOR4reg@data$Gentri <- as.factor(LOR4reg@data$Gentri)

table(LOR4reg@data$Gentri,useNA="ifany")
table(LOR4reg@data$Gentri,LOR4reg@data$STADTRAUM,useNA="ifany")

spplot(LOR4reg, zcol="Gentri", 
       col.regions=c("blue","red","grey"))

##### Gentri Variable an Long Datensatz mergen #####
Gentri4long <- data.frame(LOR4reg@data$RAUMID,LOR4reg@data$MietechgrQNTL,LOR4reg@data$Gentri)
colnames(Gentri4long) <- c("RAUMID","MietechgrQNTL","Gentri")

source("/home/dao/Desktop/MasterArbeit/R_files/functions/merge_with_order_FUNCTION.R")
LORdataFULL   <- merge.with.order(LORdataFULL, Gentri4long, sort=F,
                                  by.x="RAUMID", by.y="RAUMID",
                                  all.x=T, all.y=T,
                                  keep_order=1)
#View(LORdataFULL[,c(1,2,3,4,5,125,127)])

# Neuer endgültiger long Datensatz
LORdataFULLvalid <- subset(LORdataFULL, !is.na(Gentri))

