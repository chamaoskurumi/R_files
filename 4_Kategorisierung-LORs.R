#*************************************************
#*************************************************
#
# Kategorisierung der LORs in Gentri-Kategorien 
#
#*************************************************
#*************************************************

library("vioplot")
library("ggplot2")
library("sp")
require("gridExtra")
require("lattice")

#*************************************************

LOR@data <- LORattrFULL

LOR@data$E_E_u300 <- as.factor(ifelse(LOR@data$E_E.2007 < 300, 
                                       c("unter 300EW"), 
                                       c("über  300EW")))
spplot(LOR, zcol="E_E_u300")

# Für die Regression und Kategorisierung arbeiten wir mit dem Datensatz LOR4reg, 
#    um im Originaldatensatz LOR keine 
LOR4reg <- LOR

# Wir überschreiben die Miete & Alosingkeit
# mit NAs, damit sie nicht bei der Kategorisierung und Regression mitverwendet werden
LOR4reg@data[LOR4reg@data$E_E_u300=="unter 300EW" | 
               LOR4reg@data$RAUMID_NAME=="Motardstr.",][,c("Mietechg",
                                                           "Mietechgr",
                                                           "Alosechg",
                                                           "nicht_Alose_Hartzchg")] <- NA
LOR4reg@data$valid <- as.factor(ifelse(is.na(LOR4reg@data$FortzuegeRel), 
                                       c("ungültig"), 
                                       c("gültig")))

# hier sollte eigentlich gewichtet werden...vielleicht mit weighted quantiles oder so?!?
# weighted boxplot?
vioplot(na.omit(LOR4reg@data$Mietechg))
vioplot(na.omit(LOR4reg@data$nicht_Alose_Hartzchg))

ggplot(LOR4reg, aes(BEZ_NAME, PDAU5chg), weight=E_E.2012) + 
  geom_jitter(aes(colour=BEZ_NAME, size = E_E.2012), 
              position = position_jitter(width = .3)) +
  scale_size_continuous(breaks=c(1000,5000,10000,20000), range=c(1,10)) +
  geom_hline(yintercept=0, col="black")


qntl <- quantile(LOR4reg@data$Mietechg, na.rm=T); qntl 
LOR4reg@data$MietechgQNTL <- cut(LOR4reg@data$Mietechg, 
                                 qntl,
                                 labels=c("1.Quartil",
                                          "2.Quartil",
                                          "3.Quartil",
                                          "4.Quartil"))
table(LOR4reg@data$MietechgQNTL)
PLOT_MietechgQNTL <- spplot(LOR4reg, zcol="MietechgQNTL", 
                           col.regions=c("darkblue","lightblue","orange","red"))


qntl <- quantile(LOR4reg@data$Mietechgr, na.rm=T); qntl 
LOR4reg@data$MietechgrQNTL <- cut(LOR4reg@data$Mietechgr, 
                                  qntl,
                                  labels=c("1.Quartil",
                                           "2.Quartil",
                                           "3.Quartil",
                                           "4.Quartil"))
table(LOR4reg@data$MietechgrQNTL)
PLOT_MietechgrQNTL <- spplot(LOR4reg, zcol="MietechgrQNTL", 
                             col.regions=c("darkblue","lightblue","orange","red"))

qntl <- quantile(LOR4reg@data$Alosechg, na.rm=T); qntl 
LOR4reg@data$AlosechgQNTL <- cut(LOR4reg@data$Alosechg, 
                                 qntl,
                                 labels=c("1.Quartil",
                                          "2.Quartil",
                                          "3.Quartil",
                                          "4.Quartil"))
table(LOR4reg@data$AlosechgQNTL)
PLOT_AlosechgQNTL <- spplot(LOR4reg, zcol="AlosechgQNTL", 
                            col.regions=c("darkblue","lightblue","orange","red"))

qntl <- quantile(LOR4reg@data$nicht_Alose_Hartzchg, na.rm=T); qntl 
LOR4reg@data$nicht_Alose_HartzchgQNTL <- cut(LOR4reg@data$nicht_Alose_Hartzchg, 
                                             qntl,
                                             labels=c("1.Quartil",
                                                      "2.Quartil",
                                                      "3.Quartil",
                                                      "4.Quartil"))
table(LOR4reg@data$nicht_Alose_HartzchgQNTL)
PLOT_nicht_Alose_HartzchgQNTL <- spplot(LOR4reg, zcol="nicht_Alose_HartzchgQNTL", 
                                        col.regions=c("darkblue","lightblue","orange","red"))

grid.arrange(PLOT_MietechgQNTL,PLOT_MietechgrQNTL, nrow=2)
grid.arrange(PLOT_AlosechgQNTL,PLOT_AlosechgQNTL, nrow=2)
grid.arrange(PLOT_MietechgQNTL,PLOT_AlosechgQNTL, nrow=2)
grid.arrange(PLOT_MietechgrQNTL,PLOT_AlosechgQNTL, nrow=2)

grid.arrange(PLOT_MietechgQNTL,
             PLOT_MietechgrQNTL,
             PLOT_AlosechgQNTL,
             PLOT_nicht_Alose_HartzchgQNTL,
             nrow=2, ncol=2)


LOR4reg@data$Gentri <- -1
LOR4reg@data$Gentri[LOR4reg@data$AlosechgQNTL=="4.Quartil" & LOR4reg@data$MietechgrQNTL=="4.Quartil"] <- "Gentri hi"
LOR4reg@data$Gentri[LOR4reg@data$AlosechgQNTL=="3.Quartil" & LOR4reg@data$MietechgrQNTL=="4.Quartil" | 
                      LOR4reg@data$AlosechgQNTL=="4.Quartil" & LOR4reg@data$MietechgrQNTL=="3.Quartil" | 
                      LOR4reg@data$AlosechgQNTL=="3.Quartil" & LOR4reg@data$MietechgrQNTL=="3.Quartil" ] <- "Gentri lo"
LOR4reg@data$Gentri[(LOR4reg@data$Gentri!="Gentri hi" & 
                       LOR4reg@data$Gentri!="Gentri lo")] <- "Non Gentri"
LOR4reg@data$Gentri[is.na(LOR4reg@data$MietechgrQNTL) |
                      is.na(LOR4reg@data$AlosechgQNTL)  |
                      LOR4reg@data$valid=="ungültig"] <- NA
LOR4reg@data$Gentri <- as.factor(LOR4reg@data$Gentri)
table(LOR4reg@data$Gentri)
spplot(LOR4reg, zcol="Gentri", 
       col.regions=c("red","yellow","darkblue"))

boxplot(Miete.2007 ~ Gentri, data=LOR4reg@data)
boxplot(Alose.2007 ~ Gentri, data=LOR4reg@data)
boxplot(nicht_Alose_Hartz.2007 ~ Gentri, data=LOR4reg@data)

vioplot(na.omit(LOR4reg@data$Miete.2007[LOR4reg@data$Gentri=="Gentri hi"]), 
        na.omit(LOR4reg@data$Miete.2007[LOR4reg@data$Gentri=="Gentri lo"]), 
        na.omit(LOR4reg@data$Miete.2007[LOR4reg@data$Gentri=="Non Gentri"]), 
        names=c("Gentri hi", "Gentri lo", "Non Gentri"),
        col="gold")

vioplot(na.omit(LOR4reg@data$Alose.2007[LOR4reg@data$Gentri=="Gentri hi"]), 
        na.omit(LOR4reg@data$Alose.2007[LOR4reg@data$Gentri=="Gentri lo"]), 
        na.omit(LOR4reg@data$Alose.2007[LOR4reg@data$Gentri=="Non Gentri"]), 
        names=c("Gentri hi", "Gentri lo", "Non Gentri"),
        col="gold")

vioplot(na.omit(LOR4reg@data$nicht_Alose_Hartz.2007[LOR4reg@data$Gentri=="Gentri hi"]), 
        na.omit(LOR4reg@data$nicht_Alose_Hartz.2007[LOR4reg@data$Gentri=="Gentri lo"]), 
        na.omit(LOR4reg@data$nicht_Alose_Hartz.2007[LOR4reg@data$Gentri=="Non Gentri"]), 
        names=c("Gentri hi", "Gentri lo", "Non Gentri"),
        col="gold")

by(LOR4reg@data$Miete.2007, LOR4reg@data$Gentri, summary)
by(LOR4reg@data$Alose.2007, LOR4reg@data$Gentri, summary)
by(LOR4reg@data$nicht_Alose_Hartz.2007, LOR4reg@data$Gentri, summary)

GentriMiete.2007_1quartil <- quantile(LOR4reg@data$Miete.2007[LOR4reg@data$Gentri=="Gentri hi"], na.rm=T)[[2]]
GentriMiete.2007_3quartil <- quantile(LOR4reg@data$Miete.2007[LOR4reg@data$Gentri=="Gentri hi"], na.rm=T)[[4]]

GentriAlose.2007_1quartil <- quantile(LOR4reg@data$Alose.2007[LOR4reg@data$Gentri=="Gentri hi"], na.rm=T)[[2]]
GentriAlose.2007_3quartil <- quantile(LOR4reg@data$Alose.2007[LOR4reg@data$Gentri=="Gentri hi"], na.rm=T)[[4]]

subset(LOR4reg@data,
       (Miete.2007 > GentriMiete.2007_1quartil &
          Miete.2007 < GentriMiete.2007_3quartil &
          Alose.2007 > GentriAlose.2007_1quartil &
          Alose.2007 < GentriAlose.2007_3quartil &
          (Gentri=="Non Gentri" | Gentri=="Gentri lo")),
       select=c(RAUMID_NAME,BEZ_NAME,STADTRAUM))
LOR4reg@data$RAUM_NAME[LOR4reg@data$Alose.2007)
