#*************************************************
#*************************************************
#
# Kategorisierung der LORs in Gentri-Kategorien 
#
#*************************************************
#*************************************************

# ____ Packages ______ ----------------------------------------------------------------

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
               LOR4reg@data$RAUMID_NAME=="Motardstr.",][,c("Miete.2007",
                                                           "Miete.2012",
                                                           "Alose.2007",
                                                           "Alose.2012",
                                                           "nicht_Alose_Hartz.2007",
                                                           "nicht_Alose_Hartz.2012",
                                                           "Mietechg",
                                                           "Mietechgr",
                                                           "Alosechg",
                                                           "nicht_Alose_Hartzchg")] <- NA
LOR4reg@data$valid <- as.factor(ifelse(is.na(LOR4reg@data$FortzuegeR), 
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

symbols(x=LOR4reg@data$Alosechg,
        y=LOR4reg@data$Mietechgr, 
        circles=sqrt(LOR4reg@data$E_E.2012/ pi ),
        xlab="Arbeitslosigkeit Änderung",
        ylab="Rel. Mietpreisänderung",
        inches=0.2, fg="black", bg="red")
abline(v=weighted.mean(x=LOR4reg@data$Alosechg,
                       w=LOR4reg@data$E_E.2012,
                       na.rm=T),
       col="blue",lty=2,lwd=2)
abline(h=weighted.mean(x=LOR4reg@data$Mietechgr,
                       w=LOR4reg@data$E_E.2012,
                       na.rm=T),
       col="blue",lty=2,lwd=2)


par(fig=c(0,0.8,0,0.8), new=TRUE)
abline(v=weighted.mean(x=LOR4reg@data$Alosechg,
                       w=LOR4reg@data$E_E.2012,
                       na.rm=T),
       col="blue",lty=2,lwd=2)
abline(h=weighted.mean(x=LOR4reg@data$Mietechgr,
                       w=LOR4reg@data$E_E.2012,
                       na.rm=T),
       col="blue",lty=2,lwd=2)
plot(LOR4reg@data$Alosechg, LOR4reg@data$Mietechgr, 
     xlab="Arbeitslosigkeit Änderung",
     ylab="Rel. Mietpreisänderung",
     col=LOR4reg@data$Gentri,
     cex=1.5,pch=16)
par(fig=c(0,0.85,0.55,1), new=TRUE)
boxplot(LOR4reg@data$Alosechg, horizontal=TRUE, axes=FALSE)
par(fig=c(0.65,1,0,0.8),new=TRUE)
boxplot(LOR4reg@data$Mietechgr, axes=FALSE)
mtext("Enhanced Scatterplot", side=3, outer=TRUE, line=-3) 

z.cols <- cut(z, 3, labels = c("pink", "green", "yellow"))
plot(x,y, col = as.character(z.cols), pch = 16)

abline(v=weighted.mean(x=LOR4reg@data$Alosechg,
                       w=LOR4reg@data$E_E.2012,
                       na.rm=T),
       col="blue",lty=2,lwd=2)
abline(h=weighted.mean(x=LOR4reg@data$Mietechgr,
                       w=LOR4reg@data$E_E.2012,
                       na.rm=T),
       col="blue",lty=2,lwd=2)
plot(LOR4reg@data$Alosechg, LOR4reg@data$Mietechgr, 
     xlab="Arbeitslosigkeit Änderung",
     ylab="Rel. Mietpreisänderung",
     col=LOR4reg@data$Gentri,
     cex=1.5,pch=16)
legend(7,65,rev(unique(LOR4reg@data$Gentri)),col=(1:length(LOR4reg@data$Gentri)),pch=16,pt.cex=1.5)

p <- ggplot(LOR4reg@data, aes(Alosechg, Mietechgr, weight=E_E.2012)) + 
  geom_point(aes(size = E_E.2012, colour = Gentri)) + 
  scale_size_area(breaks=c(300, 1000, 5000, 10000, 30000), "Einwohner 2012", max_size=10) +
  scale_x_continuous("Änderung der Arbeitslosenquote") +
  scale_y_continuous("Rel. Änderung des Mietpreises")
p

View(LOR4reg@data[,c("RAUMID_NAME","BEZ_NAME","Alosechg","Mietechgr","E_E.2012")])
View(LOR4reg@data[,c("RAUMID_NAME","BEZ_NAME","Alose.2007","Mietechgr","E_E.2007")])

p <- ggplot(LOR4reg@data, aes(Alose.2007, Miete.2007, weight=E_E.2007)) + 
  geom_point(aes(size = E_E.2007, colour = Gentri)) + 
  scale_size_area(breaks=c(300, 1000, 5000, 10000, 30000), "Einwohner 2007", max_size=10) +
  scale_x_continuous("Arbeitslosenquote 2007") +
  scale_y_continuous("Mietpreises 2007")
p

boxplot(LOR4reg@data$FortzuegeR ~ LOR4reg@data$Gentri)
boxplot(LOR4reg@data$ZuzuegeR ~ LOR4reg@data$Gentri)
