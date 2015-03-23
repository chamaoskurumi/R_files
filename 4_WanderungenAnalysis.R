#******************************************
#
# "Fort- und Zuzüge"
#
# by Guido Schulz
#******************************************

#install.packages("outliers")
library("outliers")
library("sp")
library("ggplot2")
library("vioplot")
library("sp")
library("plyr")
library("devtools")
require("gridExtra")
require("lattice")
library("latticeExtra") # For layer spplot

source("/home/dao/Desktop/MasterArbeit/R_files/functions/merge_with_order_FUNCTION.R")

#******************************************************************************************
#******************************************************************************************
# A.) BINNENWANDERUNGEN -------------------------------------------------------------------
#******************************************************************************************
#******************************************************************************************

#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
###### -------- I.) LOR inklusive Außreisser LORs          --------------
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



LOR@data <- LORattrFULL

LOR4BWAND <- subset(LOR@data, select=c(RAUMID:STADTRAUM,
                                       E_E.2007,
                                       E_E.2012))

LOR_BWANDdf <- merge.with.order(LOR4BWAND, FORTZUEGEdf,
                                sort=F,
                                by.x="RAUMID", by.y="RAUMID",
                                all.x=T, all.y=T,
                                keep_order=1)

LOR_BWANDdf <- merge.with.order(LOR_BWANDdf, ZUZUEGEdf,
                                sort=F,
                                by.x="RAUMID", by.y="RAUMID",
                                all.x=T, all.y=T,
                                keep_order=1)

LOR_BWANDdf$FortzuegeRel <- round(((LOR_BWANDdf$Fortzuege/6)/LOR_BWANDdf$E_E.2012)*100, digits=1)
LOR_BWANDdf$ZuzuegeRel   <- round(((LOR_BWANDdf$Zuzuege  /6)/LOR_BWANDdf$E_E.2012)*100, digits=1)

LOR_BWANDdfsubset <- subset(LOR_BWANDdf, select=c(RAUMID,Fortzuege,Zuzuege,FortzuegeRel,ZuzuegeRel))

LOR@data <- merge.with.order(LOR@data, LOR_BWANDdfsubset,
                             sort=F,
                             by.x="RAUMID", by.y="RAUMID",
                             all.x=T, all.y=T,
                             keep_order=1)
names(LOR@data)

#### ---- Verteilungscheck und Außreisser Identifizieren ---- #####

#View(LOR_BWANDdf)

hist(LOR_BWANDdf$Fortzuege, breaks=50)
hist(LOR_BWANDdf$Zuzuege, breaks=50)

# Einwohner Verteilungen pro LOR 2007 & 2012
vioplot(LOR_BWANDdf$E_E.2007, 
        LOR_BWANDdf$E_E.2012, 
        names=c("2007", "2012"),
        col="gold")
title("Violinplots der LOR Einwohnerzahlen 2007 und 2012")
summary(LOR_BWANDdf$E_E.2007)
summary(LOR_BWANDdf$E_E.2012)

spplot(LOR, zcol="FortzuegeRel")

##### ------ Übersicht FortzügeRel & ZuzügeRel Vioplot ------

vioplot(LOR_BWANDdf$FortzuegeRel, 
        LOR_BWANDdf$ZuzuegeRel, 
        names=c("Fortzüge Relativ", "Zuzüge Relativ"),
        col="gold")
title("Violinplots der durchschn. rel. Fort- & Zuzüge 2007-2012")
summary(LOR_BWANDdf$FortzuegeRel)
summary(LOR_BWANDdf$ZuzuegeRel)


#### ----- FortzuegeRel VERTEILUNG --------
FZrel <- LOR@data$FortzuegeRel

h<-hist(FZrel, breaks=200, density=10, col="blue", xlab="Relative Binnen-Zuzüge") 
xfit<-seq(min(FZrel),max(FZrel),length=length(FZrel)) 
yfit<-dnorm(xfit,mean=median(FZrel),sd=mad(FZrel)) 
yfit <- yfit*diff(h$mids[1:2])*length(FZrel) 
lines(xfit, yfit, col="black", lwd=2)

qqplot(FZrel, rnorm(n=length(FZrel),
                    mean=median(FZrel),
                    sd=mad(FZrel)),
       ylab="~N Perzentil",
       xlab="Relative Binnen-Fortzüge")
qqline(FZrel,
       distribution = function(p) qnorm(p,mean=median(FZrel),sd=mad(FZrel)),
       prob = c(0.1, 0.6), col = 2, lwd=3)
mtext("qqline: Relative Fortzüge")

plot(FZrel,LOR@data$E_E.2012)
#identify(FZrel,LOR@data$E_E.2012)
# Ausreisser: 104 105 123 197 332 416

#### ----- ZuzuegeRel VERTEILUNG --------
ZZrel <- LOR@data$ZuzuegeRel

h<-hist(ZZrel, breaks=200, density=10, col="blue", xlab="Relative Binnen-Zuzüge") 
xfit<-seq(min(ZZrel),max(ZZrel),length=length(ZZrel)) 
yfit<-dnorm(xfit,mean=median(ZZrel),sd=mad(ZZrel)) 
yfit <- yfit*diff(h$mids[1:2])*length(ZZrel) 
lines(xfit, yfit, col="black", lwd=2)

qqplot(ZZrel, rnorm(n=length(ZZrel),
                    mean=median(ZZrel),
                    sd=mad(ZZrel)),
       ylab="~N Perzentil",
       xlab="Relative Binnen-Zuzüge")
qqline(ZZrel,
       distribution = function(p) qnorm(p,mean=median(ZZrel),sd=mad(ZZrel)),
       prob = c(0.1, 0.6), col = 2, lwd=3,
       ylab="~N Perzentil")
mtext("qqline: Relative Zuzüge")

plot(ZZrel,LOR@data$E_E.2012)
#identify(ZZrel,LOR@data$E_E.2012)
# Ausreisser: 104 123 197 332 416

plot(density(LOR_BWANDdf$ZuzuegeRel))
plot(density(LOR_BWANDdf$FortzuegeRel))

save(LOR_BWANDdf, file="/home/dao/Desktop/MasterArbeit/R_4_SUBLIME/LOR_BWANDdf.Rdata")
save(LOR, file="/home/dao/Desktop/MasterArbeit/R_4_SUBLIME/LOR.Rdata")

LOR@data$E_E_u300 <- as.factor(ifelse(LOR@data$E_E.2007 < 300, 
                                      c("unter 300EW"), 
                                      c("über  300EW")))
spplot(LOR, zcol="E_E_u300")
 
# LOR@data$E_E_u400 <- as.factor(ifelse(LOR@data$E_E.2012 < 400, 
#                                            c("unter 400EW"), 
#                                            c("über 400EW")))
# spplot(LOR, zcol="E_E_u400")
# 
# LOR@data$E_E_u500 <- as.factor(ifelse(LOR@data$E_E.2012 < 500, 
#                                    c("unter 500EW"), 
#                                    c("über 500EW")))
# spplot(LOR, zcol="E_E_u500")

#LOR@data <- subset(LOR@data, select=-c(E_E_u500.2012,
#                                       E_E_u400.2012))

LOR4reg <- LOR


# das hier sind die bösen Ausreisser
delvarsinfo <- c("RAUMID_NAME","BEZ_NAME","E_E.2007","Miete.2007",
                 "Fortzuege","Zuzuege","FortzuegeRel","ZuzuegeRel")
delcases <- c(104,105,123,197,332,416)
LOR4reg@data[delcases,][,delvarsinfo]

# das hier sind die auszuschließenden Fälle: Alle LORs mit <300EW 2012 und Motardstr.
LOR4reg@data[LOR4reg@data$E_E_u300=="unter 300EW" | LOR4reg@data$RAUMID_NAME=="Motardstr.",][,delvarsinfo]
# Wir überschreiben die Binnenwandeurngen mit NAs, damit sie später nicht bei der Regression mitverwendet werden
LOR4reg@data[LOR4reg@data$E_E_u300=="unter 300EW" | LOR4reg@data$RAUMID_NAME=="Motardstr.",][,c("FortzuegeRel","ZuzuegeRel")] <- NA
LOR4reg@data$valid <- as.factor(ifelse(is.na(LOR4reg@data$FortzuegeRel), 
                                            c("ungültig"), 
                                            c("gültig")))
spplot(LOR4reg, zcol="valid", col.regions=c("green","red"))

#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
###### -------- II.) LOR4reg exklusive Ausreisser LORs      --------------
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

spplot(LOR4reg, zcol="FortzuegeRel")
spplot(LOR4reg, zcol="ZuzuegeRel")

##### ------ Übersicht FortzügeRel & ZuzügeRel Vioplot ------

vioplot(na.omit(LOR4reg@data$FortzuegeRel), 
        na.omit(LOR4reg@data$ZuzuegeRel), 
        names=c("Fortzüge Relativ", "Zuzüge Relativ"),
        col="gold")
title("Violinplots der durchschn. rel. Fort- & Zuzüge 2007-2012")
summary(LOR4reg@data$FortzuegeRel)
summary(LOR4reg@data$ZuzuegeRel)


#### ----- FortzuegeRel VERTEILUNG --------
FZrel <- na.omit(LOR4reg@data$FortzuegeRel)

h<-hist(FZrel, breaks=50, density=10, col="blue", xlab="Relative Binnen-Zuzüge") 
xfit<-seq(min(FZrel),max(FZrel),length=length(FZrel)) 
yfit<-dnorm(xfit,mean=median(FZrel),sd=mad(FZrel)) 
yfit <- yfit*diff(h$mids[1:2])*length(FZrel) 
lines(xfit, yfit, col="black", lwd=2)

qqplot(FZrel, rnorm(n=length(FZrel),
                    mean=median(FZrel),
                    sd=mad(FZrel)),
       ylab="~N Perzentil",
       xlab="Relative Binnen-Fortzüge")
qqline(FZrel,
       distribution = function(p) qnorm(p,mean=median(FZrel),sd=mad(FZrel)),
       prob = c(0.1, 0.6), col = 2, lwd=3)
mtext("qqline: Relative Fortzüge")

plot(FZrel,LOR4reg@data$E_E.2012)
#identify(FZrel,LOR4reg@data$E_E.2012)
# Ausreisser: 104 105 123 197 332 416

#### ----- ZuzuegeRel VERTEILUNG --------
ZZrel <- na.omit(LOR4reg@data$ZuzuegeRel)

h<-hist(ZZrel, breaks=40, density=10, col="blue", xlab="Relative Binnen-Zuzüge") 
xfit<-seq(min(ZZrel),max(ZZrel),length=length(ZZrel)) 
yfit<-dnorm(xfit,mean=median(ZZrel),sd=mad(ZZrel)) 
yfit <- yfit*diff(h$mids[1:2])*length(ZZrel) 
lines(xfit, yfit, col="black", lwd=2)

qqplot(ZZrel, rnorm(n=length(ZZrel),
                    mean=median(ZZrel),
                    sd=mad(ZZrel)),
       ylab="~N Perzentil",
       xlab="Relative Binnen-Zuzüge")
qqline(ZZrel,
       distribution = function(p) qnorm(p,mean=median(ZZrel),sd=mad(ZZrel)),
       prob = c(0.1, 0.6), col = 2, lwd=3,
       ylab="~N Perzentil")
mtext("qqline: Relative Zuzüge")

#******************************************************************************************
#******************************************************************************************
# C.) Auswählen der LORs für Gentri-Kategorisierung  --------------------------------------
#******************************************************************************************
#******************************************************************************************

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

qntl <- quantile(LOR4reg@data$Mietechg, na.rm=T); qntl 
LOR4reg@data$MietechgQNTL <- cut(LOR4reg@data$Mietechg, 
    qntl,
    labels=c("1.Quartil",
             "2.Quartil",
             "3.Quartil",
             "4.Quartil"))
table(LOR4reg@data$MietechgQNTL)
spplot(LOR4reg, zcol="MietechgQNTL", 
       col.regions=c("darkblue","lightblue","orange","red"))


qntl <- quantile(LOR4reg@data$Mietechgr, na.rm=T); qntl 
LOR4reg@data$MietechgrQNTL <- cut(LOR4reg@data$Mietechgr, 
                             qntl,
                             labels=c("1.Quartil",
                                      "2.Quartil",
                                      "3.Quartil",
                                      "4.Quartil"))
table(LOR4reg@data$MietechgrQNTL)
spplot(LOR4reg, zcol="MietechgrQNTL", 
       col.regions=c("darkblue","lightblue","orange","red"))

qntl <- quantile(LOR4reg@data$Alosechg, na.rm=T); qntl 
LOR4reg@data$AlosechgQNTL <- cut(LOR4reg@data$Alosechg, 
                              qntl,
                              labels=c("1.Quartil",
                                       "2.Quartil",
                                       "3.Quartil",
                                       "4.Quartil"))
table(LOR4reg@data$AlosechgQNTL)
spplot(LOR4reg, zcol="AlosechgQNTL", 
       col.regions=c("darkblue","lightblue","orange","red"))

qntl <- quantile(LOR4reg@data$nicht_Alose_Hartzchg, na.rm=T); qntl 
LOR4reg@data$nicht_Alose_HartzchgQNTL <- cut(LOR4reg@data$nicht_Alose_Hartzchg, 
                                 qntl,
                                 labels=c("1.Quartil",
                                          "2.Quartil",
                                          "3.Quartil",
                                          "4.Quartil"))
table(LOR4reg@data$nicht_Alose_HartzchgQNTL)
spplot(LOR4reg, zcol="nicht_Alose_HartzchgQNTL", 
       col.regions=c("darkblue","lightblue","orange","red"))

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

