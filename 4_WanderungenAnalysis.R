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
spplot(LOR4reg, zcol="valid", col.regions=c("red","green"))

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

vioplot(na.omit(LOR@data$Mietechg))

LOR@data$MietechgCAT <- as.factor(ifelse(LOR@data$E_E.2007 < 300, 
                                        c("unter 300EW"), 
                                        c("über  300EW")))


qntl <- quantile(LOR@data$Mietechg, na.rm=T); qntl 
LOR@data$MietechgQNTL <- cut(LOR@data$Mietechg, 
    qntl,
    labels=c("1.Quartil",
             "2.Quartil",
             "3.Quartil",
             "4.Quartil"))
table(LOR@data$MietechgQNTL)
spplot(LOR, zcol="MietechgQNTL", 
       col.regions=c("darkblue","lightblue","orange","red"))


qntl <- quantile(LOR@data$Mietechgr, na.rm=T); qntl 
LOR@data$MietechgrQNTL <- cut(LOR@data$Mietechgr, 
                             qntl,
                             labels=c("1.Quartil",
                                      "2.Quartil",
                                      "3.Quartil",
                                      "4.Quartil"))
table(LOR@data$MietechgrQNTL)
spplot(LOR, zcol="MietechgrQNTL", 
       col.regions=c("darkblue","lightblue","orange","red"))

qntl <- quantile(LOR@data$Alosechg, na.rm=T); qntl 
LOR@data$AlosechgQNTL <- cut(LOR@data$Alosechg, 
                              qntl,
                              labels=c("1.Quartil",
                                       "2.Quartil",
                                       "3.Quartil",
                                       "4.Quartil"))
table(LOR@data$AlosechgQNTL)
spplot(LOR, zcol="AlosechgQNTL", 
       col.regions=c("darkblue","lightblue","orange","red"))

LOR@data$Gentri[LOR@data$AlosechgQNTL=="4.Quartil" & LOR@data$MietechgrQNTL=="4.Quartil"] <- "Gentri hi"
LOR@data$Gentri[LOR@data$AlosechgQNTL=="3.Quartil" & LOR@data$MietechgrQNTL=="4.Quartil" | 
                LOR@data$AlosechgQNTL=="4.Quartil" & LOR@data$MietechgrQNTL=="3.Quartil" | 
                LOR@data$AlosechgQNTL=="3.Quartil" & LOR@data$MietechgrQNTL=="3.Quartil" ] <- "Gentri low"
LOR@data$Gentri[is.na(LOR@data$Gentri) & !is.na(LOR@data$MietechgrQNTL)] <- "Non Gentri"
LOR@data$Gentri <- as.factor(LOR@data$Gentri)
table(LOR@data$Gentri)
spplot(LOR, zcol="Gentri", 
       col.regions=c("red","yellow","darkblue"))


