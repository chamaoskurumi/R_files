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

# BINNENWANDERUNGEN -------------------------------------------------------------------

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

LOR_BWANDdf$FortzuegeRel <- (LOR_BWANDdf$Fortzuege/LOR_BWANDdf$E_E.2012)*100
LOR_BWANDdf$ZuzuegeRel   <- (LOR_BWANDdf$Zuzuege  /LOR_BWANDdf$E_E.2012)*100

LOR_BWANDdfsubset <- subset(LOR_BWANDdf, select=c(RAUMID,Fortzuege,Zuzuege,FortzuegeRel,ZuzuegeRel))

LOR@data <- merge.with.order(LOR@data, LOR_BWANDdfsubset,
                             sort=F,
                             by.x="RAUMID", by.y="RAUMID",
                             all.x=T, all.y=T,
                             keep_order=1)
names(LOR@data)

#### ---- Verteilungscheck und Außreisser Identifizieren ---- #####

View(LOR_BWANDdf)

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
identify(FZrel,LOR@data$E_E.2012)
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
identify(ZZrel,LOR@data$E_E.2012)
# Ausreisser: 104 123 197 332 416

plot(density(LORshape4ZUZUEGE$ZuzuegeRel*100))
plot(density(LORshape4FORTZUEGE$FortzuegeRel*100))

save(LOR_BWANDdf, file="/home/dao/Desktop/MasterArbeit/R_4_SUBLIME/LOR_BWANDdf.Rdata")
save(LOR, file="/home/dao/Desktop/MasterArbeit/R_4_SUBLIME/LOR.Rdata")

LOR@data$E_E_u400 <- as.factor(ifelse(LOR@data$E_E.2012 < 400, 
                                           c("unter 400EW"), 
                                           c("über 400EW")))
spplot(LOR, zcol="E_E_u400")

LOR@data$E_E_u500 <- as.factor(ifelse(LOR@data$E_E.2012 < 500, 
                                   c("unter 500EW"), 
                                   c("über 500EW")))
spplot(LOR, zcol="E_E_u500")

LOR@data <- subset(LOR@data, select=-c(E_E_u500.2012,
                                       E_E_u400.2012))

LOR4reg <- LOR

delvars <- c("Fortzuege","Zuzuege","FortzuegeRel","ZuzuegeRel")
LOR4reg@data[LOR4reg@data$E_E_u500=="unter 500EW",][,delvars]

delvarsinfo <- c("RAUMID_NAME","BEZ_NAME","E_E.2007","Miete.2007",
                 "Fortzuege","Zuzuege","FortzuegeRel","ZuzuegeRel")
delcases <- c(104,105,123,197,332,416)
LOR4reg@data[delcases,][,delvarsinfo]
