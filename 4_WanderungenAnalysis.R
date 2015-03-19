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

spplot(LORshape4FORTZUEGE, zcol="FortzuegeRel")

install.packages("vcd")
library(vcd) ## loading vcd package
gf <- goodfit(ODdf$BinnenWand.Sum, type = "poisson", method = "MinChisq")
summary(gf)
hist(ODdf$BinnenWand.Sum[ODdf$BinnenWand.Sum<100], breaks=100)

var(ODdf$BinnenWand.Sum)

LORshape4FORTZUEGE <- LORshape
colnames(LORshape4FORTZUEGE@data)[1]  <- "RAUMID"
LORdf         <- as(LORshape4FORTZUEGE, "data.frame")
source("/home/dao/Desktop/MasterArbeit/R_files/functions/merge_with_order_FUNCTION.R")

LORshape4FORTZUEGE@data          <- FORTZUEGEattr
LORshape4FORTZUEGE@data$E_E.2012 <- LOR@data$E_E.2012
plot(LORshape4FORTZUEGE@data$Fortzuege,
     LORshape4FORTZUEGE@data$E_E.2013)
LORshape4FORTZUEGE@data$FortzuegeRel <- ((LORshape4FORTZUEGE@data$Fortzuege)/LORshape4FORTZUEGE@data$E_E.2012)
spplot(LORshape4FORTZUEGE, zcol="FortzuegeRel")

hist(LORshape4FORTZUEGE@data$FortzuegeRel, breaks=200)
FZrel <- LORshape4FORTZUEGE@data$FortzuegeRel

h<-hist(FZrel, breaks=200, density=10, col="blue", xlab="Accuracy", main="Overall") 
xfit<-seq(min(FZrel),max(FZrel),length=length(FZrel)) 
yfit<-dnorm(xfit,mean=median(FZrel),sd=mad(FZrel)) 
yfit <- yfit*diff(h$mids[1:2])*length(FZrel) 
lines(xfit, yfit, col="black", lwd=2)

qqplot(FZrel, rnorm(n=length(FZrel),
                    mean=median(FZrel),
                    sd=mad(FZrel)))
qqline(FZrel,
       distribution = function(p) qnorm(p,mean=median(FZrel),sd=mad(FZrel)),
       prob = c(0.1, 0.6), col = 2)
mtext("qqline Relative Fortzüege")

colnames(ZUZUEGEdf)[1] <- "RAUMID"
LORshape4ZUZUEGE <- LORshape
colnames(LORshape4ZUZUEGE@data)[1]  <- "RAUMID"
LORdf         <- as(LORshape4ZUZUEGE, "data.frame")
source("/home/dao/Desktop/MasterArbeit/R_files/functions/merge_with_order_FUNCTION.R")
ZUZUEGEattr       <- merge.with.order(
  LORdf, ZUZUEGEdf, sort=F,
  by.x="RAUMID", by.y="RAUMID",
  all.x=T, all.y=T,
  keep_order=1)


LORshape4ZUZUEGE@data          <- ZUZUEGEattr
LORshape4ZUZUEGE@data$E_E.2013 <- LOR@data$E_E.2013
plot(LORshape4ZUZUEGE@data$Zuzuege,LORshape4ZUZUEGE@data$E_E.2013)
LORshape4ZUZUEGE@data$ZuzuegeRel <- (((LORshape4ZUZUEGE@data$Zuzuege/7)/LORshape4ZUZUEGE@data$E_E.2013)*100)
spplot(LORshape4ZUZUEGE, zcol="ZuzuegeRel")

hist(LORshape4ZUZUEGE$ZuzuegeRel*100, breaks=100)
hist(LORshape4FORTZUEGE$FortzuegeRel*100, breaks=100)

plot(density(LORshape4ZUZUEGE$ZuzuegeRel*100))
plot(density(LORshape4FORTZUEGE$FortzuegeRel*100))

plot(LORshape4ZUZUEGE@data$ZuzuegeRel*100,
     LOR@data$Miete.2013)

plot(LORshape4FORTZUEGE@data$FortzuegeRel*100,
     LOR@data$PDAU10chg)

save(LOR_BWANDdf, file="/home/dao/Desktop/MasterArbeit/R_4_SUBLIME/LOR_BWANDdf.Rdata")
save(LOR, file="/home/dao/Desktop/MasterArbeit/R_4_SUBLIME/LOR.Rdata")

