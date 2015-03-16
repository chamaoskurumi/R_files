
plot(density(ODdf$BinnenWand.Sum))
hist(log(ODdf$BinnenWand.Sum), breaks=100)
summary(ODdf$BinnenWand.Sum)
table(ODdf$BinnenWand.Sum)

ODdf
library(plyr)
FORTZUEGEdf <- ddply(ODdf, "VonLOR", summarise, 
                     Fortzuege = sum(BinnenWand.Sum))
plot(density(FORTZUEGEdf$Fortzuege))
hist(FORTZUEGEdf$Fortzuege, breaks=100)

ZUZUEGEdf <- ddply(ODdf, "NachLOR", summarise, 
                   Zuzuege = sum(BinnenWand.Sum))
plot(density(ZUZUEGEdf$Zuzuege))
hist(ZUZUEGEdf$Zuzuege, breaks=100)

install.packages("vcd")
library(vcd) ## loading vcd package
gf <- goodfit(ODdf$BinnenWand.Sum, type = "poisson", method = "MinChisq")
summary(gf)
hist(ODdf$BinnenWand.Sum[ODdf$BinnenWand.Sum<100], breaks=100)

var(ODdf$BinnenWand.Sum)

colnames(FORTZUEGEdf)[1] <- "RAUMID"
LORshape4FORTZUEGE <- LORshape
colnames(LORshape4FORTZUEGE@data)[1]  <- "RAUMID"
LORdf         <- as(LORshape4FORTZUEGE, "data.frame")
source("/home/dao/Desktop/MasterArbeit/R_files/functions/merge_with_order_FUNCTION.R")
FORTZUEGEattr       <- merge.with.order(
  LORdf, FORTZUEGEdf, sort=F,
  by.x="RAUMID", by.y="RAUMID",
  all.x=T, all.y=T,
  keep_order=1)
#View(LORattr)

LORshape4FORTZUEGE@data          <- FORTZUEGEattr
LORshape4FORTZUEGE@data$E_E.2013 <- LOR@data$E_E.2013
plot(LORshape4FORTZUEGE@data$Fortzuege,
     LORshape4FORTZUEGE@data$E_E.2013)
LORshape4FORTZUEGE@data$FortzuegeRel <- ((LORshape4FORTZUEGE@data$Fortzuege)/LORshape4FORTZUEGE@data$E_E.2013)
spplot(LORshape4FORTZUEGE, zcol="FortzuegeRel")



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
plot(LORshape4ZUZUEGE@data$Zuzuege,
     LORshape4ZUZUEGE@data$E_E.2013)
LORshape4ZUZUEGE@data$ZuzuegeRel <- (((LORshape4ZUZUEGE@data$Zuzuege/7)/LORshape4ZUZUEGE@data$E_E.2013)*100)
spplot(LORshape4ZUZUEGE, zcol="ZuzuegeRel")

spdep::spautolm()

hist(LORshape4ZUZUEGE$ZuzuegeRel*100, breaks=100)
hist(LORshape4FORTZUEGE$FortzuegeRel*100, breaks=100)


plot(density(LORshape4ZUZUEGE$ZuzuegeRel*100))
plot(density(LORshape4FORTZUEGE$FortzuegeRel*100))

x2 <- rnorm(447, 
            mean=mean(LORshape4FORTZUEGE$FortzuegeRel),
            sd=sd(LORshape4FORTZUEGE$FortzuegeRel))
ks.test(LORshape4FORTZUEGE$FortzuegeRel, x2)

plot(LORshape4ZUZUEGE@data$ZuzuegeRel*100,
     LOR@data$Miete.2013)

plot(LORshape4FORTZUEGE@data$FortzuegeRel*100,
     LOR@data$PDAU10chg)
