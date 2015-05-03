#*************************************************
#*************************************************
#*************************************************
#
# Analyse der Gentri-Kategorisierung
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

boxplot(E_E.2012~interaction(Gentri,STADTRAUM), data=LOR4reg@data, col=3:1)
table(LOR4reg@data$E_E.2012,LOR4reg@data$Gentri,LOR4reg@data$STADTRAUM)
table(LOR4reg@data$Gentri,LOR4reg@data$STADTRAUM)
addmargins(table(LOR4reg@data$E_E.2012,LOR4reg@data$Gentri,LOR4reg@data$STADTRAUM), FUN = sum, quiet = FALSE)


boxplot(Miete.2007 ~ Gentri, data=LOR4reg@data)
boxplot(Armut.2007 ~ Gentri, data=LOR4reg@data)
boxplot(nicht_Alose_Hartz.2007 ~ Gentri, data=LOR4reg@data)

boxplot(Armutchg ~ Gentri, data=LOR4reg@data)
boxplot(Mietechg ~ Gentri, data=LOR4reg@data)


boxplot(LOR4reg@data$FortzuegeR ~ LOR4reg@data$Gentri)
boxplot(LOR4reg@data$ZuzuegeR ~ LOR4reg@data$Gentri)

bpDF <- subset(LOR4reg@data, !is.na(Gentri))

GENTRIplot <- function(data,var)
{
  localenv <- environment()
  p  <- ggplot(data, aes_string(x="Gentri", 
                                y=var, 
                                weight="E_E.2012",                         
                                fill="Gentri"),
               environment = localenv) + 
    scale_y_continuous(breaks=pretty_breaks(n=10))+ 
    geom_violin(scale = "area") +
    theme(text = element_text(size=20),
          axis.text.x = element_text(face="bold"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position="none") 
  p + geom_boxplot(width=.3)
}

GENTRIplot(bpDF,"E_65U110Rchg")

p  <- ggplot(bpDF, aes(Gentri, 
                       E_65U110Rchg, 
                       weight=E_E.2012,
                       fill=Gentri)) + 
  scale_y_continuous(breaks=pretty_breaks(n=10))+ 
  geom_violin(scale = "area") +
  theme(text = element_text(size=20),
        axis.text.x = element_text(face="bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position="none") 
p + geom_boxplot(width=.3)

p  <- ggplot(bpDF, aes(Gentri, 
                       FortzuegeR, 
                       weight=E_E.2012,
                       fill=Gentri)) + 
  scale_y_continuous(breaks=pretty_breaks(n=10), limits=c(3,17)) + 
  geom_violin(scale = "width") +
  theme(text = element_text(size=20),
        axis.text.x = element_text(face="bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position="none") 
p + geom_boxplot(width=.1)


p  <- ggplot(bpDF, aes(Gentri, 
                       ZuzuegeUDAR, 
                       weight=E_E.2012,
                       fill=Gentri)) + 
  scale_y_continuous(breaks=pretty_breaks(n=10), limits=c(0,15)) + 
  geom_violin(scale = "width") +
  theme(text = element_text(size=20),
        axis.text.x = element_text(face="bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position="none") 
p + geom_boxplot(width=.1)

p  <- ggplot(bpDF, aes(Gentri, 
                       nicht_Alose_Hartz.2007, 
                       weight=E_E.2012,
                       fill=Gentri)) + 
  scale_y_continuous(breaks=pretty_breaks(n=10)) + 
  geom_violin(scale = "area") +
  theme(text = element_text(size=20),
        axis.text.x = element_text(face="bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position="none") 
p + geom_boxplot(width=.05)


p  <- ggplot(bpDF, aes(Gentri, 
                       Alose_u25.2007, 
                       weight=E_E.2012,
                       fill=Gentri)) + 
  scale_y_continuous(breaks=pretty_breaks(n=10)) + 
  geom_violin(scale = "area") +
  theme(text = element_text(size=20),
        axis.text.x = element_text(face="bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position="none") 
p + geom_boxplot(width=.05)

GentriDEFarmut0.1_0.9 <- wtd.quantile(x=bpDF$Armut.2007[bpDF$Gentri=="Gentri"], 
                                      weights=bpDF$E_E.2012[bpDF$Gentri=="Gentri"],             
                                      probs=c(0.1,0.9))
p  <- ggplot(bpDF, aes(Gentri, 
                       Armut.2007, 
                       weight=E_E.2012,
                       fill=Gentri)) + 
  scale_y_continuous(breaks=pretty_breaks(n=10)) + 
  geom_violin(scale = "area") +
  theme(text = element_text(size=20),
        axis.text.x = element_text(face="bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position="none") 
p <- p + geom_boxplot(width=.05)
p <- p + geom_segment(mapping=aes(x="Gentri", y=GentriDEFarmut0.1_0.9[1], 
                                  xend="Kontroll", yend=GentriDEFarmut0.1_0.9[1]), 
                      linetype="dashed", color="red") +
  geom_segment(mapping=aes(x="Gentri", y=GentriDEFarmut0.1_0.9[2], 
                           xend="Kontroll", yend=GentriDEFarmut0.1_0.9[2]), 
               linetype="dashed", color="red") 
p

GentriDEFmiete0.1_0.9 <- wtd.quantile(x=bpDF$Miete.2007[bpDF$Gentri=="Gentri"], 
                                      weights=bpDF$E_E.2012[bpDF$Gentri=="Gentri"],             
                                      probs=c(0.1,0.9))
p  <- ggplot(bpDF, aes(Gentri, 
                       Miete.2007, 
                       weight=E_E.2012,
                       fill=Gentri)) + 
  scale_y_continuous(breaks=pretty_breaks(n=10)) + 
  geom_violin(scale = "area") +
  theme(text = element_text(size=20),
        axis.text.x = element_text(face="bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position="none") 
p <- p + geom_boxplot(width=.05) 
p <- p + geom_segment(mapping=aes(x="Gentri", y=GentriDEFmiete0.1_0.9[1], 
                                  xend="Kontroll", yend=GentriDEFmiete0.1_0.9[1]), 
                      linetype="dashed", color="red") +
  geom_segment(mapping=aes(x="Gentri", y=GentriDEFmiete0.1_0.9[2], 
                           xend="Kontroll", yend=GentriDEFmiete0.1_0.9[2]), 
               linetype="dashed", color="red") 
p

p  <- ggplot(bpDF, aes(Gentri, 
                       ZuzuegeAR, 
                       weight=E_E.2012,
                       fill=Gentri)) + 
  scale_y_continuous(breaks=pretty_breaks(n=10), limits=c(0,7)) + 
  geom_violin(scale = "width") +
  theme(text = element_text(size=20),
        axis.text.x = element_text(face="bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position="none") 
p + geom_boxplot(width=.1)

p  <- ggplot(bpDF, aes(Gentri, 
                       FortzuegeUR, 
                       weight=E_E.2012,
                       fill=Gentri)) + 
  scale_y_continuous(breaks=pretty_breaks(n=10), limits=c(0,2.5)) + 
  geom_violin(scale = "width") +
  theme(text = element_text(size=20),
        axis.text.x = element_text(face="bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position="none") 
p + geom_boxplot(width=.1)


p  <- ggplot(bpDF, aes(Gentri, 
                       ZuzuegeDR, 
                       weight=E_E.2012,
                       fill=Gentri)) + 
  scale_y_continuous(breaks=pretty_breaks(n=10)) + 
  geom_violin(scale = "width") +
  theme(text = element_text(size=20),
        axis.text.x = element_text(face="bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position="none") 
p + geom_boxplot(width=.1)

by(LOR4reg@data$Miete.2007, LOR4reg@data$Gentri, summary)
by(LOR4reg@data$Armut.2007, LOR4reg@data$Gentri, summary)
by(LOR4reg@data$nicht_Alose_Hartz.2007, LOR4reg@data$Gentri, summary)
by(LOR4reg@data$E_65U110Rchg, LOR4reg@data$Gentri, summary)

### --- Versuch auf extreme Outliers extern per Arrow zu verweisen ----
p  <- ggplot(bpDF, aes(Gentri, 
                       FortzuegeR, 
                       weight=E_E.2012,
                       fill=Gentri)) + 
  #  scale_y_continuous(breaks=pretty_breaks(n=10))+ 
  geom_violin(scale = "area") 
theme(text = element_text(size=20),
      axis.text.x = element_text(face="bold"),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.position="none") 
p1 <- p + geom_boxplot(width=.1)

maxval <- 17
dd <- bpDF %>% filter(FortzuegeR>maxval) %>%  group_by(Gentri) %>%  summarise(outlier_txt=paste(FortzuegeR,collapse=","))
p2 <- p1 + scale_y_continuous(limits=c(3,maxval)) +
  geom_text(data=dd,aes(y=maxval,label=outlier_txt),size=3,vjust=1.5,hjust=-0.5)
geom_segment(data=dd,aes(y=maxval*0.95,yend=maxval,
                         xend=factor(Gentri)),
             arrow = arrow(length = unit(0.1,"cm")))
p2



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# III.) Visualisierung der Kategorisierung GENTRI ------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

#View(LOR4reg@data[,c("RAUMID_NAME","BEZ_NAME","Alosechg","Mietechgr","E_E.2012")])
#View(LOR4reg@data[,c("RAUMID_NAME","BEZ_NAME","Alose.2007","Mietechgr","E_E.2007")])

p <- ggplot(LOR4reg@data, aes(Alose.2007, Miete.2007, weight=E_E.2007)) + 
  geom_point(aes(size = E_E.2007, colour = Gentri)) + 
  scale_size_area(breaks=c(300, 1000, 5000, 10000, 30000), "Einwohner 2007", max_size=10) +
  scale_x_continuous("Arbeitslosenquote 2007") +
  scale_y_continuous("Mietpreises 2007")
p

p <- ggplot(LOR4reg@data, aes(nicht_Alose_Hartz.2007, Alose_u25.2007, weight=E_E.2012)) + 
  geom_point(aes(size = E_E.2012, colour = Gentri)) + 
  scale_size_area(breaks=c(300, 1000, 5000, 10000, 30000), "Einwohner 2012", max_size=10) +
  scale_x_continuous("nicht_Alose_Hartz.2007") +
  scale_y_continuous("Alose_u25.2007")
p

##### sollte noch gemacht werden #####
# Alterarmut.2012
# AlleinerzHH.2012
# PDAU5chg
# PDAU10chg

library(car)
scatterplot(FortzuegeR ~ Mietechgr | Gentri, data=bpDF,
            xlab="Miete % Änderung", ylab="Rel. Fortzüge in %",
            main="Enhanced Scatter Plot",
            labels=row.names(bpDF)) 


xyplot(FortzuegeR ~ Mietechgr | Gentri, bpDF, groups = bpDF$Gentri, pch= 20,
       type = c("p","r"))
summary(bpDF$FortzuegeR)


boxplot(FortzuegeR~Gentri*ZEIT,data=LORdataFULLvalid, ylim=c(1,20))

LORdataFULLvalidINNERESTADT <- subset(LORdataFULLvalid, STADTRAUM=="innere Stadt")

boxplot(FortzuegeR~Gentri*ZEIT,data=LORdataFULLvalidINNERESTADT, ylim=c(3,18), col=c("blue","red","green"))
boxplot(FortzuegeR~ZEIT*Gentri,data=LORdataFULLvalidINNERESTADT, 
        ylim=c(3,18), 
        col= c(rep(c("blue"),times=6),
               rep(c("red"),times=6),
               rep(c("green"),times=6)))         


boxplot(FortzuegeR~MietechgrQNTL,data=LORdataFULLvalid, ylim=c(0,40), col=c("blue","yellow","orange","red"))

boxplot(Armutchg.2012~MietechgrQNTL,data=LORdataFULLvalid, ylim=c(0,40), col=c("blue","yellow","orange","red"))

boxplot(Armutchg~MietechgrQNTL,data=LOR4reg@data, ylim=c(-20,10), col=c("blue","yellow","orange","red"))
boxplot(FortzuegeR~,data=LORdataFULLvalid, ylim=c(0,40), col=c("blue","yellow","orange","red"))
boxplot(FortzuegeR~MietechgrQNTL,data=LORdataFULLvalid, 
        ylim=c(3,18), 
        col= c(rep(c("blue"),times=6),
               rep(c("red"),times=6),
               rep(c("green"),times=6)))         


boxplot(ZuzuegeDAR~Gentri*ZEIT,data=LORdataFULLvalidINNERESTADT, ylim=c(1,14), col=c("blue","red","green"))
boxplot(ZuzuegeDAR~ZEIT*Gentri,data=LORdataFULLvalidINNERESTADT, 
        ylim=c(1,15), 
        col= c(rep(c("blue"),times=6),
               rep(c("red"),times=6),
               rep(c("green"),times=6)))    



boxplot(ZuzuegeR~Gentri*ZEIT,data=LORdataFULLvalidINNERESTADT, ylim=c(5,16), col=c("blue","red","green"))
boxplot(ZuzuegeR~ZEIT*Gentri,data=LORdataFULLvalidINNERESTADT, 
        ylim=c(5,16), 
        col= c(rep(c("blue"),times=6),
               rep(c("red"),times=6),
               rep(c("green"),times=6)))    
