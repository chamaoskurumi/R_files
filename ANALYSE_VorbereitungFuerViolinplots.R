#setwd("/home/dao/Desktop/MasterArbeit/R_files/KNITR/")
#load(file = "FINAL_WORKSPACE.Rdata")

#### packages ####
library("ggplot2")
library("rgdal")
library("sp")
library("plyr")
library("ggplot2")
library("scales")
library("ENmisc")
library("gridExtra")
library("Hmisc")
library("dplyr")
library("car")
library("vcd")
library("plyr")

bpDF <- subset(LOR4reg@data, !is.na(Gentri))
bpDF$Gentri  <- factor(bpDF$Gentri,levels(bpDF$Gentri)[c(2,3,1)])
bpDF$GentriA <- factor(bpDF$GentriA,levels(bpDF$GentriA)[c(2,3,1)])

levels(bpDF$valid)[levels(bpDF$valid)=="gültig"] <- "Gesamt"

# Gentri: GENTRI Quantilsgrenzen
qntl_Armutchg  <- wtd.quantile(LOR4reg@data$Armutchg, weights=LOR4reg@data$E_E.2007, 
                               probs=c(0, .25, .5, .75, 1))
qntl_Mietechgr <- wtd.quantile(LOR4reg@data$Mietechgr, weights=LOR4reg@data$E_E.2007, 
                               probs=c(0, .25, .5, .75, 1))
# GentriA: GENTRI Quantilsgrenzen
qntlA_Armutchg  <- wtd.quantile(LOR4reg@data$Armutchg, weights=LOR4reg@data$E_E.2007, 
                                probs=c(0, .15, .5, .85, 1))
qntlA_Mietechgr <- wtd.quantile(LOR4reg@data$Mietechgr, weights=LOR4reg@data$E_E.2007, 
                                probs=c(0, .15, .5, .85, 1))

bp_theme <- theme(#text = element_text(colour = "grey20", size =20, face = "bold"),
  #axis.text.x = element_text(face="bold", size=8),
  axis.title.x = element_blank(),
  axis.title.y = element_text(colour = "grey50"),#, size=10),#, size =20, face = "bold", angle = 90),
  plot.margin=unit(c(0.5,-0.3,0.5,0.5), "cm"),
  legend.position="none") 

bp_themeMITRAND <- theme(#text = element_text(colour = "grey20", size =20, face = "bold"),
  #axis.text.x = element_text(face="bold", size=8),
  axis.title.x = element_blank(),
  axis.title.y = element_text(colour = "grey50"),#, size=10),#, face = "bold", angle = 90),
  #plot.margin=unit(c(0.5,-0.3,0.5,0.5), "cm"),
  legend.position="none") 

bp_themeG <- theme(#text= element_text(size=20),
  #axis.text.x = element_text(face="bold", size=8),
  axis.text.y = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.ticks = element_blank(),
  plot.margin=unit(c(0.5,0.5,0.5,-0.3), "cm"),
  legend.position="none") 

bp_themeGMITRAND <- theme(#text= element_text(size=20),
  #axis.text.x = element_text(face="bold", size=8),
  axis.text.y = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.ticks = element_blank(),
  #plot.margin=unit(c(0.5,0.5,0.5,-0.3), "cm"),
  legend.position="none") 

brueche <- 8

##### Mietechgr ####

p1Mietechgr  <- ggplot(bpDF, aes(Gentri, 
                                 Mietechgr, 
                                 weight=E_E.2007,
                                 fill=Gentri)) + 
  ylab(expression(paste(Delta[r],'Miete (%)')))+
  scale_y_continuous(breaks=pretty_breaks(n=brueche))+ 
  geom_violin(scale = "width") + bp_theme
p1Mietechgr  <- p1Mietechgr  + geom_boxplot(width=.3)
p1Mietechgr  <- p1Mietechgr  + geom_hline(aes(yintercept=qntl_Mietechgr[4]),
                                          linetype="dotted", color="red", size=0.5)

p2Mietechgr   <- ggplot(bpDF, aes(valid,
                                  Mietechgr, 
                                  weight=E_E.2007)) +
  scale_y_continuous(breaks=pretty_breaks(n=brueche)) +
  geom_violin(scale="width") +  bp_themeG
p2Mietechgr  <- p2Mietechgr  + geom_boxplot(width=.3)
p2Mietechgr  <- p2Mietechgr  + geom_hline(aes(yintercept=qntl_Mietechgr[4]),
                                          linetype="dotted", color="red", size=0.5)
p2Mietechgr <- p2Mietechgr + annotate("text", x=0.7, y=4.73, label="Q[0.75]", parse=TRUE, col="red", size=3)
#p2Mietechgr

#grid.arrange(p1Mietechgr,p2Mietechgr, ncol=2, nrow=1, widths=c(3,1))

##### Armutchg ####

p1Armutchg   <- ggplot(bpDF, aes(Gentri, 
                                 Armutchg, 
                                 weight=E_E.2007,
                                 fill=Gentri)) + 
  ylab(expression(paste(Delta,'Armut (Prozentpunkte)')))+
  scale_y_continuous(breaks=pretty_breaks(n=brueche))+ 
  geom_violin(scale = "width") + bp_theme
p1Armutchg <- p1Armutchg + geom_boxplot(width=.3)
p1Armutchg <- p1Armutchg + geom_hline(aes(yintercept=qntl_Armutchg[2]),
                                      linetype="dotted", color="red", size=0.5)

p2Armutchg  <- ggplot(bpDF, aes(valid,
                                Armutchg, 
                                weight=E_E.2007)) +
  scale_y_continuous(breaks=pretty_breaks(n=brueche)) +
  geom_violin(scale="width") +  bp_themeG
p2Armutchg <- p2Armutchg + geom_boxplot(width=.3)
p2Armutchg <- p2Armutchg + geom_hline(aes(yintercept=qntl_Armutchg[2]),
                                      linetype="dotted", color="red", size=0.5)
p2Armutchg <- p2Armutchg + annotate("text", x=1.35, y=-4.2, label="Q[0.25]", parse=TRUE, col="red", size=3)
#p2Armutchg

#grid.arrange(p1Armutchg, p2Armutchg, ncol=2, nrow=1, widths=c(3,1))

##### Miete 2007 ####

p1Miete.2007  <- ggplot(bpDF, aes(Gentri, 
                                  Miete.2007, 
                                  weight=E_E.2007,
                                  fill=Gentri)) + 
  #ylab("Miete 2007")+
  ylab(expression(paste(Miete[2007],' ','(Euro/',m^{2},')')))+
  scale_y_continuous(breaks=pretty_breaks(n=brueche))+ 
  geom_violin(scale = "width") + bp_theme
p1Miete.2007 <- p1Miete.2007 + geom_boxplot(width=.3)
p1Miete.2007 <- p1Miete.2007 + geom_segment(mapping=aes(x="Gentri", y=Gentri_Miete.2007_0.10, 
                                                        xend="Kontroll", yend=Gentri_Miete.2007_0.10), 
                                            linetype="dotted", color="red", size=0.3) +
  geom_segment(mapping=aes(x="Gentri", y=Gentri_Miete.2007_0.90, 
                           xend="Kontroll", yend=Gentri_Miete.2007_0.90), 
               linetype="dotted", color="red", size=0.3) 
p1Miete.2007 <- p1Miete.2007 + 
  annotate("text", x=1.5, y=7.7, label="Q[0.9]^Gentri", parse=TRUE, col="red", size=3) +
  annotate("text", x=1.5, y=4.6, label="Q[0.1]^Gentri", parse=TRUE, col="red", size=3)
#p1Miete.2007

p2Miete.2007  <- ggplot(bpDF, aes(valid,
                                  Miete.2007, 
                                  weight=E_E.2007)) +
  scale_y_continuous(breaks=pretty_breaks(n=brueche)) +
  geom_violin(scale="width") +  bp_themeG
p2Miete.2007 <- p2Miete.2007 + geom_boxplot(width=.3)

#grid.arrange(p1Miete.2007, p2Miete.2007, ncol=2, nrow=1, widths=c(3,1))

##### Armut 2007 ####

p1Armut.2007  <- ggplot(bpDF, aes(Gentri, 
                                  Armut.2007, 
                                  weight=E_E.2007,
                                  fill=Gentri)) + 
  ylab(expression(paste(Armut[2007],' ','(%)')))+
  scale_y_continuous(breaks=pretty_breaks(n=5))+ 
  geom_violin(scale = "width") + bp_theme
p1Armut.2007  <- p1Armut.2007 + geom_boxplot(width=.3)
p1Armut.2007  <- p1Armut.2007 + geom_segment(mapping=aes(x="Gentri", y=Gentri_Armut.2007_0.10, 
                                                         xend="Kontroll", yend=Gentri_Armut.2007_0.10), 
                                             linetype="dotted", color="red", size=0.3) +
  geom_segment(mapping=aes(x="Gentri", y=Gentri_Armut.2007_0.90, 
                           xend="Kontroll", yend=Gentri_Armut.2007_0.90), 
               linetype="dotted", color="red", size=0.3) 
p1Armut.2007 <- p1Armut.2007 + 
  annotate("text", x=1.5, y=49, label="Q[0.9]^Gentri", parse=TRUE, col="red", size=3) +
  annotate("text", x=1.5, y=13, label="Q[0.1]^Gentri", parse=TRUE, col="red", size=3)
# p1Armut.2007

p2Armut.2007 <- ggplot(bpDF, aes(valid,
                                 Armut.2007, 
                                 weight=E_E.2007)) +
  scale_y_continuous(breaks=pretty_breaks(n=brueche)) +
  geom_violin(scale="width") +  bp_themeG
p2Armut.2007  <- p2Armut.2007 + geom_boxplot(width=.3)

#grid.arrange(p1Armut.2007, p2Armut.2007, ncol=2, nrow=1, widths=c(3,1))

##### Miete 2012 ####

p1Miete.2012  <- ggplot(bpDF, aes(Gentri, 
                                  Miete.2012, 
                                  weight=E_E.2007,
                                  fill=Gentri)) + 
  #ylab("Miete 2007")+
  ylab(expression(paste(Miete[2012],' ','(Euro/',m^{2},')')))+
  scale_y_continuous(breaks=pretty_breaks(n=brueche))+ 
  geom_violin(scale = "width") + bp_theme
p1Miete.2012<- p1Miete.2012 + geom_boxplot(width=.3)
#p1Miete.2012

p2Miete.2012 <- ggplot(bpDF, aes(valid,
                                 Miete.2012, 
                                 weight=E_E.2007)) +
  scale_y_continuous(breaks=pretty_breaks(n=brueche)) +
  geom_violin(scale="width") +  bp_themeG
p2Miete.2012 <- p2Miete.2012 + geom_boxplot(width=.3)

#grid.arrange(p1Miete.2012, p2Miete.2012, ncol=2, nrow=1, widths=c(3,1))

##### Armut 2012 ####

p1Armut.2012  <- ggplot(bpDF, aes(Gentri, 
                                  Armut.2012, 
                                  weight=E_E.2007,
                                  fill=Gentri)) + 
  #ylab("Miete 2007")+
  ylab(expression(paste(Armut[2012],' ','(%)')))+
  scale_y_continuous(breaks=pretty_breaks(n=brueche))+ 
  geom_violin(scale = "width") + bp_theme
p1Armut.2012 <- p1Armut.2012 + geom_boxplot(width=.3)

p2Armut.2012  <- ggplot(bpDF, aes(valid,
                                  Armut.2012, 
                                  weight=E_E.2007)) +
  scale_y_continuous(breaks=pretty_breaks(n=brueche)) +
  geom_violin(scale="width") +  bp_themeG
p2Armut.2012 <- p2Armut.2012 + geom_boxplot(width=.3)