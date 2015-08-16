setwd("/home/dao/Desktop/MasterArbeit/R_files/KNITR/")
load(file = "FULL_FINAL_WORKSPACE.Rdata")

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

bpGentriInnere   <- subset(bpDF, Gentri=="Gentri" & STADTRAUM=="innere Stadt")
bpGesamtInnere   <- subset(bpDF, STADTRAUM=="innere Stadt")

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

bp_theme <- theme(axis.title.x = element_blank(),
                  plot.margin=unit(c(0.5,-0.3,0.5,0.5), "cm"),
                  legend.position="none") 

bp_themeMITRAND <- theme(axis.title.x = element_blank(),
                         legend.position="none") 

bp_themeG <- theme(axis.text.y = element_blank(),
                   axis.title.x = element_blank(),
                   axis.title.y = element_blank(),
                   axis.ticks = element_blank(),
                   plot.margin=unit(c(0.5,0.5,0.5,-0.3), "cm"),
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
                          linetype="dotted", color="red", size=1)

p2Mietechgr   <- ggplot(bpDF, aes(valid,
                       Mietechgr, 
                       weight=E_E.2007)) +
  scale_y_continuous(breaks=pretty_breaks(n=brueche)) +
  geom_violin(scale="width") +  bp_themeG
p2Mietechgr  <- p2Mietechgr  + geom_boxplot(width=.3)
p2Mietechgr  <- p2Mietechgr  + geom_hline(aes(yintercept=qntl_Mietechgr[4]),
                                          linetype="dotted", color="red", size=1)
p2Mietechgr <- p2Mietechgr + annotate("text", x=0.6, y=4.55, label="Q[0.75]", parse=TRUE, col="red")
#p2Mietechgr

grid.arrange(p1Mietechgr,p2Mietechgr, ncol=2, nrow=1, widths=c(3,1))

##### Armutchg ####

p1Armutchg   <- ggplot(bpDF, aes(Gentri, 
                        Armutchg, 
                        weight=E_E.2007,
                        fill=Gentri)) + 
  ylab(expression(paste(Delta,'Armut (%)')))+
  scale_y_continuous(breaks=pretty_breaks(n=brueche))+ 
  geom_violin(scale = "width") + bp_theme
p1Armutchg <- p1Armutchg + geom_boxplot(width=.3)
p1Armutchg <- p1Armutchg + geom_hline(aes(yintercept=qntl_Armutchg[2]),
                      linetype="dotted", color="red", size=1)

p2Armutchg  <- ggplot(bpDF, aes(valid,
                        Armutchg, 
                        weight=E_E.2007)) +
  scale_y_continuous(breaks=pretty_breaks(n=brueche)) +
  geom_violin(scale="width") +  bp_themeG
p2Armutchg <- p2Armutchg + geom_boxplot(width=.3)
p2Armutchg <- p2Armutchg + geom_hline(aes(yintercept=qntl_Armutchg[2]),
                      linetype="dotted", color="red", size=1)
p2Armutchg <- p2Armutchg + annotate("text", x=1.4, y=-3.6, label="Q[0.25]", parse=TRUE, col="red")
#p2Armutchg

#grid.arrange(p1Armutchg, p2Armutchg, ncol=2, nrow=1, widths=c(3,1))

##### Miete 2007 ####

p1Miete.2007  <- ggplot(bpDF, aes(Gentri, 
                        Miete.2007, 
                        weight=E_E.2007,
                        fill=Gentri)) + 
  #ylab("Miete 2007")+
  ylab(expression(paste(Miete[2007],' ','(€)')))+
  scale_y_continuous(breaks=pretty_breaks(n=brueche))+ 
  geom_violin(scale = "width") + bp_theme
p1Miete.2007 <- p1Miete.2007 + geom_boxplot(width=.3)
p1Miete.2007 <- p1Miete.2007 + geom_segment(mapping=aes(x="Gentri", y=Gentri_Miete.2007_0.10, 
                                    xend="Kontroll", yend=Gentri_Miete.2007_0.10), 
                        linetype="dotted", color="red", size=0.5) +
  geom_segment(mapping=aes(x="Gentri", y=Gentri_Miete.2007_0.90, 
                           xend="Kontroll", yend=Gentri_Miete.2007_0.90), 
               linetype="dotted", color="red", size=0.5) 
p1Miete.2007 <- p1Miete.2007 + 
  annotate("text", x=1.5, y=7.4, label="Q[0.9]^Gentri", parse=TRUE, col="red") +
  annotate("text", x=1.5, y=4.9, label="Q[0.1]^Gentri", parse=TRUE, col="red")
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
                        linetype="dotted", color="red", size=0.5) +
  geom_segment(mapping=aes(x="Gentri", y=Gentri_Armut.2007_0.90, 
                           xend="Kontroll", yend=Gentri_Armut.2007_0.90), 
               linetype="dotted", color="red", size=0.5) 
p1Armut.2007 <- p1Armut.2007 + 
  annotate("text", x=1.5, y=46.5, label="Q[0.9]^Gentri", parse=TRUE, col="red") +
  annotate("text", x=1.5, y=15.5, label="Q[0.1]^Gentri", parse=TRUE, col="red")
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
  ylab(expression(paste(Miete[2012],' ','(€)')))+
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

grid.arrange(p1Miete.2012, p2Miete.2012, ncol=2, nrow=1, widths=c(3,1))

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

grid.arrange(p1Armut.2012, p2Armut.2012, ncol=2, nrow=1, widths=c(3,1))

grid.arrange(p1Mietechgr, p2Mietechgr, p1Armutchg, p2Armutchg,
             p1Miete.2007, p2Miete.2007, p1Armut.2007, p2Armut.2007,
             ncol=4, nrow=2, widths=c(3,1,3,1,3,1,3,1))

grid.arrange(p1Mietechgr, p2Mietechgr, p1Armutchg, p2Armutchg,
             p1Miete.2007, p2Miete.2007, p1Armut.2007, p2Armut.2007,
             p1Miete.2012, p2Miete.2012, p1Armut.2012, p2Armut.2012,
             ncol=4, nrow=3, widths=c(3,1,3,1,3,1,3,1,3,1,3,1))

View(bpDF)

##### H1 Boxplots ####

#**** innere stadt ****##

p1ArmutG.2007  <- ggplot(bpGentriInnere , aes(Gentri, 
                                  Armut.2007, 
                                  weight=E_E.2007,
                                  fill=Gentri)) + 
  ylab(expression(paste(Armut[2007],' ','(%)')))+
  scale_y_continuous(breaks=pretty_breaks(n=brueche),
                     limits=c(4, 52))+ 
  geom_violin(scale = "width") + bp_theme
p1ArmutG.2007 <- p1ArmutG.2007 + geom_boxplot(width=.3)
p1ArmutG.2007

p2ArmutG.2007  <- ggplot(bpGesamtInnere, aes(valid,
                                  Armut.2007, 
                                  weight=E_E.2007)) +
  scale_y_continuous(breaks=pretty_breaks(n=brueche),
                     limits=c(4, 52)) +
  geom_violin(scale="width") +  bp_themeG
p2ArmutG.2007 <- p2ArmutG.2007 + geom_boxplot(width=.3)
p2ArmutG.2007

grid.arrange(p1ArmutG.2007, p2ArmutG.2007,
             ncol=2, nrow=1, widths=c(1,1))

p1MieteG.2007  <- ggplot(bpGentriInnere , aes(Gentri, 
                                              Miete.2007, 
                                              weight=E_E.2007,
                                              fill=Gentri)) + 
  ylab(expression(paste(Miete[2007],' ','(Euro)')))+
  scale_y_continuous(breaks=pretty_breaks(n=brueche),
                     limits=c(4.7, 10.9))+ 
  geom_violin(scale = "width") + bp_theme
p1MieteG.2007 <- p1MieteG.2007 + geom_boxplot(width=.3)
p1MieteG.2007

p2MieteG.2007  <- ggplot(bpGesamtInnere, aes(valid,
                                             Miete.2007, 
                                             weight=E_E.2007)) +
  scale_y_continuous(breaks=pretty_breaks(n=brueche),
                     limits=c(4.7, 10.9)) +
  geom_violin(scale="width") +  bp_themeG
p2MieteG.2007 <- p2MieteG.2007 + geom_boxplot(width=.3)
p2MieteG.2007

grid.arrange(p1MieteG.2007, p2MieteG.2007,
             ncol=2, nrow=1, widths=c(1,1))

grid.arrange(p1MieteG.2007, p2MieteG.2007,
             p1ArmutG.2007, p2ArmutG.2007,
             ncol=4, nrow=1, widths=c(1,1,1,1))


barMiete.2007I <- ggplot(data=bpGesamtInnere, aes(x=Miete.2007, fill=Gentri)) +
  geom_bar(width=0.5, binwidth=1, colour="white",size=1) + 
  theme(axis.title.y = element_text(face="bold",colour = "grey50"),
        axis.title.x = element_text(face="bold",colour = "grey50"),
        legend.text=element_text(colour = "grey50"),
        legend.title=element_text(face="bold",colour = "grey50"),
        legend.title=element_text(size=rel(0.7), face="bold"),
        legend.text=element_text(size=rel(0.5))) + 
  scale_x_continuous(breaks=seq(4,11,1), limits=c(4,11)) +
  scale_y_continuous(breaks=seq(0,60,10))  +
  xlab(expression(paste(Miete[2007],' ','(Euro/',m^{2},')'))) + 
  ylab(expression(paste('Anzahl der LOR'))) + 
  guides(fill=guide_legend(title="Kategorie")) 
barMiete.2007I

grid.arrange(p1MieteG.2007, p2MieteG.2007, barMiete.2007I, 
             ncol=3, nrow=1, widths=c(1,1,2))

barArmut.2007I <- ggplot(data=bpGesamtInnere, aes(x=Armut.2007, fill=Gentri)) +
  geom_bar(width=0.5, binwidth=5, colour="white",size=1) + 
  theme(axis.title.y = element_text(face="bold",colour = "grey50"),
        axis.title.x = element_text(face="bold",colour = "grey50"),
        legend.text=element_text(colour = "grey50"),
        legend.title=element_text(face="bold",colour = "grey50"),
        legend.title=element_text(size=rel(0.7), face="bold"),
        legend.text=element_text(size=rel(0.5))) + 
  scale_x_continuous(breaks=seq(0,60,10), limits=c(0,60)) +
  scale_y_continuous(breaks=seq(0,35,10))  +
  xlab(expression(paste(Armut[2007],' ','(%)'))) +
  ylab(expression(paste('Anzahl der LOR'))) + 
  guides(fill=guide_legend(title="Kategorie")) 
barArmut.2007I

grid.arrange(p1ArmutG.2007, p2ArmutG.2007, barArmut.2007I, 
             ncol=3, nrow=1, widths=c(1,1,2))

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#---- +++ Robustheitscheck von H 1 mit alternativer Klassifizierung +++ -----
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

#***** innere Stadt ******#


bpGentriAInnere   <- subset(bpDF, GentriA=="Gentri" & STADTRAUM=="innere Stadt")

p1ArmutG.2007  <- ggplot(bpGentriAInnere , aes(GentriA, 
                                               Armut.2007, 
                                               weight=E_E.2007,
                                               fill=GentriA)) + 
  ylab(expression(paste(Armut[2007],' ','(%)')))+
  scale_y_continuous(breaks=pretty_breaks(n=brueche),
                     limits=c(4, 52))+ 
  geom_violin(scale = "width") + bp_theme
p1ArmutG.2007 <- p1ArmutG.2007 + geom_boxplot(width=.3)
p1ArmutG.2007

p2ArmutG.2007  <- ggplot(bpGesamtInnere, aes(valid,
                                             Armut.2007, 
                                             weight=E_E.2007)) +
  scale_y_continuous(breaks=pretty_breaks(n=brueche),
                     limits=c(4, 52)) +
  geom_violin(scale="width") +  bp_themeG
p2ArmutG.2007 <- p2ArmutG.2007 + geom_boxplot(width=.3)
p2ArmutG.2007

grid.arrange(p1ArmutG.2007, p2ArmutG.2007,
             ncol=2, nrow=1, widths=c(1,1))

p1MieteG.2007  <- ggplot(bpGentriAInnere , aes(GentriA, 
                                               Miete.2007, 
                                               weight=E_E.2007,
                                               fill=GentriA)) + 
  ylab(expression(paste(Miete[2007],' ','(Euro)')))+
  scale_y_continuous(breaks=pretty_breaks(n=brueche),
                     limits=c(4.7, 10.9))+ 
  geom_violin(scale = "width") + bp_theme
p1MieteG.2007 <- p1MieteG.2007 + geom_boxplot(width=.3)
p1MieteG.2007

p2MieteG.2007  <- ggplot(bpGesamtInnere, aes(valid,
                                             Miete.2007, 
                                             weight=E_E.2007)) +
  scale_y_continuous(breaks=pretty_breaks(n=brueche),
                     limits=c(4.7, 10.9)) +
  geom_violin(scale="width") +  bp_themeG
p2MieteG.2007 <- p2MieteG.2007 + geom_boxplot(width=.3)
p2MieteG.2007

grid.arrange(p1MieteG.2007, p2MieteG.2007,
             ncol=2, nrow=1, widths=c(1,1))

grid.arrange(p1MieteG.2007, p2MieteG.2007,
             p1ArmutG.2007, p2ArmutG.2007,
             ncol=4, nrow=1, widths=c(1,1,1,1))


barMiete.2007I <- ggplot(data=bpGesamtInnere, aes(x=Miete.2007, fill=GentriA)) +
  geom_bar(width=0.5, binwidth=1, colour="white",size=1) + 
  theme(axis.title.y = element_text(face="bold",colour = "grey50"),
        axis.title.x = element_text(face="bold",colour = "grey50"),
        legend.text=element_text(colour = "grey50"),
        legend.title=element_text(face="bold",colour = "grey50"),
        legend.title=element_text(size=rel(0.7), face="bold"),
        legend.text=element_text(size=rel(0.5))) + 
  scale_x_continuous(breaks=seq(4,11,1), limits=c(4,11)) +
  scale_y_continuous(breaks=seq(0,60,10))  +
  xlab(expression(paste(Miete[2007],' ','(Euro/',m^{2},')'))) + 
  ylab(expression(paste('Anzahl der LOR'))) + 
  guides(fill=guide_legend(title="Kategorie")) 
barMiete.2007I

grid.arrange(p1MieteG.2007, p2MieteG.2007, barMiete.2007I, 
             ncol=3, nrow=1, widths=c(1,1,2))

barArmut.2007I <- ggplot(data=bpGesamtInnere, aes(x=Armut.2007, fill=GentriA)) +
  geom_bar(width=0.5, binwidth=5, colour="white",size=1) + 
  theme(axis.title.y = element_text(face="bold",colour = "grey50"),
        axis.title.x = element_text(face="bold",colour = "grey50"),
        legend.text=element_text(colour = "grey50"),
        legend.title=element_text(face="bold",colour = "grey50"),
        legend.title=element_text(size=rel(0.7), face="bold"),
        legend.text=element_text(size=rel(0.5))) + 
  scale_x_continuous(breaks=seq(0,60,10), limits=c(0,60)) +
  scale_y_continuous(breaks=seq(0,35,10))  +
  xlab(expression(paste(Armut[2007],' ','(%)'))) +
  ylab(expression(paste('Anzahl der LOR'))) + 
  guides(fill=guide_legend(title="Kategorie")) 
barArmut.2007I

grid.arrange(p1ArmutG.2007, p2ArmutG.2007, barArmut.2007I, 
             ncol=3, nrow=1, widths=c(1,1,2))


##### H2 Boxplots ####

#####**** gesamte Stadt ****####

p1FortzuegeR  <- ggplot(bpDF , aes(Gentri, 
                                  FortzuegeR, 
                                  weight=E_E.2007,
                                  fill=Gentri)) + 
  ylab(expression(paste('Fortzüge',' ','(%)')))+
  scale_y_continuous(breaks=pretty_breaks(n=brueche)) +
  coord_cartesian(ylim = c(3, 17)) +
  geom_violin(scale = "width") + bp_themeMITRAND
p1FortzuegeR <- p1FortzuegeR + geom_boxplot(width=.3)
p1FortzuegeR

p1FortzuegeUDAR  <- ggplot(bpDF , aes(Gentri, 
                                  FortzuegeUDAR, 
                                  weight=E_E.2007,
                                  fill=Gentri)) + 
  ylab(expression(paste('FortzügeA',' ','(%)')))+
  scale_y_continuous(breaks=pretty_breaks(n=brueche)) +
  coord_cartesian(ylim = c(0, 10)) +
  geom_violin(scale = "width") + bp_themeMITRAND
p1FortzuegeUDAR <- p1FortzuegeUDAR + geom_boxplot(width=.3)
p1FortzuegeUDAR

p1ZuzuegeR  <- ggplot(bpDF , aes(Gentri, 
                                   ZuzuegeR, 
                                   weight=E_E.2007,
                                   fill=Gentri)) + 
  ylab(expression(paste('Zuzüge',' ','(%)')))+
  scale_y_continuous(breaks=pretty_breaks(n=brueche)) +
  geom_violin(scale = "width") + bp_themeMITRAND
p1ZuzuegeR <- p1ZuzuegeR + geom_boxplot(width=.3)
p1ZuzuegeR

p1ZuzuegeUDAR  <- ggplot(bpDF , aes(Gentri, 
                                    ZuzuegeUDAR, 
                                    weight=E_E.2007,
                                    fill=Gentri)) + 
  ylab(expression(paste('ZuzügeA',' ','(%)')))+
  scale_y_continuous(breaks=pretty_breaks(n=brueche)) +
  coord_cartesian(ylim = c(0, 15)) +
  geom_violin(scale = "width") + bp_themeMITRAND
p1ZuzuegeUDAR <- p1ZuzuegeUDAR+ geom_boxplot(width=.3)
p1ZuzuegeUDAR

grid.arrange(p1FortzuegeR, p1ZuzuegeR,
             p1FortzuegeUDAR, p1ZuzuegeUDAR,
             ncol=2, nrow=2, widths=c(1,1,1,1))

#####**** innere Stadt ****####

p1FortzuegeRI  <- ggplot(bpGesamtInnere, aes(Gentri, 
                                            FortzuegeR, 
                                            weight=E_E.2007,
                                            fill=Gentri)) + 
  ylab(expression(paste('Fortzüge',' ','(%)')))+
  scale_y_continuous(breaks=pretty_breaks(n=brueche)) +
  coord_cartesian(ylim = c(3, 17)) +
  geom_violin(scale = "width") + bp_themeMITRAND
p1FortzuegeRI <- p1FortzuegeRI + geom_boxplot(width=.3)
#p1FortzuegeRI

p1FortzuegeUDARI  <- ggplot(bpGesamtInnere, aes(Gentri, 
                                             FortzuegeUDAR, 
                                             weight=E_E.2007,
                                             fill=Gentri)) + 
  ylab(expression(paste('FortzügeA',' ','(%)')))+
  scale_y_continuous(breaks=pretty_breaks(n=brueche)) +
  coord_cartesian(ylim = c(1,10)) +
  geom_violin(scale = "width") + bp_themeMITRAND
p1FortzuegeUDARI <- p1FortzuegeUDARI + geom_boxplot(width=.3)
#p1FortzuegeUDARI

p1ZuzuegeRI  <- ggplot(bpGesamtInnere, aes(Gentri, 
                                             ZuzuegeR, 
                                             weight=E_E.2007,
                                             fill=Gentri)) + 
  ylab(expression(paste('Zuzüge',' ','(%)')))+
  scale_y_continuous(breaks=pretty_breaks(n=brueche)) +
  coord_cartesian(ylim = c(5, 22)) +
  geom_violin(scale = "width") + bp_themeMITRAND
p1ZuzuegeRI <- p1ZuzuegeRI + geom_boxplot(width=.3)
#p1ZuzuegeRI

p1ZuzuegeUDARI  <- ggplot(bpGesamtInnere, aes(Gentri, 
                                              ZuzuegeUDAR, 
                                              weight=E_E.2007,
                                              fill=Gentri)) + 
  ylab(expression(paste('ZuzügeA',' ','(%)')))+
  scale_y_continuous(breaks=pretty_breaks(n=brueche)) +
  coord_cartesian(ylim = c(1, 14)) +
  geom_violin(scale = "width") + bp_themeMITRAND
p1ZuzuegeUDARI <- p1ZuzuegeUDARI + geom_boxplot(width=.3)
#p1ZuzuegeUDARI

grid.arrange(p1FortzuegeRI, p1ZuzuegeRI,
             p1FortzuegeUDARI, p1ZuzuegeUDARI,
             ncol=2, nrow=2, widths=c(1,1,1,1))

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#---- +++ Robustheitscheck von H 2 mit alternativer Klassifizierung +++ -----
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

#***** gesamte Stadt ******#

p1FortzuegeR  <- ggplot(bpDF , aes(GentriA, 
                                   FortzuegeR, 
                                   weight=E_E.2007,
                                   fill=GentriA)) + 
  ylab(expression(paste('Fortzüge',' ','(%)')))+
  scale_y_continuous(breaks=pretty_breaks(n=brueche)) +
  coord_cartesian(ylim = c(3, 17)) +
  geom_violin(scale = "width") + bp_themeMITRAND
p1FortzuegeR <- p1FortzuegeR + geom_boxplot(width=.3)

p1FortzuegeUDAR  <- ggplot(bpDF , aes(GentriA, 
                                      FortzuegeUDAR, 
                                      weight=E_E.2007,
                                      fill=GentriA)) + 
  ylab(expression(paste('FortzügeA',' ','(%)')))+
  scale_y_continuous(breaks=pretty_breaks(n=brueche)) +
  coord_cartesian(ylim = c(0, 10)) +
  geom_violin(scale = "width") + bp_themeMITRAND
p1FortzuegeUDAR <- p1FortzuegeUDAR + geom_boxplot(width=.3)

p1ZuzuegeR  <- ggplot(bpDF , aes(GentriA, 
                                 ZuzuegeR, 
                                 weight=E_E.2007,
                                 fill=GentriA)) + 
  ylab(expression(paste('Zuzüge',' ','(%)')))+
  scale_y_continuous(breaks=pretty_breaks(n=brueche)) +
  geom_violin(scale = "width") + bp_themeMITRAND
p1ZuzuegeR <- p1ZuzuegeR + geom_boxplot(width=.3)

p1ZuzuegeUDAR  <- ggplot(bpDF , aes(GentriA, 
                                    ZuzuegeUDAR, 
                                    weight=E_E.2007,
                                    fill=GentriA)) + 
  ylab(expression(paste('ZuzügeA',' ','(%)')))+
  scale_y_continuous(breaks=pretty_breaks(n=brueche)) +
  coord_cartesian(ylim = c(0, 15)) +
  geom_violin(scale = "width") + bp_themeMITRAND
p1ZuzuegeUDAR <- p1ZuzuegeUDAR+ geom_boxplot(width=.3)

grid.arrange(p1FortzuegeR, p1ZuzuegeR,
             p1FortzuegeUDAR, p1ZuzuegeUDAR,
             ncol=2, nrow=2, widths=c(1,1,1,1))

#***** innere Stadt ******#

p1FortzuegeRI  <- ggplot(bpGesamtInnere, aes(GentriA, 
                                             FortzuegeR, 
                                             weight=E_E.2007,
                                             fill=GentriA)) + 
  ylab(expression(paste('Fortzüge',' ','(%)')))+
  scale_y_continuous(breaks=pretty_breaks(n=brueche)) +
  coord_cartesian(ylim = c(3, 17)) +
  geom_violin(scale = "width") + bp_themeMITRAND
p1FortzuegeRI <- p1FortzuegeRI + geom_boxplot(width=.3)
#p1FortzuegeRI

p1FortzuegeUDARI  <- ggplot(bpGesamtInnere, aes(GentriA, 
                                                FortzuegeUDAR, 
                                                weight=E_E.2007,
                                                fill=GentriA)) + 
  ylab(expression(paste('FortzügeA',' ','(%)')))+
  scale_y_continuous(breaks=pretty_breaks(n=brueche)) +
  coord_cartesian(ylim = c(1,10)) +
  geom_violin(scale = "width") + bp_themeMITRAND
p1FortzuegeUDARI <- p1FortzuegeUDARI + geom_boxplot(width=.3)
#p1FortzuegeUDARI

p1ZuzuegeRI  <- ggplot(bpGesamtInnere, aes(GentriA, 
                                           ZuzuegeR, 
                                           weight=E_E.2007,
                                           fill=GentriA)) + 
  ylab(expression(paste('Zuzüge',' ','(%)')))+
  scale_y_continuous(breaks=pretty_breaks(n=brueche)) +
  coord_cartesian(ylim = c(5, 22)) +
  geom_violin(scale = "width") + bp_themeMITRAND
p1ZuzuegeRI <- p1ZuzuegeRI + geom_boxplot(width=.3)
#p1ZuzuegeRI

p1ZuzuegeUDARI  <- ggplot(bpGesamtInnere, aes(GentriA, 
                                              ZuzuegeUDAR, 
                                              weight=E_E.2007,
                                              fill=GentriA)) + 
  ylab(expression(paste('ZuzügeA',' ','(%)')))+
  scale_y_continuous(breaks=pretty_breaks(n=brueche)) +
  coord_cartesian(ylim = c(1, 14)) +
  geom_violin(scale = "width") + bp_themeMITRAND
p1ZuzuegeUDARI <- p1ZuzuegeUDARI + geom_boxplot(width=.3)
#p1ZuzuegeUDARI

grid.arrange(p1FortzuegeRI, p1ZuzuegeRI,
             p1FortzuegeUDARI, p1ZuzuegeUDARI,
             ncol=2, nrow=2, widths=c(1,1,1,1))


#********************************************
#                                           #
#         LONG DATA                         #
#                                           #
#********************************************

bpDFlong <- subset(LORdataFULLvalid, ZEIT==2007 | ZEIT==2012)
bpDFlong$ZEIT <- factor(bpDFlong$ZEIT)
bpDFlong$Gentri  <- factor(bpDFlong$Gentri,levels(bpDFlong$Gentri)[c(2,3,1)])
bpDFlong$GentriA <- factor(bpDFlong$GentriA,levels(bpDFlong$GentriA)[c(2,3,1)])
bpDFlong$valid <- as.factor("Gesamt")
bpDFlong$ZEITvalid <- interaction(bpDFlong$ZEIT,bpDFlong$valid)
levels(bpDFlong$ZEITvalid) <- c("Gesamt 2007","Gesamt 2012")
bpDFlong$ZEITGentri <- interaction(bpDFlong$ZEIT,bpDFlong$Gentri)
levels(bpDFlong$ZEITGentri) <- c("Gentri 2007","Gentri 2012",
                                 "Kontroll 2007", "Kontroll 2012",
                                 "Andere 2007", "Andere 2012")

##### Miete 2007-2012 ####

p1Miete.2007_2012  <- ggplot(bpDFlong, aes(x=ZEITGentri, 
                                           y=Miete,
                                           fill=Gentri,
                                           weight=E_E)) + 
  ylab("Miete (€)")+
  scale_y_continuous(breaks=pretty_breaks(n=brueche))+ 
  geom_violin(scale = "width") + bp_theme
p1Miete.2007_2012 <- p1Miete.2007_2012 + geom_boxplot(width=.3)
p1Miete.2007_2012

p2Miete.2007_2012  <- ggplot(bpDFlong, aes(x=ZEITvalid,
                                           y=Miete,
                                           weight=E_E)) +
  scale_y_continuous(breaks=pretty_breaks(n=brueche)) +
  geom_violin(scale="width") +  bp_themeG
p2Miete.2007_2012 <- p2Miete.2007_2012 + geom_boxplot(width=.3)

grid.arrange(p1Miete.2007_2012, p2Miete.2007_2012, ncol=2, nrow=1, widths=c(3,1))


##### Armut 2007-2012 ####

p1Armut.2007_2012  <- ggplot(bpDFlong, aes(x=ZEITGentri, 
                                           y=Armut,
                                           fill=Gentri,
                                           weight=E_E)) + 
  ylab("Armut (%)")+
  scale_y_continuous(breaks=pretty_breaks(n=brueche))+ 
  geom_violin(scale = "width") + bp_theme
p1Armut.2007_2012 <- p1Armut.2007_2012 + geom_boxplot(width=.3)
p1Armut.2007_2012

p2Armut.2007_2012  <- ggplot(bpDFlong, aes(x=ZEITvalid,
                                           y=Armut,
                                           weight=E_E)) +
  scale_y_continuous(breaks=pretty_breaks(n=brueche)) +
  geom_violin(scale="width") +  bp_themeG
p2Armut.2007_2012 <- p2Armut.2007_2012 + geom_boxplot(width=.3)

grid.arrange(p1Armut.2007_2012, p2Armut.2007_2012, ncol=2, nrow=1, widths=c(3,1))

##### MOSAIC PLOT: MietechgrQNTLS*ArmutchgQNTLS ######

fill_colors <- matrix(c("red", "gray", "gray","grey",
                       "gray", "gray", "gray","grey",
                       "gray", "gray", "gray","grey",
                       "gray", "gray", "gray","grey"), ncol = 4)

bpDF$MietechgrQNTL <- factor(bpDF$MietechgrQNTL,levels(bpDF$MietechgrQNTL)[c(4,3,2,1)])

mosaicDF <- table(bpDF$MietechgrQNTL, bpDF$ArmutchgQNTL,dnn=c("Relative Mietpressteigerung","Änderung der Armutsquote"))
levels(bpDF$valid)[levels(bpDF$valid)=="gültig"] <- "Gesamt"
levels(bpDF$MietechgrQNTL)[levels(bpDF$MietechgrQNTL)=="4.Quartil"] <- "4.Qrt"
levels(bpDF$MietechgrQNTL)[levels(bpDF$MietechgrQNTL)=="3.Quartil"] <- "3.Qrt"
levels(bpDF$MietechgrQNTL)[levels(bpDF$MietechgrQNTL)=="2.Quartil"] <- "2.Qrt"
levels(bpDF$MietechgrQNTL)[levels(bpDF$MietechgrQNTL)=="1.Quartil"] <- "1.Qrt"
levels(bpDF$ArmutchgQNTL)[levels(bpDF$ArmutchgQNTL)=="4.Quartil"] <- "4.Qrt"
levels(bpDF$ArmutchgQNTL)[levels(bpDF$ArmutchgQNTL)=="3.Quartil"] <- "3.Qrt"
levels(bpDF$ArmutchgQNTL)[levels(bpDF$ArmutchgQNTL)=="2.Quartil"] <- "2.Qrt"
levels(bpDF$ArmutchgQNTL)[levels(bpDF$ArmutchgQNTL)=="1.Quartil"] <- "1.Qrt"

mosaic(mosaicDF, gp = gpar(fill = fill_colors, col = 0))
assoc(mosaicDF,shade=TRUE) 

summary(bpDF$Gentri)
table(bpDF$STADTRAUM,bpDF$Gentri)
summary(bpDF$STADTRAUM)

ddply(bpDF, .(Gentri), summarize,
     #dist2STADTMITTE_mean=round(weighted.mean(dist2STADTMITTE), digits=0),
      dist2STADTMITTE_median=round(median(dist2STADTMITTE), digits=0),
      E_E.2007_median=round(median(E_E.2007), digits=0), 
      E_E.2007_sum=sum(E_E.2007),
      Mietechgr_mean=round(weighted.mean(Mietechgr,E_E.2007),digits=1),
      Miete.2007_mean=round(weighted.mean(Miete.2007,E_E.2007),digits=2),
      Miete.2012_mean=round(weighted.mean(Miete.2012,E_E.2007),digits=2),
      Armutchg_mean=round(weighted.mean(Armutchg,E_E.2007),digits=1)  ,
      Armut.2007_mean=round(weighted.mean(Armut.2007,E_E.2007),digits=1),
      Armut.2012_mean=round(weighted.mean(Armut.2012,E_E.2007),digits=1)) -> summaryGENTRI
write.table(summaryGENTRI,file="/home/dao/Desktop/MasterArbeit/R_files/KNITR/TabellenSkizzen/summaryGENTRI.csv",
            quote=T,append=F,sep=";",eol = "\n", na = "NA", dec = ",", row.names = T,col.names = T)

summaryGENTRI$dist2STADTMITTE_median
summaryGENTRI$E_E.2007_median
summaryGENTRI$E_E.2007_sum

Mietechgr_qntl <- wtd.quantile(bpDF$Mietechgr, weights=bpDF$E_E.2007, 
                     probs=c(0, .25, .5, .75, 1))
Mietechgr_qntl[4]

Armutchg_qntl <- wtd.quantile(bpDF$Armutchg, weights=bpDF$E_E.2007, 
                              probs=c(0, .25, .5, .75, 1))
Armutchg_qntl[2]

Gentri_Miete.2007_0.10
Gentri_Miete.2007_0.90

Gentri_Armut.2007_0.10
Gentri_Armut.2007_0.90

summarize(bpDF,
          dist2STADTMITTE_median=round(median(dist2STADTMITTE), digits=0),
          E_E.2007_median=round(median(E_E.2007), digits=0), 
          E_E.2007_sum=sum(E_E.2007),
          Mietechgr_mean=round(weighted.mean(Mietechgr,E_E.2007),digits=1),
          Miete.2007_mean=round(weighted.mean(Miete.2007,E_E.2007),digits=2),
          Miete.2012_mean=round(weighted.mean(Miete.2012,E_E.2007),digits=2),
          Armutchg_mean=round(weighted.mean(Armutchg,E_E.2007),digits=1)  ,
          Armut.2007_mean=round(weighted.mean(Armut.2007,E_E.2007),digits=1),
          Armut.2012_mean=round(weighted.mean(Armut.2012,E_E.2007),digits=1))



bar_theme <- theme(#axis.text.y = element_text(colour = "grey20",size =16),
                   axis.title.y = element_blank(),
                   axis.title.x = element_text(face="bold",colour = "grey50"),
                   #legend.text=element_text(colour = "grey20",size=14),
                   legend.title=element_text(face="bold",colour = "grey50"))

summaryBEZ_NAME <- ddply(bpDF, 
                         .(BEZ_NAME, Gentri), summarize,
                         E_E.2007_sum=sum(E_E.2007))

summaryBEZ_NAME <- arrange(summaryBEZ_NAME, Gentri, E_E.2007_sum)

names(summaryBEZ_NAME)[names(summaryBEZ_NAME)=="Gentri"] <- "Kategorie"
summaryBEZ_NAME$E_E.2007_sum <- summaryBEZ_NAME$E_E.2007_sum/1000

Reihenfolge <- rev(c("Friedrichshain-Kreuzberg","Neukölln",
                     "Pankow","Lichtenberg",
                     "Charlottenburg-Wilmersdorf", 
                     "Treptow-Köpenick", "Mitte",
                     "Tempelhof-Schöneberg","Spandau","Reinickendorf",
                     "Marzahn-Hellersdorf","Steglitz-Zehlendorf"))

bar1 <- ggplot(data=summaryBEZ_NAME, aes(x=factor(BEZ_NAME, levels=Reihenfolge), y=E_E.2007_sum, fill=Kategorie)) +
  geom_bar(stat='identity') + bar_theme + ylab("Einwohner*innenzahl 2007 in 1000") +  coord_flip() 
bar1

##### Unterschiede GENTRI / KONTROLL ####

p1  <- ggplot(bpDF, aes(Gentri,
                        weight=E_E.2007,
                        fill=Gentri)) + 
  ylab(expression(paste('Fortzüge',' ','(%)')))+
  scale_y_continuous(breaks=pretty_breaks(n=brueche)) +
  coord_cartesian(ylim = c(3, 17)) +
  geom_violin(scale = "width") + bp_themeMITRAND
p1FortzuegeRI <- p1FortzuegeRI + geom_boxplot(width=.3)
#p1FortzuegeRI

p1FortzuegeUDARI  <- ggplot(bpGesamtInnere, aes(Gentri, 
                                                FortzuegeUDAR, 
                                                weight=E_E.2007,
                                                fill=Gentri)) + 
  ylab(expression(paste('FortzügeA',' ','(%)')))+
  scale_y_continuous(breaks=pretty_breaks(n=brueche)) +
  coord_cartesian(ylim = c(1,10)) +
  geom_violin(scale = "width") + bp_themeMITRAND
p1FortzuegeUDARI <- p1FortzuegeUDARI + geom_boxplot(width=.3)
#p1FortzuegeUDARI

p1ZuzuegeRI  <- ggplot(bpGesamtInnere, aes(Gentri, 
                                           ZuzuegeR, 
                                           weight=E_E.2007,
                                           fill=Gentri)) + 
  ylab(expression(paste('Zuzüge',' ','(%)')))+
  scale_y_continuous(breaks=pretty_breaks(n=brueche)) +
  coord_cartesian(ylim = c(5, 22)) +
  geom_violin(scale = "width") + bp_themeMITRAND
p1ZuzuegeRI <- p1ZuzuegeRI + geom_boxplot(width=.3)
#p1ZuzuegeRI

p1ZuzuegeUDARI  <- ggplot(bpGesamtInnere, aes(Gentri, 
                                              ZuzuegeUDAR, 
                                              weight=E_E.2007,
                                              fill=Gentri)) + 
  ylab(expression(paste('ZuzügeA',' ','(%)')))+
  scale_y_continuous(breaks=pretty_breaks(n=brueche)) +
  coord_cartesian(ylim = c(1, 14)) +
  geom_violin(scale = "width") + bp_themeMITRAND
p1ZuzuegeUDARI <- p1ZuzuegeUDARI + geom_boxplot(width=.3)
#p1ZuzuegeUDARI

grid.arrange(p1FortzuegeRI, p1ZuzuegeRI,
             p1FortzuegeUDARI, p1ZuzuegeUDARI,
             ncol=2, nrow=2, widths=c(1,1,1,1))

boxplot(bpDF$E_65U110R.2007 ~ bpDF$Gentri)
boxplot(bpDF$E_65U110R.2012 ~ bpDF$Gentri)
boxplot(bpDF$E_18U35R.2007 ~ bpDF$Gentri)
boxplot(bpDF$E_18U35R.2012 ~ bpDF$Gentri)
boxplot(bpDF$E_18U35Rchg ~ bpDF$Gentri)
boxplot(bpDF$E_65U110Rchg ~ bpDF$Gentri)
boxplot(bpDF$AlleinerzHH.2012 ~ bpDF$Gentri)
boxplot(bpDF$HK_Turkchg ~ bpDF$Gentri)
boxplot(bpDF$HK_TurkRchg ~ bpDF$Gentri)
boxplot(bpDF$HK_ArabRchg ~ bpDF$Gentri)
boxplot(bpDF$MH_ERchg ~ bpDF$Gentri)
boxplot(bpDF$HK_EU15Rchg ~ bpDF$Gentri)
boxplot(bpDF$HK_EU27Rchg ~ bpDF$Gentri)
boxplot(bpDF$HK_EheJugRMHchg ~ bpDF$Gentri)
boxplot(bpDF$HK_EU27RMHhg ~ bpDF$Gentri)
boxplot(bpDF$MH_65U110R65U110chg ~ bpDF$Gentri)
boxplot(bpDF$MH_ERchg ~ bpDF$Gentri)

table(bpDF$SanGebiet.2012, bpDF$Gentri)

######### Statistiken zu Aufwertung ######

  MietechgrMEANI <- round(weighted.mean(bpDF$Mietechgr[bpDF$STADTRAUM=="innere Stadt"], 
                                        bpDF$E_E.2007[bpDF$STADTRAUM=="innere Stadt"]),2)
  MietechgrMEANI
  MietechgrMEANA <- round(weighted.mean(bpDF$Mietechgr[bpDF$STADTRAUM!="innere Stadt"], 
                                        bpDF$E_E.2007[bpDF$STADTRAUM!="innere Stadt"]),2)
  MietechgrMEANA
  ArmutchgMEANI <- round(weighted.mean(bpDF$Armutchg[bpDF$STADTRAUM=="innere Stadt"], 
                                       bpDF$E_E.2007[bpDF$STADTRAUM=="innere Stadt"]),2)
  ArmutchgMEANI
  ArmutchgMEANA <- round(weighted.mean(bpDF$Armutchg[bpDF$STADTRAUM!="innere Stadt"], 
                                       bpDF$E_E.2007[bpDF$STADTRAUM!="innere Stadt"]),2)
  ArmutchgMEANA

#### __MOBILITÄTSRATEN Statistiken__ ####

#### FortzügeR Vergleich ####

wtd.mean(x=bpDF$FortzuegeR[bpDF$GentriA=="Gentri"], 
         weights=bpDF$E_E.2007[bpDF$GentriA=="Gentri"])
wtd.mean(x=bpDF$FortzuegeR[bpDF$GentriA=="Kontroll"], 
         weights=bpDF$E_E.2007[bpDF$GentriA=="Kontroll"])
  
wtd.mean(x=bpDF$FortzuegeR, 
           weights=bpDF$E_E.2007)
wtd.mean(x=bpDF$FortzuegeUDAR, 
           weights=bpDF$E_E.2007)
wtd.mean(x=bpDF$ZuzuegeR, 
           weights=bpDF$E_E.2007)
wtd.mean(x=bpDF$ZuzuegeUDAR, 
           weights=bpDF$E_E.2007)
  wtd.quantile(x=bpDF$FortzuegeR, 
               weights=bpDF$E_E.2007,
               probs=c(0, .25, .5, .75, 1))
  wtd.quantile(x=bpDF$FortzuegeUDAR, 
               weights=bpDF$E_E.2007,
               probs=c(0, .25, .5, .75, 1))
  wtd.quantile(x=bpDF$ZuzuegeR, 
               weights=bpDF$E_E.2007,
               probs=c(0, .25, .5, .75, 1))
  wtd.quantile(x=bpDF$ZuzuegeUDAR, 
               weights=bpDF$E_E.2007,
               probs=c(0, .25, .5, .75, 1))

wtd.mean(x=bpGesamtInnere$FortzuegeR[bpGesamtInnere$Gentri=="Gentri"], 
         weights=bpGesamtInnere$E_E.2007[bpGesamtInnere$Gentri=="Gentri"])
wtd.mean(x=bpGesamtInnere$FortzuegeR[bpGesamtInnere$Gentri=="Kontroll"], 
         weights=bpGesamtInnere$E_E.2007[bpGesamtInnere$Gentri=="Kontroll"])
  
  
  
#----- TAB dist2Stadtmitte -----
wtd.quantile(x=bpDF$dist2STADTMITTE, 
               weights=bpDF$E_E.2007,
               probs=c(0, .25, .5, .75, 1))
  
wtd.quantile(x=bpDF$dist2STADTMITTE[bpDF$Gentri=="Gentri"], 
               weights=bpDF$E_E.2007[bpDF$Gentri=="Gentri"],
               probs=c(0, .25, .5, .75, 1))
  
wtd.quantile(x=bpDF$dist2STADTMITTE[bpDF$Gentri=="Kontroll"], 
               weights=bpDF$E_E.2007[bpDF$Gentri=="Kontroll"],
               probs=c(0, .25, .5, .75, 1))

wtd.quantile(x=bpDF$dist2STADTMITTE[bpDF$Gentri=="Andere"], 
               weights=bpDF$E_E.2007[bpDF$Gentri=="Andere"],
               probs=c(0, .25, .5, .75, 1))  
  
#---- TAB FortzuegeR -----  
wtd.quantile(x=bpDF$FortzuegeR[bpDF$Gentri=="Gentri"], 
             weights=bpDF$E_E.2007[bpDF$Gentri=="Gentri"],
             probs=c(0, .25, .5, .75, 1))
wtd.quantile(x=bpDF$FortzuegeR[bpDF$Gentri=="Kontroll"], 
             weights=bpDF$E_E.2007[bpDF$Gentri=="Kontroll"],
             probs=c(0, .25, .5, .75, 1))

wtd.quantile(x=bpGesamtInnere$FortzuegeR[bpGesamtInnere$Gentri=="Gentri"], 
             weights=bpGesamtInnere$E_E.2007[bpGesamtInnere$Gentri=="Gentri"],
             probs=c(0, .25, .5, .75, 1))
wtd.quantile(x=bpGesamtInnere$FortzuegeR[bpGesamtInnere$Gentri=="Kontroll"], 
             weights=bpGesamtInnere$E_E.2007[bpGesamtInnere$Gentri=="Kontroll"],
             probs=c(0, .25, .5, .75, 1))

  
  
  #### ZuzügeR Vergleich ####
  
  wtd.mean(x=bpDF$ZuzuegeR[bpDF$Gentri=="Gentri"], 
           weights=bpDF$E_E.2007[bpDF$Gentri=="Gentri"])
  wtd.mean(x=bpDF$ZuzuegeR[bpDF$Gentri=="Kontroll"], 
           weights=bpDF$E_E.2007[bpDF$Gentri=="Kontroll"])
  
  wtd.mean(x=bpGesamtInnere$ZuzuegeR[bpGesamtInnere$Gentri=="Gentri"], 
           weights=bpGesamtInnere$E_E.2007[bpGesamtInnere$Gentri=="Gentri"])
  wtd.mean(x=bpGesamtInnere$ZuzuegeR[bpGesamtInnere$Gentri=="Kontroll"], 
           weights=bpGesamtInnere$E_E.2007[bpGesamtInnere$Gentri=="Kontroll"])
  
  wtd.quantile(x=bpDF$ZuzuegeR[bpDF$Gentri=="Gentri"], 
               weights=bpDF$E_E.2007[bpDF$Gentri=="Gentri"],
               probs=c(0, .25, .5, .75, 1))
  wtd.quantile(x=bpDF$ZuzuegeR[bpDF$Gentri=="Kontroll"], 
               weights=bpDF$E_E.2007[bpDF$Gentri=="Kontroll"],
               probs=c(0, .25, .5, .75, 1))
  
  wtd.quantile(x=bpGesamtInnere$ZuzuegeR[bpGesamtInnere$Gentri=="Gentri"], 
               weights=bpGesamtInnere$E_E.2007[bpGesamtInnere$Gentri=="Gentri"],
               probs=c(0, .25, .5, .75, 1))
  wtd.quantile(x=bpGesamtInnere$ZuzuegeR[bpGesamtInnere$Gentri=="Kontroll"], 
               weights=bpGesamtInnere$E_E.2007[bpGesamtInnere$Gentri=="Kontroll"],
               probs=c(0, .25, .5, .75, 1))
  
  
  #### FortzuegeUDAR Vergleich ####
  
  wtd.mean(x=bpDF$FortzuegeUDAR[bpDF$Gentri=="Gentri"], 
           weights=bpDF$E_E.2007[bpDF$Gentri=="Gentri"])
  wtd.mean(x=bpDF$FortzuegeUDAR[bpDF$Gentri=="Kontroll"], 
           weights=bpDF$E_E.2007[bpDF$Gentri=="Kontroll"])
  
  wtd.mean(x=bpGesamtInnere$FortzuegeUDAR[bpGesamtInnere$Gentri=="Gentri"], 
           weights=bpGesamtInnere$E_E.2007[bpGesamtInnere$Gentri=="Gentri"])
  wtd.mean(x=bpGesamtInnere$FortzuegeUDAR[bpGesamtInnere$Gentri=="Kontroll"], 
           weights=bpGesamtInnere$E_E.2007[bpGesamtInnere$Gentri=="Kontroll"])
  
  wtd.quantile(x=bpDF$FortzuegeUDAR[bpDF$Gentri=="Gentri"], 
               weights=bpDF$E_E.2007[bpDF$Gentri=="Gentri"],
               probs=c(0, .25, .5, .75, 1))
  wtd.quantile(x=bpDF$FortzuegeUDAR[bpDF$Gentri=="Kontroll"], 
               weights=bpDF$E_E.2007[bpDF$Gentri=="Kontroll"],
               probs=c(0, .25, .5, .75, 1))
  
  wtd.quantile(x=bpGesamtInnere$FortzuegeUDAR[bpGesamtInnere$Gentri=="Gentri"], 
               weights=bpGesamtInnere$E_E.2007[bpGesamtInnere$Gentri=="Gentri"],
               probs=c(0, .25, .5, .75, 1))
  wtd.quantile(x=bpGesamtInnere$FortzuegeUDAR[bpGesamtInnere$Gentri=="Kontroll"], 
               weights=bpGesamtInnere$E_E.2007[bpGesamtInnere$Gentri=="Kontroll"],
               probs=c(0, .25, .5, .75, 1))
  wtd.quantile(x=bpGesamtInnere$FortzuegeUDAR, 
               weights=bpGesamtInnere$E_E.2007,
               probs=c(0, .25, .5, .75, 1))

  #### ZuzügeUDAR Vergleich ####
  
  wtd.mean(x=bpDF$ZuzuegeUDAR[bpDF$Gentri=="Gentri"], 
           weights=bpDF$E_E.2007[bpDF$Gentri=="Gentri"])
  wtd.mean(x=bpDF$ZuzuegeUDAR[bpDF$Gentri=="Kontroll"], 
           weights=bpDF$E_E.2007[bpDF$Gentri=="Kontroll"])
  
  wtd.mean(x=bpGesamtInnere$ZuzuegeUDAR[bpGesamtInnere$Gentri=="Gentri"], 
           weights=bpGesamtInnere$E_E.2007[bpGesamtInnere$Gentri=="Gentri"])
  wtd.mean(x=bpGesamtInnere$ZuzuegeUDAR[bpGesamtInnere$Gentri=="Kontroll"], 
           weights=bpGesamtInnere$E_E.2007[bpGesamtInnere$Gentri=="Kontroll"])
  
  wtd.quantile(x=bpDF$ZuzuegeUDAR[bpDF$Gentri=="Gentri"], 
               weights=bpDF$E_E.2007[bpDF$Gentri=="Gentri"],
               probs=c(0, .25, .5, .75, 1))
  wtd.quantile(x=bpDF$ZuzuegeUDAR[bpDF$Gentri=="Kontroll"], 
               weights=bpDF$E_E.2007[bpDF$Gentri=="Kontroll"],
               probs=c(0, .25, .5, .75, 1))
  
  wtd.quantile(x=bpGesamtInnere$ZuzuegeUDAR[bpGesamtInnere$Gentri=="Gentri"], 
               weights=bpGesamtInnere$E_E.2007[bpGesamtInnere$Gentri=="Gentri"],
               probs=c(0, .25, .5, .75, 1))
  wtd.quantile(x=bpGesamtInnere$ZuzuegeUDAR[bpGesamtInnere$Gentri=="Kontroll"], 
               weights=bpGesamtInnere$E_E.2007[bpGesamtInnere$Gentri=="Kontroll"],
               probs=c(0, .25, .5, .75, 1))
  wtd.quantile(x=bpGesamtInnere$ZuzuegeUDAR, 
               weights=bpGesamtInnere$E_E.2007,
               probs=c(0, .25, .5, .75, 1))
  
  wtd.quantile(x=bpDF$ZuzuegeUDAR, 
               weights=bpDF$E_E.2007,
               probs=c(0, .25, .5, .75, 1))
  
  
  wtd.quantile(x=bpDF$ZuzuegeUDAR[bpDF$Gentri=="Gentri"], 
               weights=bpDF$E_E.2007[bpDF$Gentri=="Gentri"],
               probs=c(0, .25, .5, .75, 1)) -
    wtd.quantile(x=bpDF$FortzuegeUDAR[bpDF$Gentri=="Gentri"], 
                 weights=bpDF$E_E.2007[bpDF$Gentri=="Gentri"],
                 probs=c(0, .25, .5, .75, 1))
  
  wtd.quantile(x=bpDF$ZuzuegeR[bpDF$Gentri=="Gentri"], 
               weights=bpDF$E_E.2007[bpDF$Gentri=="Gentri"],
               probs=c(0, .25, .5, .75, 1)) -
    wtd.quantile(x=bpDF$FortzuegeR[bpDF$Gentri=="Gentri"], 
                 weights=bpDF$E_E.2007[bpDF$Gentri=="Gentri"],
                 probs=c(0, .25, .5, .75, 1))
  
  wtd.quantile(x=bpDF$ZuzuegeUDAR[bpDF$Gentri=="Kontroll"], 
               weights=bpDF$E_E.2007[bpDF$Gentri=="Kontroll"],
               probs=c(0, .25, .5, .75, 1)) -
    wtd.quantile(x=bpDF$FortzuegeUDAR[bpDF$Gentri=="Kontroll"], 
                 weights=bpDF$E_E.2007[bpDF$Gentri=="Kontroll"],
                 probs=c(0, .25, .5, .75, 1))
  
  wtd.quantile(x=bpDF$ZuzuegeR[bpDF$Gentri=="Kontroll"], 
               weights=bpDF$E_E.2007[bpDF$Gentri=="Kontroll"],
               probs=c(0, .25, .5, .75, 1)) -
    wtd.quantile(x=bpDF$FortzuegeR[bpDF$Gentri=="Kontroll"], 
                 weights=bpDF$E_E.2007[bpDF$Gentri=="Kontroll"],
                 probs=c(0, .25, .5, .75, 1))
  
  wtd.quantile(x=bpDF$ZuzuegeUDAR, 
               weights=bpDF$E_E.2007,
               probs=c(0, .25, .5, .75, 1)) -
    wtd.quantile(x=bpDF$FortzuegeUDAR, 
                 weights=bpDF$E_E.2007,
                 probs=c(0, .25, .5, .75, 1))

  ODdf[with(ODdf, VonLOR=="08010301" & NachLOR=="08010301" ),]
write.csv(bpDF[bpDF$RAUMID_NAME=="Reuterkiez",], file = "/home/dao/Desktop/Reuterkiez.csv", dec =",", sep = ";",quote = TRUE)
  bpDF[bpDF$RAUMID_NAME=="Reuterkiez",c("E_E.2007","E_E.2008","E_E.2009","E_E.2010","E_E.2011","E_E.2012")]
  bpDF[bpDF$RAUMID_NAME=="Reuterkiez",c("FortzuegeR.2007","FortzuegeR.2008","FortzuegeR.2009","FortzuegeR.2010","FortzuegeR.2011","FortzuegeR.2012")]
  bpDF[bpDF$RAUMID_NAME=="Reuterkiez",c("FortzuegeUDAR.2007","FortzuegeUDAR.2008","FortzuegeUDAR.2009","FortzuegeUDAR.2010","FortzuegeUDAR.2011","FortzuegeUDAR.2012")]
  bpDF[bpDF$RAUMID_NAME=="Reuterkiez",c("ZuzuegeR.2007","ZuzuegeR.2008","ZuzuegeR.2009","ZuzuegeR.2010","ZuzuegeR.2011","ZuzuegeR.2012")]
  bpDF[bpDF$RAUMID_NAME=="Reuterkiez",c("ZuzuegeUDAR.2007","ZuzuegeUDAR.2008","ZuzuegeUDAR.2009","ZuzuegeUDAR.2010","ZuzuegeUDAR.2011","ZuzuegeUDAR.2012")]

  bpDF$E_65U110R.2007
  
  bpDF[bpDF$RAUMID_NAME=="Reuterkiez",c("E_U18R.2007","E_U18R.2012")]
  bpDF[bpDF$RAUMID_NAME=="Reuterkiez",c("E_65U110R.2007","E_65U110R.2012")]
  LOR4reg@data[LOR4reg@data$RAUMID_NAME=="Reuterkiez",c("FortzuegeR","Mietechgr","Armutchg",
                                                        "Miete.2007C","Armut.2007C",     
                                                        "AlleinerzHH.2012C","E_U18R.2007C","E_18U35R.2007C","E_65U110R.2007C",
                                                        "PDAU10.2007C","StaedtWohnungen.2012C","SanGebiet.2007")]
  
  bpDF[bpDF$PRG_NAME=="Kreuzberg Nord",c("RAUMID_NAME","FortzuegeR","StaedtWohnungen.2012","Armut.2007","Mietechgr")]
  
  wtd.cors(x=bpDF$Miete.2007, y=bpDF$Armut.2007, weight=bpDF$E_E.2007)

LOR4reg@data$Armutchg[LOR4reg@data$RAUMID_NAME=="Reuterkiez"]
# -6.9
LOR4reg@data$Mietechgr[LOR4reg@data$RAUMID_NAME=="Reuterkiez"]
# 8.5
wtd.mean(x=LOR4reg@data$Mietechgr[LOR4reg@data$Gentri=="Gentri"],
         weights = LOR4reg@data$E_E.2007[LOR4reg@data$Gentri=="Gentri"])
# 5.698169
5.6981*0.135
# 0.7692435
(5.6981*0.135)*6
# 4.615461

#### Mietechgr TABELLE nach Kategorie ####

wtd.quantile(x=bpDF$Mietechgr[bpDF$Gentri=="Gentri"], 
             weights=bpDF$E_E.2007[bpDF$Gentri=="Gentri"],
             probs=c(0, .25, .5, .75, 1))

wtd.quantile(x=bpDF$Mietechgr[bpDF$Gentri=="Kontroll"], 
             weights=bpDF$E_E.2007[bpDF$Gentri=="Kontroll"],
             probs=c(0, .25, .5, .75, 1))

wtd.quantile(x=bpDF$Mietechgr, 
             weights=bpDF$E_E.2007,
             probs=c(0, .25, .5, .75, 1))

wtd.mean(x=bpDF$Mietechgr[bpDF$Gentri=="Gentri"], 
         weights=bpDF$E_E.2007[bpDF$Gentri=="Gentri"])
wtd.mean(x=bpDF$Mietechgr[bpDF$Gentri=="Kontroll"], 
         weights=bpDF$E_E.2007[bpDF$Gentri=="Kontroll"])
wtd.mean(x=bpDF$Mietechgr, 
         weights=bpDF$E_E.2007)

#### Armutchg TABELLE nach Kategorie ####

wtd.quantile(x=bpDF$Armutchg[bpDF$Gentri=="Gentri"], 
             weights=bpDF$E_E.2007[bpDF$Gentri=="Gentri"],
             probs=c(0, .25, .5, .75, 1))

wtd.quantile(x=bpDF$Armutchg[bpDF$Gentri=="Kontroll"], 
             weights=bpDF$E_E.2007[bpDF$Gentri=="Kontroll"],
             probs=c(0, .25, .5, .75, 1))

wtd.quantile(x=bpDF$Armutchg, 
             weights=bpDF$E_E.2007,
             probs=c(0, .25, .5, .75, 1))

wtd.mean(x=bpDF$Armutchg[bpDF$Gentri=="Gentri"], 
         weights=bpDF$E_E.2007[bpDF$Gentri=="Gentri"])
wtd.mean(x=bpDF$Armutchg[bpDF$Gentri=="Kontroll"], 
         weights=bpDF$E_E.2007[bpDF$Gentri=="Kontroll"])
wtd.mean(x=bpDF$Armutchg, 
         weights=bpDF$E_E.2007)

  
#************************************
####*****   INTRA LOR UMZÜGE ****####
#************************************
  
INTRAdfpre      <-  subset(ODdf,VonLOR==NachLOR,select=-c(NachLOR,dyad,BinnenWand.2007,
                                                          BinnenWand.2013,BinnenWand.Sum))
names(INTRAdfpre)[1] <- "RAUMID"
bpDF4INTRAdf <-  subset(bpDF, select=c("RAUMID","RAUMID_NAME","STADTRAUM","Gentri","GentriA",
                                      "E_E.2008","E_E.2009","E_E.2010","E_E.2011","E_E.2012",
                                      "Fortzuege.2008","Fortzuege.2009","Fortzuege.2010",
                                      "Fortzuege.2011","Fortzuege.2012"))

source("/home/dao/Desktop/MasterArbeit/R_files/functions/merge_with_order_FUNCTION.R")
INTRAdf  <- merge.with.order(bpDF4INTRAdf, INTRAdfpre, sort=F,
                                  by.x="RAUMID", by.y="RAUMID",
                                  all.x=T, all.y=F,
                                  keep_order=1)
#View(INTRAdf)

INTRAdf$IntraR.2008 <- round((INTRAdf$BinnenWand.2008/INTRAdf$E_E.2008)*100,digits=1)
INTRAdf$IntraR.2009 <- round((INTRAdf$BinnenWand.2009/INTRAdf$E_E.2009)*100,digits=1)
INTRAdf$IntraR.2010 <- round((INTRAdf$BinnenWand.2010/INTRAdf$E_E.2010)*100,digits=1)
INTRAdf$IntraR.2011 <- round((INTRAdf$BinnenWand.2011/INTRAdf$E_E.2011)*100,digits=1)
INTRAdf$IntraR.2012 <- round((INTRAdf$BinnenWand.2012/INTRAdf$E_E.2012)*100,digits=1)

INTRAdf$IntraFR.2008 <- round((INTRAdf$BinnenWand.2008/INTRAdf$Fortzuege.2008)*100,digits=1)
INTRAdf$IntraFR.2009 <- round((INTRAdf$BinnenWand.2009/INTRAdf$Fortzuege.2009)*100,digits=1)
INTRAdf$IntraFR.2010 <- round((INTRAdf$BinnenWand.2010/INTRAdf$Fortzuege.2010)*100,digits=1)
INTRAdf$IntraFR.2011 <- round((INTRAdf$BinnenWand.2011/INTRAdf$Fortzuege.2011)*100,digits=1)
INTRAdf$IntraFR.2012 <- round((INTRAdf$BinnenWand.2012/INTRAdf$Fortzuege.2012)*100,digits=1)

View(INTRAdf)

INTRAdflong <- reshape(data = INTRAdf, direction="long", 
     varying=list(c("E_E.2008","E_E.2009","E_E.2010","E_E.2011","E_E.2012"),
                  c("Fortzuege.2008","Fortzuege.2009","Fortzuege.2010","Fortzuege.2011","Fortzuege.2012"),
                  c("BinnenWand.2008","BinnenWand.2009","BinnenWand.2010","BinnenWand.2011","BinnenWand.2012"),
                  c("IntraR.2008","IntraR.2009","IntraR.2010","IntraR.2011","IntraR.2012"),
                  c("IntraFR.2008","IntraFR.2009","IntraFR.2010","IntraFR.2011","IntraFR.2012")),  
        idvar=c("RAUMID","RAUMID_NAME","STADTRAUM","Gentri","GentriA"), 
        timevar="ZEIT",
     v.names = c("E_E", "Fortzuege","BinnenWand","IntraR","IntraFR"),
     times = c(2008:2012))

#save(INTRAdflong, file = "/home/dao/Desktop/MasterArbeit/R_files/INTRAdflong.Rdata")

INTRAdflongI  <- subset(INTRAdflong, STADTRAUM=="innere Stadt")

INTRAdflongI <- INTRAdflongI[order(INTRAdflongI$ZEIT, INTRAdflongI$Gentri),]

ddply(INTRAdflongI, c("ZEIT","Gentri"), summarize,
      IntraR_mean=round(weighted.mean(IntraR,E_E),digits=1),
      IntraFR_mean=round(weighted.mean(IntraFR,E_E),digits=1)) -> summaryINTRAdflongI

ddply(INTRAdflong, c("ZEIT","Gentri"), summarize,
      IntraR_mean=round(weighted.mean(IntraR,E_E),digits=1),
      IntraFR_mean=round(weighted.mean(IntraFR,E_E),digits=1)) -> summaryINTRAdflong

# hier stimmt was nich. 2008 konstant kann nich sein.
ggplot(summaryINTRAdflongI, aes(ZEIT, IntraR_mean, group = Gentri, colour = Gentri)) + geom_line(size=1)
ggplot(summaryINTRAdflong, aes(ZEIT, IntraR_mean, group = Gentri, colour = Gentri)) + geom_line(size=1)

pIntraG <- ggplot(summaryINTRAdflong, aes(ZEIT, IntraFR_mean, group = Gentri, colour = Gentri)) + 
  geom_line(size=1) +
  theme(axis.title.y = element_text(face="bold",colour = "grey50"),
        axis.title.x = element_blank(),
        legend.text=element_text(colour = "grey50"),
        legend.title=element_text(face="bold",colour = "grey50"),
        legend.position="none",
        plot.margin=unit(c(0.5,0.3,0.5,0.5), "cm")) +
  ylab("Anteil der Intra-LOR Umzüge an allen Fortzügen in %") +
  annotate("text", x = 2009, y = 13, label = "gesamte Stadt", size=6, face="bold",colour = "grey50") +
  scale_y_continuous(breaks=seq(10,22,2), limits=c(11,24))
pIntraG

pIntraI <- ggplot(summaryINTRAdflongI, aes(ZEIT, IntraFR_mean, group = Gentri, colour = Gentri)) + 
  geom_line(size=1) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.text=element_text(colour = "grey50"),
        legend.title=element_text(face="bold",colour = "grey50"),
        plot.margin=unit(c(0.5,0.5,0.5,-0.3), "cm")) +
  ylab("Anteil der Intra-LOR Umzüge an allen Fortzügen in %")  +  
  annotate("text", x=2009, y = 13, label = "innere Stadt", size=6, face="bold",colour = "grey50") +
  scale_y_continuous(breaks=seq(10,22,2), limits=c(11,24))
pIntraI

grid.arrange(pIntraG, pIntraI, ncol=2, nrow=1, widths=c(1,1.15))

grid.arrange(p1FortzuegeRI, p1ZuzuegeRI,
             p1FortzuegeUDARI, p1ZuzuegeUDARI,
             ncol=2, nrow=2, widths=c(1,1,1,1))

ddply(LORdataFULLvalid, c("ZEIT","Gentri"), summarize,
      FortzuegeR_mean=round(weighted.mean(FortzuegeR,E_E),digits=1),
      ZuzuegeUDAR_mean=round(weighted.mean(ZuzuegeUDAR,E_E),digits=1)) -> summaryLORdataFULLvalid

ggplot(summaryLORdataFULLvalid, aes(ZEIT, FortzuegeR_mean, group = Gentri, colour = Gentri)) + geom_line(size=0.8)

ggplot(LORdataFULLvalid[LORdataFULLvalid$RAUMID_NAME=="Reuterkiez",]) + geom_line(x=ZEIT, y=FortzuegeR,size=0.8)

#************************************
####*****   Allgemeine UMZÜGE Plots ****####
#************************************
LORdataFULL4umzuege <- subset(LORdataFULL,ZEIT!=2007)

umzuegeDFlong <- ddply(LORdataFULL4umzuege, c("ZEIT","STADTRAUM"), summarize,
                       FortzuegeR_mean   =round(weighted.mean(FortzuegeR,E_E),digits=1),
                       ZuzuegeR_mean    =round(weighted.mean(ZuzuegeR,E_E),digits=1),
                       FortzuegeUDAR_mean=round(weighted.mean(FortzuegeUDAR,E_E),digits=1),
                       ZuzuegeUDAR_mean  =round(weighted.mean(ZuzuegeUDAR,E_E),digits=1)) 
umzuegeDFlong$ZEIT <- as.numeric(levels(umzuegeDFlong$ZEIT ))[umzuegeDFlong$ZEIT ]
names(umzuegeDFlong)[2] <- "Stadtraum"
#str(umzuegeDFlong)
#View(umzuegeDFlong)

ALLGfortzuegeRplot <- ggplot(umzuegeDFlong, aes(ZEIT, FortzuegeR_mean, group = Stadtraum, colour = Stadtraum)) +
  geom_line(size=0.7) +
  theme(axis.title.y = element_text(face="bold",colour = "grey50"),
        axis.title.x = element_blank(),
        legend.text=element_text(colour = "grey50"),
        legend.title=element_text(face="bold",colour = "grey50"),
        legend.position="none",
        plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm")) +
  ylab("Binnenfortzugsrate in %") +
  scale_y_continuous(breaks=seq(3,12,1), limits=c(2.8,12.05))
#ALLGfortzuegeRplot

ALLGzuzuegeUDARplot <- ggplot(umzuegeDFlong, aes(ZEIT, ZuzuegeUDAR_mean, group = Stadtraum, colour = Stadtraum)) +
  geom_line(size=0.7) +
  theme(axis.title.y = element_text(face="bold",colour = "grey50"),
        axis.title.x = element_blank(),
        legend.text=element_text(colour = "grey50"),
        legend.title=element_text(face="bold",colour = "grey50"),
        legend.position=c(0.8,0.85),
        plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm")) +
  ylab("Aussenzuzugsrate in %")  +
  scale_y_continuous(breaks=seq(3,12,1), limits=c(2.8,12.05))
#ALLGzuzuegeUDARplot 

grid.arrange(ALLGfortzuegeRplot, ALLGzuzuegeUDARplot, ncol=2, nrow=1, widths=c(1,1))

#************************************
####*****   Allgemeine UMZÜGE Gentri Plots ****####
#************************************

umzuegeDFlong <- ddply(LORdataFULL4umzuege, c("ZEIT","Gentri"), summarize,
                       FortzuegeR_mean   =round(weighted.mean(FortzuegeR,E_E),digits=1),
                       ZuzuegeR_mean    =round(weighted.mean(ZuzuegeR,E_E),digits=1),
                       FortzuegeUDAR_mean=round(weighted.mean(FortzuegeUDAR,E_E),digits=1),
                       ZuzuegeUDAR_mean  =round(weighted.mean(ZuzuegeUDAR,E_E),digits=1)) 
umzuegeDFlong$ZEIT <- as.numeric(levels(umzuegeDFlong$ZEIT ))[umzuegeDFlong$ZEIT ]
umzuegeDFlong$Gentri  <- factor(umzuegeDFlong$Gentri,levels(umzuegeDFlong$Gentri)[c(2,3,1)])
names(umzuegeDFlong)[2] <- "Kategorie"
#str(umzuegeDFlong)
#View(umzuegeDFlong)

ALLGfortzuegeRGENTRIplot <- ggplot(umzuegeDFlong, aes(ZEIT, FortzuegeR_mean, group = Kategorie, colour = Kategorie)) +
  geom_line(size=0.7) +
  theme(axis.title.y = element_text(face="bold",colour = "grey50"),
        axis.title.x = element_blank(),
        legend.text=element_text(colour = "grey50"),
        legend.title=element_text(face="bold",colour = "grey50"),
        legend.position="none",
        plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm")) +
  ylab("Binnenfortzugsrate in %") +
  scale_y_continuous(breaks=seq(3,14,1), limits=c(3,14))
#ALLGfortzuegeRGENTRIplot

ALLGzuzuegeUDARGENTRIplot <- ggplot(umzuegeDFlong, aes(ZEIT, ZuzuegeUDAR_mean, group = Kategorie, colour = Kategorie)) +
  geom_line(size=0.7) +
  theme(axis.title.y = element_text(face="bold",colour = "grey50"),
        axis.title.x = element_blank(),
        legend.text=element_text(colour = "grey50"),
        legend.title=element_text(face="bold",colour = "grey50"),
        legend.position=c(0.8,0.85),
        plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm")) +
  ylab("Aussenzuzugsrate in %")  +
  scale_y_continuous(breaks=seq(3,14,1), limits=c(3,14))
#ALLGzuzuegeUDARGENTRIplot 

grid.arrange(ALLGfortzuegeRGENTRIplot, ALLGzuzuegeUDARGENTRIplot, ncol=2, nrow=1, widths=c(1,1))

######### Korrelation Soziale Aufwertung und Binnenfortzüge ######

library("weights")
require("Hmisc")
wtd.cor(LOR4reg@data$Armutchg,LOR4reg@data$FortzuegeR,weight=LOR4reg@data$E_E.2007)
wtd.cor(LOR4reg@data$Armutchg[LOR4reg@data$STADTRAUM=="innere Stadt"],LOR4reg@data$FortzuegeR[LOR4reg@data$STADTRAUM=="innere Stadt"],weight=LOR4reg@data$E_E.2007[LOR4reg@data$STADTRAUM=="innere Stadt"])
wtd.cor(LOR4reg@data$Armutchg[LOR4reg@data$STADTRAUM=="innere Stadt"],LOR4reg@data$ZuzuegeUDAR[LOR4reg@data$STADTRAUM=="innere Stadt"],weight=LOR4reg@data$E_E.2007[LOR4reg@data$STADTRAUM=="innere Stadt"])

wtd.cor(LOR4reg@data$Mietechgr,LOR4reg@data$FortzuegeR,weight=LOR4reg@data$E_E.2007)
wtd.cor(LOR4reg@data$Mietechgr[LOR4reg@data$STADTRAUM=="innere Stadt"],LOR4reg@data$FortzuegeR[LOR4reg@data$STADTRAUM=="innere Stadt"],weight=LOR4reg@data$E_E.2007[LOR4reg@data$STADTRAUM=="innere Stadt"])
wtd.cor(LOR4reg@data$Mietechgr[LOR4reg@data$STADTRAUM=="innere Stadt"],LOR4reg@data$ZuzuegeUDAR[LOR4reg@data$STADTRAUM=="innere Stadt"],weight=LOR4reg@data$E_E.2007[LOR4reg@data$STADTRAUM=="innere Stadt"])

