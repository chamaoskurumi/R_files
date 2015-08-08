#********************************************
#                                           #
#   Variablengenerierung für Analyse        #
#                                           #
#********************************************

# ____ Packages ______ ----------------------------------------------------------------

library("plyr")
library("reshape2")
library("foreign")
library("sp")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# I.) LOR long Datensatz ------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
LORdataFULL4GenerateVARIABLES <- LORdataFULLv1
LORdataFULLv2 <- LORdataFULL4GenerateVARIABLES

#names(LORdataFULLv2)
#View(LORdataFULLv2)

###### a.) Miete #####

LORdataFULLv2$MieteNom     <- round((LORdataFULLv2$Miete_H1_wmean+LORdataFULLv2$Miete_H2_wmean)/2, digits=2)

# Miete Deflationieren: Basisjahr 2012 Index=1
Mdef12 <- subset(LORdataFULLv2, ZEIT=="2012", select=c(MieteNom))
Mdef11 <- round(subset(LORdataFULLv2, ZEIT=="2011", 
                select=c(MieteNom))/VPI$VPIbasis2012[VPI$Zeit==2011], digits=2)
Mdef10 <- round(subset(LORdataFULLv2, ZEIT=="2010", 
                select=c(MieteNom))/VPI$VPIbasis2012[VPI$Zeit==2010], digits=2)
Mdef09 <- round(subset(LORdataFULLv2, ZEIT=="2009", 
                select=c(MieteNom))/VPI$VPIbasis2012[VPI$Zeit==2009], digits=2)
Mdef08 <- round(subset(LORdataFULLv2, ZEIT=="2008", 
                select=c(MieteNom))/VPI$VPIbasis2012[VPI$Zeit==2008], digits=2)
Mdef07 <- round(subset(LORdataFULLv2, ZEIT=="2007", 
                select=c(MieteNom))/VPI$VPIbasis2012[VPI$Zeit==2007], digits=2)

Miete2merge <- rbind(Mdef07,Mdef08,Mdef09,Mdef10,Mdef11,Mdef12)
colnames(Miete2merge) <- "Miete"
remove(Mdef07,Mdef08,Mdef09,Mdef10,Mdef11,Mdef12)

LORdataFULLv2ORD <- LORdataFULLv2[order(LORdataFULLv2[,"ZEIT"]), ]
#View(LORdataFULLv2ORD) 

LORdataFULLv2ORD <- data.frame(LORdataFULLv2ORD,Miete2merge$Miete)
colnames(LORdataFULLv2ORD)[dim(LORdataFULLv2ORD)[2]] <- "Miete"

LORdataFULLv2 <- LORdataFULLv2ORD[order(LORdataFULLv2ORD[,"RAUMID"],LORdataFULLv2ORD[,"ZEIT"]), ]
#View(LORdataFULLv2)

LORdataFULLv2              <- ddply(LORdataFULLv2,"RAUMID", transform,
                                  Mietechg = c(NA,diff(Miete)))
LORdataFULLv2              <- ddply(LORdataFULLv2,"RAUMID", transform,
                                  Mietechgr = c(NA,exp(diff(log(Miete)))-1))
LORdataFULLv2$Mietechgr    <- round((LORdataFULLv2$Mietechgr*100), digits=1)

###### b.) Altergruppen Inländer ######

LORdataFULLv2$E_U18         <- (LORdataFULLv2$E_U1    +
                                LORdataFULLv2$E_1U6   +
                                LORdataFULLv2$E_6U15  +
                                LORdataFULLv2$E_15U18)

LORdataFULLv2$E_15U65       <- (LORdataFULLv2$E_15U18 +
                                  LORdataFULLv2$E_18U25 +
                                  LORdataFULLv2$E_25U55 +
                                  LORdataFULLv2$E_55U65)

LORdataFULLv2$E_18U35       <- (LORdataFULLv2$E_18U25 +
                                  LORdataFULLv2$E_E25_27 +
                                  LORdataFULLv2$E_E27_30 +
                                  LORdataFULLv2$E_E30_35)

LORdataFULLv2$E_18U65       <- (LORdataFULLv2$E_18U25 +
                                LORdataFULLv2$E_25U55 +
                                LORdataFULLv2$E_55U65)

LORdataFULLv2$E_65U110      <- (LORdataFULLv2$E_65U80 +
                                LORdataFULLv2$E_80U110)

LORdataFULLv2$E_U1R         <- round(( LORdataFULLv2$E_U1       / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$E_1U6R        <- round(( LORdataFULLv2$E_1U6      / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$E_6U15R       <- round(( LORdataFULLv2$E_6U15     / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$E_15U18R      <- round(( LORdataFULLv2$E_15U18    / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$E_18U25R      <- round(( LORdataFULLv2$E_18U25    / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$E_25U55R      <- round(( LORdataFULLv2$E_25U55    / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$E_55U65R      <- round(( LORdataFULLv2$E_55U65    / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$E_65U80R      <- round(( LORdataFULLv2$E_65U80    / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$E_80U110R     <- round(( LORdataFULLv2$E_80U110   / LORdataFULLv2$E_E )*100,digits=1)

LORdataFULLv2$E_U18R        <- round(( LORdataFULLv2$E_U18      / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$E_18U35R      <- round(( LORdataFULLv2$E_18U35    / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$E_15U65R      <- round(( LORdataFULLv2$E_15U65    / LORdataFULLv2$E_E )*100,digits=1) 
LORdataFULLv2$E_18U65R      <- round(( LORdataFULLv2$E_18U65    / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$E_65U110R     <- round(( LORdataFULLv2$E_65U110   / LORdataFULLv2$E_E )*100,digits=1)

###### c.) Altergruppen Ausländer ######

LORdataFULLv2$E_AU18         <- (LORdataFULLv2$E_AU1    +
                                  LORdataFULLv2$E_A1U6   +
                                  LORdataFULLv2$E_A6U15  +
                                  LORdataFULLv2$E_A15U18)

LORdataFULLv2$E_A18U65       <- (LORdataFULLv2$E_A18U25 +
                                 LORdataFULLv2$E_A25U55 +
                                 LORdataFULLv2$E_A55U65)

LORdataFULLv2$E_A65U110      <- (LORdataFULLv2$E_A65U80 +
                                 LORdataFULLv2$E_A80U110)

LORdataFULLv2$E_AU1R         <- round(( LORdataFULLv2$E_AU1       / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$E_A1U6R        <- round(( LORdataFULLv2$E_A1U6      / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$E_A6U15R       <- round(( LORdataFULLv2$E_A6U15     / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$E_A15U18R      <- round(( LORdataFULLv2$E_A15U18    / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$E_A18U25R      <- round(( LORdataFULLv2$E_A18U25    / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$E_A25U55R      <- round(( LORdataFULLv2$E_A25U55    / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$E_A55U65R      <- round(( LORdataFULLv2$E_A55U65    / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$E_A65U80R      <- round(( LORdataFULLv2$E_A65U80    / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$E_A80U110R     <- round(( LORdataFULLv2$E_A80U110   / LORdataFULLv2$E_E )*100,digits=1)

LORdataFULLv2$E_AU18R        <- round(( LORdataFULLv2$E_AU18      / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$E_A18U65R      <- round(( LORdataFULLv2$E_A18U65    / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$E_A65U110R     <- round(( LORdataFULLv2$E_A65U110   / LORdataFULLv2$E_E )*100,digits=1)

LORdataFULLv2$E_AU18RU18       <- round(( LORdataFULLv2$E_AU18      / LORdataFULLv2$E_U18    )*100,digits=1)
LORdataFULLv2$E_A18U65R18U65   <- round(( LORdataFULLv2$E_A18U65    / LORdataFULLv2$E_18U65  )*100,digits=1)
LORdataFULLv2$E_A65U110R65U110 <- round(( LORdataFULLv2$E_A65U110   / LORdataFULLv2$E_65U110 )*100,digits=1)

###### d.) Altergruppen Migrationshintergrund ######

LORdataFULLv2$MH_U18         <- (LORdataFULLv2$MH_U1    +
                                 LORdataFULLv2$MH_1U6   +
                                 LORdataFULLv2$MH_6U15  +
                                 LORdataFULLv2$MH_15U18)

LORdataFULLv2$MH_18U65       <- (LORdataFULLv2$MH_18U25 +
                                 LORdataFULLv2$MH_25U55 +
                                 LORdataFULLv2$MH_55U65)

LORdataFULLv2$MH_65U110      <- (LORdataFULLv2$MH_65U80 +
                                 LORdataFULLv2$MH_80U110)

LORdataFULLv2$MH_U1R         <- round(( LORdataFULLv2$MH_U1       / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$MH_1U6R        <- round(( LORdataFULLv2$MH_1U6      / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$MH_6U15R       <- round(( LORdataFULLv2$MH_6U15     / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$MH_15U18R      <- round(( LORdataFULLv2$MH_15U18    / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$MH_18U25R      <- round(( LORdataFULLv2$MH_18U25    / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$MH_25U55R      <- round(( LORdataFULLv2$MH_25U55    / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$MH_55U65R      <- round(( LORdataFULLv2$MH_55U65    / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$MH_65U80R      <- round(( LORdataFULLv2$MH_65U80    / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$MH_80U110R     <- round(( LORdataFULLv2$MH_80U110   / LORdataFULLv2$E_E )*100,digits=1)

LORdataFULLv2$MH_U18R        <- round(( LORdataFULLv2$MH_U18      / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$MH_18U65R      <- round(( LORdataFULLv2$MH_18U65    / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$MH_65U110R     <- round(( LORdataFULLv2$MH_65U110   / LORdataFULLv2$E_E )*100,digits=1)

LORdataFULLv2$MH_U18RU18       <- round(( LORdataFULLv2$MH_U18      / LORdataFULLv2$E_U18    )*100,digits=1)
LORdataFULLv2$MH_18U65R18U65   <- round(( LORdataFULLv2$MH_18U65    / LORdataFULLv2$E_18U65  )*100,digits=1)
LORdataFULLv2$MH_65U110R65U110 <- round(( LORdataFULLv2$MH_65U110   / LORdataFULLv2$E_65U110 )*100,digits=1)

###### e.) Ausländeranteil & Migrationshintergrund ######

LORdataFULLv2$E_AR         <- round(( LORdataFULLv2$E_A        / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$MH_ER        <- round(( LORdataFULLv2$MH_E       / LORdataFULLv2$E_E )*100,digits=1)

LORdataFULLv2$HK_TurkR     <- round(( LORdataFULLv2$HK_Turk    / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$HK_ArabR     <- round(( LORdataFULLv2$HK_Arab    / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$HK_EU15R     <- round(( LORdataFULLv2$HK_EU15    / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$HK_EU27R     <- round(( LORdataFULLv2$HK_EU27    / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$HK_PolenR    <- round(( LORdataFULLv2$HK_Polen   / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$HK_EheJugR   <- round(( LORdataFULLv2$HK_EheJug  / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$HK_EheSUR    <- round(( LORdataFULLv2$HK_EheSU   / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$HK_SonstR    <- round(( LORdataFULLv2$HK_Sonst   / LORdataFULLv2$E_E )*100,digits=1)

LORdataFULLv2$HK_TurkRMH     <- round(( LORdataFULLv2$HK_Turk    / LORdataFULLv2$MH_E )*100,digits=1)
LORdataFULLv2$HK_ArabRMH     <- round(( LORdataFULLv2$HK_Arab    / LORdataFULLv2$MH_E )*100,digits=1)
LORdataFULLv2$HK_EU15RMH     <- round(( LORdataFULLv2$HK_EU15    / LORdataFULLv2$MH_E )*100,digits=1)
LORdataFULLv2$HK_EU27RMH     <- round(( LORdataFULLv2$HK_EU27    / LORdataFULLv2$MH_E )*100,digits=1)
LORdataFULLv2$HK_PolenRMH    <- round(( LORdataFULLv2$HK_Polen   / LORdataFULLv2$MH_E )*100,digits=1)
LORdataFULLv2$HK_EheJugRMH   <- round(( LORdataFULLv2$HK_EheJug  / LORdataFULLv2$MH_E )*100,digits=1)
LORdataFULLv2$HK_EheSURMH    <- round(( LORdataFULLv2$HK_EheSU   / LORdataFULLv2$MH_E )*100,digits=1)
LORdataFULLv2$HK_SonstRMH    <- round(( LORdataFULLv2$HK_Sonst   / LORdataFULLv2$MH_E )*100,digits=1)

###### f.) Wohnlage ######

LORdataFULLv2$WLOL    <- (LORdataFULLv2$WLEINFOL + 
                          LORdataFULLv2$WLMITOL  +
                          LORdataFULLv2$WLGUTOL) 
LORdataFULLv2$WLML    <- (LORdataFULLv2$WLEINFML + 
                          LORdataFULLv2$WLMITML  +
                          LORdataFULLv2$WLGUTML) 

LORdataFULLv2$WLEINF    <- (LORdataFULLv2$WLEINFOL + LORdataFULLv2$WLEINFML) 
LORdataFULLv2$WLMIT     <- (LORdataFULLv2$WLMITOL  + LORdataFULLv2$WLMITML)        
LORdataFULLv2$WLGUT     <- (LORdataFULLv2$WLGUTOL  + LORdataFULLv2$WLGUTML)     

LORdataFULLv2$WLEINFOLR     <- round(( LORdataFULLv2$WLEINFOL  / LORdataFULLv2$E_E )*100,digits=1)          
LORdataFULLv2$WLEINFMLR     <- round(( LORdataFULLv2$WLEINFML  / LORdataFULLv2$E_E )*100,digits=1)   
LORdataFULLv2$WLMITOLR      <- round(( LORdataFULLv2$WLMITOL   / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$WLMITMLR      <- round(( LORdataFULLv2$WLMITML   / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$WLGUTOLR      <- round(( LORdataFULLv2$WLGUTOL   / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$WLGUTMLR      <- round(( LORdataFULLv2$WLGUTML   / LORdataFULLv2$E_E )*100,digits=1)
#LORdataFULLv2$WLNZORDR      <- round(( LORdataFULLv2$WLNZORD   / LORdataFULLv2$E_E )*100,digits=1)      

LORdataFULLv2$WLOLR      <- round(( LORdataFULLv2$WLOL   / LORdataFULLv2$E_E )*100,digits=1)    
LORdataFULLv2$WLMLR      <- round(( LORdataFULLv2$WLML   / LORdataFULLv2$E_E )*100,digits=1)    

LORdataFULLv2$WLEINFR      <- round(( LORdataFULLv2$WLEINF      / LORdataFULLv2$E_E )*100,digits=1)    
LORdataFULLv2$WLMITR       <- round(( LORdataFULLv2$WLMIT       / LORdataFULLv2$E_E )*100,digits=1)    
LORdataFULLv2$WLGUTR       <- round(( LORdataFULLv2$WLGUT       / LORdataFULLv2$E_E )*100,digits=1)  

LORdataFULLv2$WL[LORdataFULLv2$WLEINFR > 33.3] <-  "einfach"
LORdataFULLv2$WL[LORdataFULLv2$WLMITR  > 33.3] <-  "mittel"
LORdataFULLv2$WL[LORdataFULLv2$WLGUTR  > 33.3] <-  "gut"
LORdataFULLv2$WL <- factor(LORdataFULLv2$WL)

LORdataFULLv2$WL

###### g.) Wohndauer ######

LORdataFULLv2              <- ddply(LORdataFULLv2,"RAUMID", transform,
                                    PDAU10chg = c(NA,diff(PDAU10)))
LORdataFULLv2              <- ddply(LORdataFULLv2,"RAUMID", transform,
                                    PDAU5chg  = c(NA,diff(PDAU5)))

###### h.) Sozialindikatoren ######

LORdataFULLv2$AloseABS        <- round(((LORdataFULLv2$Alose/100)*LORdataFULLv2$E_15U65), digits=0)
LORdataFULLv2$AloseR          <- round((LORdataFULLv2$AloseABS/LORdataFULLv2$E_E)*100,digits=1)
LORdataFULLv2$Armut           <- round((LORdataFULLv2$AloseR + LORdataFULLv2$nicht_Alose_Hartz),digits=1)

####### i.) Binnenwanderungen ######

LORdataFULLv2$FortzuegeR      <- round(( LORdataFULLv2$Fortzuege   / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$ZuzuegeR        <- round(( LORdataFULLv2$Zuzuege     / LORdataFULLv2$E_E )*100,digits=1)

####### j.) Aussenwanderungen #######

LORdataFULLv2$FortzuegeUD      <- (LORdataFULLv2$FortzuegeU  + LORdataFULLv2$FortzuegeD ) 
LORdataFULLv2$FortzuegeDA      <- (LORdataFULLv2$FortzuegeD  + LORdataFULLv2$FortzuegeA ) 
LORdataFULLv2$FortzuegeUDA     <- (LORdataFULLv2$FortzuegeU  + LORdataFULLv2$FortzuegeD + LORdataFULLv2$FortzuegeA ) 

LORdataFULLv2$FortzuegeUR      <- round(( LORdataFULLv2$FortzuegeU   / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$FortzuegeDR      <- round(( LORdataFULLv2$FortzuegeD   / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$FortzuegeAR      <- round(( LORdataFULLv2$FortzuegeA   / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$FortzuegeUDR     <- round(( LORdataFULLv2$FortzuegeUD  / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$FortzuegeDAR     <- round(( LORdataFULLv2$FortzuegeDA  / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$FortzuegeUDAR    <- round(( LORdataFULLv2$FortzuegeUDA / LORdataFULLv2$E_E )*100,digits=1)

LORdataFULLv2$ZuzuegeUD      <- (LORdataFULLv2$ZuzuegeU  + LORdataFULLv2$ZuzuegeD ) 
LORdataFULLv2$ZuzuegeDA      <- (LORdataFULLv2$ZuzuegeD  + LORdataFULLv2$ZuzuegeA ) 
LORdataFULLv2$ZuzuegeUDA     <- (LORdataFULLv2$ZuzuegeU  + LORdataFULLv2$ZuzuegeD + LORdataFULLv2$ZuzuegeA ) 

LORdataFULLv2$ZuzuegeUR      <- round(( LORdataFULLv2$ZuzuegeU   / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$ZuzuegeDR      <- round(( LORdataFULLv2$ZuzuegeD   / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$ZuzuegeAR      <- round(( LORdataFULLv2$ZuzuegeA   / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$ZuzuegeUDR     <- round(( LORdataFULLv2$ZuzuegeUD  / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$ZuzuegeDAR     <- round(( LORdataFULLv2$ZuzuegeDA  / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$ZuzuegeUDAR    <- round(( LORdataFULLv2$ZuzuegeUDA / LORdataFULLv2$E_E )*100,digits=1)

###### k.) Sanierungsgebiete #######

LORdataFULLv2$SanGebietID <- LORdataFULLv2$SanGebiet
LORdataFULLv2$SanGebiet   <- -1
LORdataFULLv2$SanGebiet[is.na(LORdataFULLv2$SanGebietID)] <- "nein"
LORdataFULLv2$SanGebiet[LORdataFULLv2$SanGebiet!="nein"]  <- "ja"
LORdataFULLv2$SanGebiet<- factor(LORdataFULLv2$SanGebiet)
LORdataFULLv2$SanGebiet

###### l.) Referenzkategorien für kategorielle Variablen setzen #######

LORdataFULLv2$WL               <- factor(LORdataFULLv2$WL,levels(LORdataFULLv2$WL)[c(1,3,2)])
LORdataFULLv2$SanGebiet_KLASSE <- factor(LORdataFULLv2$SanGebiet_KLASSE,levels(LORdataFULLv2$SanGebiet_KLASSE)[c(4,1,3,2)])
LORdataFULLv2$SanGebiet        <- factor(LORdataFULLv2$SanGebiet, levels(LORdataFULLv2$SanGebiet)[c(2,1)])

###### m.) Unnötige Vars droppen & Var order ändern ######

names(LORdataFULLv2)
LORdataFULLv3 <- subset(LORdataFULLv2, select=-c(Miete_H1_wmean,
                                                Miete_H2_wmean,
                                                HK_NZOrd,
                                                WLNZORD,
                                                EinfWhnlageLaerm,
                                                MigHinter_u18,
                                                E_E15_18,E_E18_21,E_E21_25,              
                                                E_E25_27,E_E27_30,E_E30_35,
                                                E_U1, E_1U6, E_6U15, E_15U18, E_18U25, E_25U55, E_55U65, E_65U80, E_80U110,
                                                E_U1R, E_1U6R, E_6U15R, E_15U18R, E_18U25R, E_25U55R, E_55U65R, E_65U80R, E_80U110R,  
                                                E_AU1, E_A1U6, E_A6U15, E_A15U18, E_A18U25, E_A25U55, E_A55U65, E_A65U80, E_A80U110,
                                                E_AU1R, E_A1U6R, E_A6U15R, E_A15U18R, E_A18U25R, E_A25U55R, E_A55U65R, E_A65U80R, E_A80U110R, 
                                                MH_U1, MH_1U6, MH_6U15, MH_15U18, MH_18U25, MH_25U55, MH_55U65, MH_65U80, MH_80U110,
                                                MH_U1R, MH_1U6R, MH_6U15R, MH_15U18R, MH_18U25R, MH_25U55R, MH_55U65R, MH_65U80R, MH_80U110R,
                                                WanderVol,WanderSaldo,WanderSaldo_u6,
                                                SanGebietID))
names(LORdataFULLv3)

LORdataFULLv4 <- LORdataFULLv3[c("ZEIT", 
               # ID Variablen
               "RAUMID",     "RAUMID_NAME",
               "BZR",        "BZR_NAME", 
               "PGR",        "PRG_NAME",
               "BEZ",        "BEZ_NAME",       
               "STADTRAUM",  "FL_HA", "dist2STADTMITTE",
               # Gesamteinwohner
               "E_E",        
               "E_U18",      "E_18U35",     "E_15U65",     "E_18U65",     "E_65U110",
               "E_U18R",     "E_18U35R",    "E_15U65R",    "E_18U65R",    "E_65U110R",
               # Ausländer
        #       "E_A",         "E_AR",       
        #       "E_AU18",      "E_A18U65",      "E_A65U110",
        #       "E_AU18R",     "E_A18U65R",     "E_A65U110R",
        #       "E_AU18RU18",  "E_A18U65R18U65","E_A65U110R65U110", 
               # Migrationshintergrund
               "MH_E",        "MH_ER",  
               "MH_U18",      "MH_18U65",      "MH_65U110",
               "MH_U18R",     "MH_18U65R",     "MH_65U110R",
               "MH_U18RU18",  "MH_18U65R18U65","MH_65U110R65U110",
               "HK_EU15",     "HK_EU27",    "HK_Polen",  
               "HK_EheJug",   "HK_EheSU",   "HK_Turk",      "HK_Arab",   
               "HK_Sonst",  
               "HK_EU15R",    "HK_EU27R",    "HK_PolenR",  
               "HK_EheJugR",  "HK_EheSUR",   "HK_TurkR",    "HK_ArabR",   
               "HK_SonstR",                 
               "HK_EU15RMH",  "HK_EU27RMH",    "HK_PolenRMH",  
               "HK_EheJugRMH","HK_EheSURMH",   "HK_TurkRMH",  "HK_ArabRMH",
               "HK_SonstRMH", 
               # Wohndauer
               "EINW10",     "EINW5",     
               "DAU10",      "DAU5",       
               "PDAU10",     "PDAU5", 
               "PDAU10chg",  "PDAU5chg",
               # Wohnlage
        #       "WLEINFOL",   "WLEINFML",   
        #       "WLMITOL",    "WLMITML",   
        #       "WLGUTOL",    "WLGUTML",    
        #       "WLEINFOLR",  "WLEINFMLR",
        #       "WLMITOLR",   "WLMITMLR",
        #       "WLGUTOLR",   "WLGUTMLR",
               "WLOL",       "WLML",                  
               "WLOLR",      "WLMLR",  
               "WLEINF",     "WLMIT",      "WLGUT",
               "WLEINFR",    "WLMITR",     "WLGUTR",
               "WL",
               # Sozialindikatoren
               "Alose",     "AloseABS", "AloseR", "Alose_u25",  "Alose_langzeit", "nicht_Alose_Hartz", "Hartz_u15", 
               "Veraend_HartzEmpf_D",    "Veraend_HartzEmpf_Ausl",  "Veraend_Hartz_u15", "StaedtWohnungen",
               "AlleinerzHH",            "Altersarmut",  "Armut",  
               # SanierungsGebiete
               "SanGebiet",       "SanGebiet_NAME",    "SanGebiet_KLASSE",       
               # Mietdaten
               "Miete", "Mietechg", "Mietechgr", "MieteNom",
               # Binnenwanderungen
               "Fortzuege","Zuzuege","FortzuegeR","ZuzuegeR",
               # Außenwanderungen
               "FortzuegeU","FortzuegeD","FortzuegeA","FortzuegeUD","FortzuegeDA","FortzuegeUDA",
               "FortzuegeUR","FortzuegeDR","FortzuegeAR","FortzuegeUDR","FortzuegeDAR","FortzuegeUDAR",
               "ZuzuegeU","ZuzuegeD","ZuzuegeA","ZuzuegeUD","ZuzuegeDA","ZuzuegeUDA",
               "ZuzuegeUR","ZuzuegeDR","ZuzuegeAR","ZuzuegeUDR","ZuzuegeDAR","ZuzuegeUDAR")]
remove(LORdataFULLv2)
remove(LORdataFULLv3)

LORdataFULL <- LORdataFULLv4
remove(LORdataFULLv4)
head(LORdataFULL)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# II.) LOR wide Datensatz ------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

###### a.) Reshape long to wide ######

LORdataFULL4wide     <- arrange(LORdataFULL, RAUMID, ZEIT)
LORdataFULLwidev1    <- reshape(LORdataFULL4wide,
                            idvar = c("RAUMID",  "RAUMID_NAME", "BZR",
                             "BZR_NAME","PGR",     "PRG_NAME","BEZ",    
                             "BEZ_NAME","STADTRAUM","FL_HA","dist2STADTMITTE"),
                            v.names = c(
                              # Gesamteinwohner
                              "E_E",        
                              "E_U18",      "E_18U35",     "E_15U65",     "E_18U65",     "E_65U110",
                              "E_U18R",     "E_18U35R",    "E_15U65R",    "E_18U65R",    "E_65U110R",
                              # Ausländer
                #              "E_A",         "E_AR",       
                #              "E_AU18",      "E_A18U65",      "E_A65U110",
                #              "E_AU18R",     "E_A18U65R",     "E_A65U110R",
                #              "E_AU18RU18",  "E_A18U65R18U65","E_A65U110R65U110", 
                              # Migrationshintergrund
                              "MH_E",        "MH_ER",  
                              "MH_U18",      "MH_18U65",      "MH_65U110",
                              "MH_U18R",     "MH_18U65R",     "MH_65U110R",
                              "MH_U18RU18",  "MH_18U65R18U65","MH_65U110R65U110",
                              "HK_EU15",     "HK_EU27",    "HK_Polen",  
                              "HK_EheJug",   "HK_EheSU",   "HK_Turk",      "HK_Arab",   
                              "HK_Sonst",  
                              "HK_EU15R",    "HK_EU27R",    "HK_PolenR",  
                              "HK_EheJugR",  "HK_EheSUR",   "HK_TurkR",    "HK_ArabR",   
                              "HK_SonstR",                 
                              "HK_EU15RMH",  "HK_EU27RMH",    "HK_PolenRMH",  
                              "HK_EheJugRMH","HK_EheSURMH",   "HK_TurkRMH",  "HK_ArabRMH",
                              "HK_SonstRMH", 
                              # Wohndauer
                              "EINW10",     "EINW5",     
                              "DAU10",      "DAU5",       
                              "PDAU10",     "PDAU5", 
                              "PDAU10chg",  "PDAU5chg",
                              # Wohnlage
                #              "WLEINFOL",   "WLEINFML",   
                #              "WLMITOL",    "WLMITML",   
                #              "WLGUTOL",    "WLGUTML",    
                #              "WLEINFOLR",  "WLEINFMLR",
                #              "WLMITOLR",   "WLMITMLR",
                #              "WLGUTOLR",   "WLGUTMLR",
                              "WLOL",       "WLML",                  
                              "WLOLR",      "WLMLR",  
                              "WLEINF",     "WLMIT",      "WLGUT",
                              "WLEINFR",    "WLMITR",     "WLGUTR",
                              "WL",
                              # Sozialindikatoren
                              "Alose",     "AloseABS", "AloseR", "Alose_u25",  "Alose_langzeit", "nicht_Alose_Hartz", "Hartz_u15", 
                              "Veraend_HartzEmpf_D",    "Veraend_HartzEmpf_Ausl",  "Veraend_Hartz_u15", "StaedtWohnungen",
                              "AlleinerzHH",            "Altersarmut",  "Armut",  
                              # SanierungsGebiete
                              "SanGebiet",       "SanGebiet_NAME",    "SanGebiet_KLASSE",       
                              # Mietdaten
                              "Miete", "Mietechg", "Mietechgr", "MieteNom",
                              # Binnenwanderungen
                              "Fortzuege","Zuzuege","FortzuegeR","ZuzuegeR",
                              # Außenwanderungen
                              "FortzuegeU","FortzuegeD","FortzuegeA","FortzuegeUD","FortzuegeDA","FortzuegeUDA",
                              "FortzuegeUR","FortzuegeDR","FortzuegeAR","FortzuegeUDR","FortzuegeDAR","FortzuegeUDAR",
                              "ZuzuegeU","ZuzuegeD","ZuzuegeA","ZuzuegeUD","ZuzuegeDA","ZuzuegeUDA",
                              "ZuzuegeUR","ZuzuegeDR","ZuzuegeAR","ZuzuegeUDR","ZuzuegeDAR","ZuzuegeUDAR"),
                   timevar = "ZEIT",
                   direction = "wide") 
#names(LORdataFULLwidev1)

###### b.) Generierung von speziellen WIDE Variablen #####

LORdataFULLwidev2   <- LORdataFULLwidev1
#names(LORdataFULLwidev2)

# ---- Junge Erwachsene  ----
LORdataFULLwidev2$E_18U35chg       <- LORdataFULLwidev2$E_18U35.2012 -  LORdataFULLwidev2$E_18U35.2007
LORdataFULLwidev2$E_18U35Rchg      <- LORdataFULLwidev2$E_18U35R.2012 - LORdataFULLwidev2$E_18U35R.2007

# ---- Rentner  ----
LORdataFULLwidev2$E_65U110chg       <- LORdataFULLwidev2$E_65U110.2012 -  LORdataFULLwidev2$E_65U110.2007
LORdataFULLwidev2$E_65U110Rchg      <- LORdataFULLwidev2$E_65U110R.2012 - LORdataFULLwidev2$E_65U110R.2007

# ---- Änderung der Wohndauer ----
LORdataFULLwidev2$DAU5chg        <- LORdataFULLwidev2$DAU5.2012 -  LORdataFULLwidev2$DAU5.2007
LORdataFULLwidev2$DAU10chg       <- LORdataFULLwidev2$DAU10.2012 - LORdataFULLwidev2$DAU10.2007
LORdataFULLwidev2$PDAU5chg       <- LORdataFULLwidev2$PDAU5.2012 - LORdataFULLwidev2$PDAU5.2007
LORdataFULLwidev2$PDAU10chg      <- LORdataFULLwidev2$PDAU10.2012 -LORdataFULLwidev2$PDAU10.2007

# ---- Änderung Wohnlage ----
LORdataFULLwidev2$WLEINFchg        <- LORdataFULLwidev2$WLEINF.2012 -LORdataFULLwidev2$WLEINF.2007
LORdataFULLwidev2$WLMITchg         <- LORdataFULLwidev2$WLMIT.2012 - LORdataFULLwidev2$WLMIT.2007
LORdataFULLwidev2$WLGUTchg         <- LORdataFULLwidev2$WLGUT.2012 - LORdataFULLwidev2$WLGUT.2007

LORdataFULLwidev2$WLEINFRchg        <- LORdataFULLwidev2$WLEINFR.2012 -LORdataFULLwidev2$WLEINFR.2007
LORdataFULLwidev2$WLMITRchg         <- LORdataFULLwidev2$WLMITR.2012 - LORdataFULLwidev2$WLMITR.2007
LORdataFULLwidev2$WLGUTRchg         <- LORdataFULLwidev2$WLGUTR.2012 - LORdataFULLwidev2$WLGUTR.2007

# ---- Änderung Arbeitslosigkeit ----
LORdataFULLwidev2$Alosechg             <- LORdataFULLwidev2$Alose.2012 -            LORdataFULLwidev2$Alose.2007
LORdataFULLwidev2$Alose_langzeitchg    <- LORdataFULLwidev2$Alose_langzeit.2007  -  LORdataFULLwidev2$Alose_langzeit.2007
LORdataFULLwidev2$Alose_u25chg         <- LORdataFULLwidev2$Alose_u25.2012 -        LORdataFULLwidev2$Alose_u25.2007
LORdataFULLwidev2$Hartz_u15chg         <- LORdataFULLwidev2$Hartz_u15.2012 -        LORdataFULLwidev2$Hartz_u15.2007
LORdataFULLwidev2$nicht_Alose_Hartzchg <- LORdataFULLwidev2$nicht_Alose_Hartz.2012 -LORdataFULLwidev2$nicht_Alose_Hartz.2007
LORdataFULLwidev2$Armutchg             <- LORdataFULLwidev2$Armut.2012 -            LORdataFULLwidev2$Armut.2007

# ---- Änderung Migrationshintergrunsanteil ----
LORdataFULLwidev2$MH_Echg           <-  LORdataFULLwidev2$MH_E.2012 -            LORdataFULLwidev2$MH_E.2007
LORdataFULLwidev2$MH_ERchg          <-  LORdataFULLwidev2$MH_ER.2012 -           LORdataFULLwidev2$MH_ER.2007

LORdataFULLwidev2$MH_U18chg           <-  LORdataFULLwidev2$MH_U18.2012 -           LORdataFULLwidev2$MH_U18.2007
LORdataFULLwidev2$MH_U18RU18chg       <-  LORdataFULLwidev2$MH_U18RU18.2012 -       LORdataFULLwidev2$MH_U18RU18.2007
LORdataFULLwidev2$MH_65U110R65U110chg <-  LORdataFULLwidev2$MH_65U110R65U110.2012 - LORdataFULLwidev2$MH_65U110R65U110.2007

LORdataFULLwidev2$HK_EU15chg        <-  LORdataFULLwidev2$HK_EU15.2012 -         LORdataFULLwidev2$HK_EU15.2007
LORdataFULLwidev2$HK_EU27hg         <-  LORdataFULLwidev2$HK_EU27.2012 -         LORdataFULLwidev2$HK_EU27.2007
LORdataFULLwidev2$HK_Turkchg        <-  LORdataFULLwidev2$HK_Turk.2012 -         LORdataFULLwidev2$HK_Turk.2007
LORdataFULLwidev2$HK_Arabchg        <-  LORdataFULLwidev2$HK_Arab.2012 -         LORdataFULLwidev2$HK_Arab.2007
LORdataFULLwidev2$HK_EheJugchg      <-  LORdataFULLwidev2$HK_EheJug.2012 -       LORdataFULLwidev2$HK_EheJug.2007

LORdataFULLwidev2$HK_EU15Rchg        <-  LORdataFULLwidev2$HK_EU15R.2012 -         LORdataFULLwidev2$HK_EU15R.2007
LORdataFULLwidev2$HK_EU27Rhg         <-  LORdataFULLwidev2$HK_EU27R.2012 -         LORdataFULLwidev2$HK_EU27R.2007
LORdataFULLwidev2$HK_TurkRchg        <-  LORdataFULLwidev2$HK_TurkR.2012 -         LORdataFULLwidev2$HK_TurkR.2007
LORdataFULLwidev2$HK_ArabRchg        <-  LORdataFULLwidev2$HK_ArabR.2012 -         LORdataFULLwidev2$HK_ArabR.2007
LORdataFULLwidev2$HK_EheJugRchg      <-  LORdataFULLwidev2$HK_EheJugR.2012 -       LORdataFULLwidev2$HK_EheJugR.2007

LORdataFULLwidev2$HK_EU15RMHchg        <-  LORdataFULLwidev2$HK_EU15RMH.2012 -         LORdataFULLwidev2$HK_EU15RMH.2007
LORdataFULLwidev2$HK_EU27RMHhg         <-  LORdataFULLwidev2$HK_EU27RMH.2012 -         LORdataFULLwidev2$HK_EU27RMH.2007
LORdataFULLwidev2$HK_TurkRMHchg        <-  LORdataFULLwidev2$HK_TurkRMH.2012 -         LORdataFULLwidev2$HK_TurkRMH.2007
LORdataFULLwidev2$HK_ArabRMHchg        <-  LORdataFULLwidev2$HK_ArabRMH.2012 -         LORdataFULLwidev2$HK_ArabRMH.2007
LORdataFULLwidev2$HK_EheJugRMHchg      <-  LORdataFULLwidev2$HK_EheJugRMH.2012 -       LORdataFULLwidev2$HK_EheJugRMH.2007

# Mietpreisänderung
LORdataFULLwidev2$Mietechg    <-   LORdataFULLwidev2$Miete.2012 - LORdataFULLwidev2$Miete.2007
LORdataFULLwidev2$Mietechgr   <-   round(((LORdataFULLwidev2$Miete.2012/LORdataFULLwidev2$Miete.2007)^(1/6)-1)*100,digits=1)
LORdataFULLwidev2$MieteNomchg <-   LORdataFULLwidev2$MieteNom.2012 - LORdataFULLwidev2$MieteNom.2007

# ---- Binnenwanderungen ----
LORdataFULLwidev2$Fortzuege        <-   (LORdataFULLwidev2$Fortzuege.2007 +
                                         LORdataFULLwidev2$Fortzuege.2008 +
                                         LORdataFULLwidev2$Fortzuege.2009 +
                                         LORdataFULLwidev2$Fortzuege.2010 +
                                         LORdataFULLwidev2$Fortzuege.2011 +
                                         LORdataFULLwidev2$Fortzuege.2012)

LORdataFULLwidev2$Zuzuege          <-   (LORdataFULLwidev2$Zuzuege.2007 +
                                         LORdataFULLwidev2$Zuzuege.2008 +
                                         LORdataFULLwidev2$Zuzuege.2009 +
                                         LORdataFULLwidev2$Zuzuege.2010 +
                                         LORdataFULLwidev2$Zuzuege.2011 +
                                         LORdataFULLwidev2$Zuzuege.2012)

LORdataFULLwidev2$FortzuegeR         <- round((LORdataFULLwidev2$FortzuegeR.2007 +
                                                LORdataFULLwidev2$FortzuegeR.2008 +
                                                LORdataFULLwidev2$FortzuegeR.2009 +
                                                LORdataFULLwidev2$FortzuegeR.2010 +
                                                LORdataFULLwidev2$FortzuegeR.2011 +
                                                LORdataFULLwidev2$FortzuegeR.2012)/6,digits=2)
LORdataFULLwidev2$ZuzuegeR         <- round((LORdataFULLwidev2$ZuzuegeR.2007 +
                                                LORdataFULLwidev2$ZuzuegeR.2008 +
                                                LORdataFULLwidev2$ZuzuegeR.2009 +
                                                LORdataFULLwidev2$ZuzuegeR.2010 +
                                                LORdataFULLwidev2$ZuzuegeR.2011 +
                                                LORdataFULLwidev2$ZuzuegeR.2012)/6,digits=2)

# ---- Aussenwanderungen ----

#### ___ Aussenwanderungen ____Fortzuege ####
LORdataFULLwidev2$FortzuegeU        <-   (LORdataFULLwidev2$FortzuegeU.2007 +
                                           LORdataFULLwidev2$FortzuegeU.2008 +
                                           LORdataFULLwidev2$FortzuegeU.2009 +
                                           LORdataFULLwidev2$FortzuegeU.2010 +
                                           LORdataFULLwidev2$FortzuegeU.2011 +
                                           LORdataFULLwidev2$FortzuegeU.2012)
LORdataFULLwidev2$FortzuegeD        <-   (LORdataFULLwidev2$FortzuegeD.2007 +
                                            LORdataFULLwidev2$FortzuegeD.2008 +
                                            LORdataFULLwidev2$FortzuegeD.2009 +
                                            LORdataFULLwidev2$FortzuegeD.2010 +
                                            LORdataFULLwidev2$FortzuegeD.2011 +
                                            LORdataFULLwidev2$FortzuegeD.2012)
LORdataFULLwidev2$FortzuegeA        <-   (LORdataFULLwidev2$FortzuegeA.2007 +
                                            LORdataFULLwidev2$FortzuegeA.2008 +
                                            LORdataFULLwidev2$FortzuegeA.2009 +
                                            LORdataFULLwidev2$FortzuegeA.2010 +
                                            LORdataFULLwidev2$FortzuegeA.2011 +
                                            LORdataFULLwidev2$FortzuegeA.2012)
LORdataFULLwidev2$FortzuegeUD       <-   (LORdataFULLwidev2$FortzuegeUD.2007 +
                                            LORdataFULLwidev2$FortzuegeUD.2008 +
                                            LORdataFULLwidev2$FortzuegeUD.2009 +
                                            LORdataFULLwidev2$FortzuegeUD.2010 +
                                            LORdataFULLwidev2$FortzuegeUD.2011 +
                                            LORdataFULLwidev2$FortzuegeUD.2012)
LORdataFULLwidev2$FortzuegeDA       <-   (LORdataFULLwidev2$FortzuegeDA.2007 +
                                            LORdataFULLwidev2$FortzuegeDA.2008 +
                                            LORdataFULLwidev2$FortzuegeDA.2009 +
                                            LORdataFULLwidev2$FortzuegeDA.2010 +
                                            LORdataFULLwidev2$FortzuegeDA.2011 +
                                            LORdataFULLwidev2$FortzuegeDA.2012)
LORdataFULLwidev2$FortzuegeUDA      <-   (LORdataFULLwidev2$FortzuegeUDA.2007 +
                                            LORdataFULLwidev2$FortzuegeUDA.2008 +
                                            LORdataFULLwidev2$FortzuegeUDA.2009 +
                                            LORdataFULLwidev2$FortzuegeUDA.2010 +
                                            LORdataFULLwidev2$FortzuegeUDA.2011 +
                                            LORdataFULLwidev2$FortzuegeUDA.2012)

#### ___ Aussenwanderungen ____Zuzuege ####
LORdataFULLwidev2$ZuzuegeU        <-   (LORdataFULLwidev2$ZuzuegeU.2007 +
                                            LORdataFULLwidev2$ZuzuegeU.2008 +
                                            LORdataFULLwidev2$ZuzuegeU.2009 +
                                            LORdataFULLwidev2$ZuzuegeU.2010 +
                                            LORdataFULLwidev2$ZuzuegeU.2011 +
                                            LORdataFULLwidev2$ZuzuegeU.2012)
LORdataFULLwidev2$ZuzuegeD        <-   (LORdataFULLwidev2$ZuzuegeD.2007 +
                                            LORdataFULLwidev2$ZuzuegeD.2008 +
                                            LORdataFULLwidev2$ZuzuegeD.2009 +
                                            LORdataFULLwidev2$ZuzuegeD.2010 +
                                            LORdataFULLwidev2$ZuzuegeD.2011 +
                                            LORdataFULLwidev2$ZuzuegeD.2012)
LORdataFULLwidev2$ZuzuegeA        <-   (LORdataFULLwidev2$ZuzuegeA.2007 +
                                            LORdataFULLwidev2$ZuzuegeA.2008 +
                                            LORdataFULLwidev2$ZuzuegeA.2009 +
                                            LORdataFULLwidev2$ZuzuegeA.2010 +
                                            LORdataFULLwidev2$ZuzuegeA.2011 +
                                            LORdataFULLwidev2$ZuzuegeA.2012)
LORdataFULLwidev2$ZuzuegeUD       <-   (LORdataFULLwidev2$ZuzuegeUD.2007 +
                                            LORdataFULLwidev2$ZuzuegeUD.2008 +
                                            LORdataFULLwidev2$ZuzuegeUD.2009 +
                                            LORdataFULLwidev2$ZuzuegeUD.2010 +
                                            LORdataFULLwidev2$ZuzuegeUD.2011 +
                                            LORdataFULLwidev2$ZuzuegeUD.2012)
LORdataFULLwidev2$ZuzuegeDA       <-   (LORdataFULLwidev2$ZuzuegeDA.2007 +
                                            LORdataFULLwidev2$ZuzuegeDA.2008 +
                                            LORdataFULLwidev2$ZuzuegeDA.2009 +
                                            LORdataFULLwidev2$ZuzuegeDA.2010 +
                                            LORdataFULLwidev2$ZuzuegeDA.2011 +
                                            LORdataFULLwidev2$ZuzuegeDA.2012)
LORdataFULLwidev2$ZuzuegeUDA      <-   (LORdataFULLwidev2$ZuzuegeUDA.2007 +
                                            LORdataFULLwidev2$ZuzuegeUDA.2008 +
                                            LORdataFULLwidev2$ZuzuegeUDA.2009 +
                                            LORdataFULLwidev2$ZuzuegeUDA.2010 +
                                            LORdataFULLwidev2$ZuzuegeUDA.2011 +
                                            LORdataFULLwidev2$ZuzuegeUDA.2012)

#### ___ Aussenwanderungsquoten ____ Fortzuege ####
LORdataFULLwidev2$FortzuegeUR         <- round((LORdataFULLwidev2$FortzuegeUR.2007 +
                                               LORdataFULLwidev2$FortzuegeUR.2008 +
                                               LORdataFULLwidev2$FortzuegeUR.2009 +
                                               LORdataFULLwidev2$FortzuegeUR.2010 +
                                               LORdataFULLwidev2$FortzuegeUR.2011 +
                                               LORdataFULLwidev2$FortzuegeUR.2012)/6,digits=2)

LORdataFULLwidev2$FortzuegeDR         <- round((LORdataFULLwidev2$FortzuegeDR.2007 +
                                               LORdataFULLwidev2$FortzuegeDR.2008 +
                                               LORdataFULLwidev2$FortzuegeDR.2009 +
                                               LORdataFULLwidev2$FortzuegeDR.2010 +
                                               LORdataFULLwidev2$FortzuegeDR.2011 +
                                               LORdataFULLwidev2$FortzuegeDR.2012)/6,digits=2)

LORdataFULLwidev2$FortzuegeAR         <- round((LORdataFULLwidev2$FortzuegeAR.2007 +
                                               LORdataFULLwidev2$FortzuegeAR.2008 +
                                               LORdataFULLwidev2$FortzuegeAR.2009 +
                                               LORdataFULLwidev2$FortzuegeAR.2010 +
                                               LORdataFULLwidev2$FortzuegeAR.2011 +
                                               LORdataFULLwidev2$FortzuegeAR.2012)/6,digits=2)

LORdataFULLwidev2$FortzuegeUDR         <- round((LORdataFULLwidev2$FortzuegeUDR.2007 +
                                                LORdataFULLwidev2$FortzuegeUDR.2008 +
                                                LORdataFULLwidev2$FortzuegeUDR.2009 +
                                                LORdataFULLwidev2$FortzuegeUDR.2010 +
                                                LORdataFULLwidev2$FortzuegeUDR.2011 +
                                                LORdataFULLwidev2$FortzuegeUDR.2012)/6,digits=2)

LORdataFULLwidev2$FortzuegeDAR         <- round((LORdataFULLwidev2$FortzuegeDAR.2007 +
                                                LORdataFULLwidev2$FortzuegeDAR.2008 +
                                                LORdataFULLwidev2$FortzuegeDAR.2009 +
                                                LORdataFULLwidev2$FortzuegeDAR.2010 +
                                                LORdataFULLwidev2$FortzuegeDAR.2011 +
                                                LORdataFULLwidev2$FortzuegeDAR.2012)/6,digits=2)

LORdataFULLwidev2$FortzuegeUDAR         <- round((LORdataFULLwidev2$FortzuegeUDAR.2007 +
                                                 LORdataFULLwidev2$FortzuegeUDAR.2008 +
                                                 LORdataFULLwidev2$FortzuegeUDAR.2009 +
                                                 LORdataFULLwidev2$FortzuegeUDAR.2010 +
                                                 LORdataFULLwidev2$FortzuegeUDAR.2011 +
                                                 LORdataFULLwidev2$FortzuegeUDAR.2012)/6,digits=2)

#### ___ Aussenwanderungsquoten ____Zuzuege ####
LORdataFULLwidev2$ZuzuegeUR         <- round((LORdataFULLwidev2$ZuzuegeUR.2007 +
                                              LORdataFULLwidev2$ZuzuegeUR.2008 +
                                              LORdataFULLwidev2$ZuzuegeUR.2009 +
                                              LORdataFULLwidev2$ZuzuegeUR.2010 +
                                              LORdataFULLwidev2$ZuzuegeUR.2011 +
                                              LORdataFULLwidev2$ZuzuegeUR.2012)/6,digits=2)

LORdataFULLwidev2$ZuzuegeDR         <- round((LORdataFULLwidev2$ZuzuegeDR.2007 +
                                              LORdataFULLwidev2$ZuzuegeDR.2008 +
                                              LORdataFULLwidev2$ZuzuegeDR.2009 +
                                              LORdataFULLwidev2$ZuzuegeDR.2010 +
                                              LORdataFULLwidev2$ZuzuegeDR.2011 +
                                              LORdataFULLwidev2$ZuzuegeDR.2012)/6,digits=2)

LORdataFULLwidev2$ZuzuegeAR         <- round((LORdataFULLwidev2$ZuzuegeAR.2007 +
                                              LORdataFULLwidev2$ZuzuegeAR.2008 +
                                              LORdataFULLwidev2$ZuzuegeAR.2009 +
                                              LORdataFULLwidev2$ZuzuegeAR.2010 +
                                              LORdataFULLwidev2$ZuzuegeAR.2011 +
                                              LORdataFULLwidev2$ZuzuegeAR.2012)/6,digits=2)

LORdataFULLwidev2$ZuzuegeUDR         <- round((LORdataFULLwidev2$ZuzuegeUDR.2007 +
                                              LORdataFULLwidev2$ZuzuegeUDR.2008 +
                                              LORdataFULLwidev2$ZuzuegeUDR.2009 +
                                              LORdataFULLwidev2$ZuzuegeUDR.2010 +
                                              LORdataFULLwidev2$ZuzuegeUDR.2011 +
                                              LORdataFULLwidev2$ZuzuegeUDR.2012)/6,digits=2)

LORdataFULLwidev2$ZuzuegeDAR         <- round((LORdataFULLwidev2$ZuzuegeDAR.2007 +
                                              LORdataFULLwidev2$ZuzuegeDAR.2008 +
                                              LORdataFULLwidev2$ZuzuegeDAR.2009 +
                                              LORdataFULLwidev2$ZuzuegeDAR.2010 +
                                              LORdataFULLwidev2$ZuzuegeDAR.2011 +
                                              LORdataFULLwidev2$ZuzuegeDAR.2012)/6,digits=2)

LORdataFULLwidev2$ZuzuegeUDAR         <- round((LORdataFULLwidev2$ZuzuegeUDAR.2007 +
                                                LORdataFULLwidev2$ZuzuegeUDAR.2008 +
                                                LORdataFULLwidev2$ZuzuegeUDAR.2009 +
                                                LORdataFULLwidev2$ZuzuegeUDAR.2010 +
                                                LORdataFULLwidev2$ZuzuegeUDAR.2011 +
                                                LORdataFULLwidev2$ZuzuegeUDAR.2012)/6,digits=2)

LORdataFULLwidev2$OekoVerdraengungA <- LORdataFULLwidev2$Mietechgr*(0.135*6) 
LORdataFULLwidev2$OekoVerdraengungA[LORdataFULLwidev2$OekoVerdraengungA<0] <- 0

LORdataFULLwidev2$OekoVerdraengungB <- (LORdataFULLwidev2$Mietechgr*(0.135)/LORdataFULLwidev2$FortzuegeR)*100
LORdataFULLwidev2$OekoVerdraengungB[LORdataFULLwidev2$OekoVerdraengungB<0] <- 0


# ---- _____ Fertiger WIDE Datensatz _____ ----

names(LORdataFULLwidev2)
LORdataWIDE <- LORdataFULLwidev2
#names(LORdataWIDE)

###### c.) Merge LOR Shape file mit LOR Wide FULL Datensatz #########

source("/home/dao/Desktop/MasterArbeit/R_files/functions/merge_with_order_FUNCTION.R")
LORattrFULL   <- merge.with.order(LORdf, LORdataWIDE, sort=F,
                                  by.x="RAUMID", by.y="RAUMID",
                                  all.x=T, all.y=T,
                                  keep_order=1)
#View(LORattrFULL)
LOR@data <- LORattrFULL

#LOR@data$E_E.2012
#LOR@data$EWdichte.2012 <- (LOR@data$E_E.2012/LOR@data$FL_HA)*100
#library("sp")
#spplot(LOR, zcol="Miete.2012")

write.dbf(dataframe = LOR@data, file = "/home/dao/Desktop/MasterArbeit/GentriMap/4 Geodaten/LOR/LORinfo.dbf")

#View(LOR@data)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# III.) Ungültige LORs ausschliessen --> Datensätze für Kategorisierung und Regressionen erstellen ------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

LOR@data <- LORattrFULL

LOR@data$E_E_u300 <- as.factor(ifelse(LOR@data$E_E.2007 < 300, 
                                      c("unter 300EW"), 
                                      c("über  300EW")))
spplot(LOR, zcol="E_E_u300")

# Für die Regression und Kategorisierung arbeiten wir mit dem Datensatz LOR4reg, 
#    um im Originaldatensatz LOR keine Daten zu überschreiben 
LOR4reg <- LOR

# Wir überschreiben die Miete & Alosingkeit
# mit NAs, damit sie nicht bei der Kategorisierung und Regression mitverwendet werden
LOR4reg@data[LOR4reg@data$E_E_u300=="unter 300EW" | 
               LOR4reg@data$RAUMID_NAME=="Motardstr." | 
               LOR4reg@data$RAUMID_NAME=="Herzbergstraße" | # Motardstr. & Herzbergstraße raus wegen zu vielen Umzügen
               LOR4reg@data$RAUMID_NAME=="Blankenfelde" | # Blankenfelde raus weil Mietdaten fehlen für 2007 und 2008
               LOR4reg@data$RAUMID_NAME=="Blankenburg",][,c("Miete.2007", # Blankenburg raus weil Mietdaten fehlen für 2007 und 2008
                                                             "Miete.2012",
                                                             "Alose.2007",
                                                             "Alose.2012",
                                                             "nicht_Alose_Hartz.2007",
                                                             "nicht_Alose_Hartz.2012",
                                                             "Armut.2007",
                                                             "Armut.2012",
                                                             "Mietechg",
                                                             "Mietechgr",
                                                             "Alosechg",
                                                             "nicht_Alose_Hartzchg",
                                                             "Armutchg",
                                                             "Fortzuege",
                                                             "Zuzuege",
                                                             "FortzuegeR",
                                                             "ZuzuegeR",
                                                             "FortzuegeU", "FortzuegeD", "FortzuegeA",                 
                                                             "FortzuegeUD","FortzuegeUDA","FortzuegeDA",
                                                             "ZuzuegeU","ZuzuegeD","ZuzuegeA",
                                                             "ZuzuegeUD","ZuzuegeDA","ZuzuegeUDA",
                                                             "FortzuegeUR","FortzuegeDR","FortzuegeAR",
                                                             "FortzuegeUDR","FortzuegeDAR","FortzuegeUDAR",
                                                             "ZuzuegeUR","ZuzuegeDR","ZuzuegeAR",
                                                             "ZuzuegeUDR","ZuzuegeDAR","ZuzuegeUDAR")] <- NA
LOR4reg@data$valid <- as.factor(ifelse(is.na(LOR4reg@data$FortzuegeR), 
                                       c("ungültig"), 
                                       c("gültig")))

# nur gültige LORs behalten --> subset by valid=="gültig"
LOR4reg <- LOR4reg[LOR4reg$valid=="gültig",]
# update factor levels
LOR4reg@data$RAUMID      <- factor(LOR4reg@data$RAUMID)
LOR4reg@data$RAUMID_NAME <- factor(LOR4reg@data$RAUMID_NAME)
LOR4reg@data$BZR         <- factor(LOR4reg@data$BZR)
LOR4reg@data$BZR_NAME    <- factor(LOR4reg@data$BZR_NAME )
# Shape file für LOR4reg erstellen
LORshape4reg <- SpatialPolygons(LOR4reg@polygons,proj4string=zielCRS)
plot(LORshape4reg)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# IV.) R Global Environment ausmisten - überflüssige Objekte löschen ------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

remove(bloecke07,                     bloecke07_attributes,          bloecke07_pt,                 
       bloecke07_ptdf,                bloecke08,                     bloecke08_attributes,         
       bloecke08_pt,                  bloecke08_ptdf,                bloecke09,                    
       bloecke09_attributes,          bloecke09_pt,                  bloecke09_ptdf,               
       bloecke10_12,                  bloecke10_12_pt,               bloecke10_12_ptdf,            
       bloecke2LOR_07,                bloecke2LOR_08,                bloecke2LOR_09,               
       bloecke2LOR_10_12,             bloeckePLZ07_ptdf,             bloeckePLZ08_ptdf,            
       bloeckePLZ09_ptdf,             bloeckePLZ10_12_ptdf,
       DF1,                           DF1b,                         
       DF2,                           DF3,                           DF4,                          
       DF5,                           DF6,                           DF6a,                    
       DF7,                           DF8,                           DF9,
       EW_07, EW_07_raw,
       ODdf_v7,
       PLZ2007, PLZ2008, PLZ2009, PLZ2010, PLZ2011, PLZ2012, PLZ2010_2012)
