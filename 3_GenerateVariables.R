#********************************************
#                                           #
#   Variablengenerierung für Analyse        #
#                                           #
#********************************************

library(plyr)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOR long Datensatz ------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

LORdataFULLv2 <- LORdataFULLv1

names(LORdataFULLv2)
#View(LORdataFULLv2)

###### a.) Miete #####

LORdataFULLv2$Miete        <- round(( LORdataFULLv2$Miete_H1_wmean+LORdataFULLv2$Miete_H2_wmean)/2, digits=2)
LORdataFULLv2              <- ddply(LORdataFULLv2,"RAUMID", transform,
                                  Mietechg = c(NA,diff(Miete)))
LORdataFULLv2              <- ddply(LORdataFULLv2,"RAUMID", transform,
                                  Mietechgr = c(NA,exp(diff(log(Miete)))-1))
LORdataFULLv2$Mietechgr    <- round((LORdataFULLv2$Mietechgr*100), digits=0)

###### b.) Altergruppen Inländer ######

LORdataFULLv2$E_U18         <- (LORdataFULLv2$E_U1    +
                                LORdataFULLv2$E_1U6   +
                                LORdataFULLv2$E_6U15  +
                                LORdataFULLv2$E_15U18)
LORdataFULLv2$E_18U65       <- (LORdataFULLv2$E_18U55 +
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
LORdataFULLv2$E_18U65R      <- round(( LORdataFULLv2$E_18U65    / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$E_65U110R     <- round(( LORdataFULLv2$E_65U110   / LORdataFULLv2$E_E )*100,digits=1)

###### c.) Altergruppen Ausländer ######

LORdataFULLv2$E_AU18         <- (LORdataFULLv2$E_AU1    +
                                  LORdataFULLv2$E_A1U6   +
                                  LORdataFULLv2$E_A6U15  +
                                  LORdataFULLv2$E_A15U18)
LORdataFULLv2$E_A18U65       <- (LORdataFULLv2$E_A18U55 +
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

###### d.) Ausländeranteil & Migrationshintergrund ######

LORdataFULLv2$E_AR         <- round(( LORdataFULLv2$E_A        / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$HK_TurkR     <- round(( LORdataFULLv2$HK_Turk    / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$HK_ArabR     <- round(( LORdataFULLv2$HK_Arab    / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$HK_EU15R     <- round(( LORdataFULLv2$HK_EU15    / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$HK_EU27R     <- round(( LORdataFULLv2$HK_EU27    / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$HK_PolenR    <- round(( LORdataFULLv2$HK_Polen   / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$HK_EheJugR   <- round(( LORdataFULLv2$HK_EheJug  / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$HK_EheSUR    <- round(( LORdataFULLv2$HK_EheSU   / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$HK_SonstR    <- round(( LORdataFULLv2$HK_Sonst   / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$MH_ER        <- round(( LORdataFULLv2$MH_E       / LORdataFULLv2$E_E )*100,digits=1)

###### e.) Wohnlage ######

LORdataFULLv2$WLOL    <- (LORdataFULLv2$WLEINFOL + 
                          LORdataFULLv2$WLMITOL  +
                          LORdataFULLv2$WLGUTOL) 
LORdataFULLv2$WLML    <- (LORdataFULLv2$WLEINFML + 
                          LORdataFULLv2$WLMITML  +
                          LORdataFULLv2$WLGUTML) 

LORdataFULLv2$WLEINF    <- (LORdataFULLv2$WLEINFOL + LORdataFULLv2$WLEINFML) 
LORdataFULLv2$WLMIT     <- (LORdataFULLv2$WLMITOL  + LORdataFULLv2$WLMITML)        
LORdataFULLv2$WLGUT     <- (LORdataFULLv2$WLGUTOL  + LORdataFULLv2$WLGUTFML)     

LORdataFULLv2$WLEINFOLR     <- round(( LORdataFULLv2$WLEINFOL  / LORdataFULLv2$E_E )*100,digits=1)          
LORdataFULLv2$WLEINFMLR     <- round(( LORdataFULLv2$WLEINFML  / LORdataFULLv2$E_E )*100,digits=1)   
LORdataFULLv2$WLMITOLR      <- round(( LORdataFULLv2$WLMITOL   / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$WLMITMLR      <- round(( LORdataFULLv2$WLMITML   / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$WLGUTOLR      <- round(( LORdataFULLv2$WLGUTOL   / LORdataFULLv2$E_E )*100,digits=1)
LORdataFULLv2$WLGUTMLR      <- round(( LORdataFULLv2$WLGUTML   / LORdataFULLv2$E_E )*100,digits=1)
#LORdataFULLv2$WLNZORDR      <- round(( LORdataFULLv2$WLNZORD   / LORdataFULLv2$E_E )*100,digits=1)      

LORdataFULLv2$WLNZORDR      <- round(( LORdataFULLv2$WLNZORD   / LORdataFULLv2$E_E )*100,digits=1)    
LORdataFULLv2$WLNZORDR      <- round(( LORdataFULLv2$WLNZORD   / LORdataFULLv2$E_E )*100,digits=1)    

LORdataFULLv2$WLEINFR     <- round(( LORdataFULLv2$WLEINF      / LORdataFULLv2$E_E )*100,digits=1)    
LORdataFULLv2$WMITR       <- round(( LORdataFULLv2$WLMIT       / LORdataFULLv2$E_E )*100,digits=1)    
LORdataFULLv2$WGUTR       <- round(( LORdataFULLv2$WLGUT       / LORdataFULLv2$E_E )*100,digits=1)    

###### f.) Unnötige Vars droppen & Var order ändern ######

LORdataFULLv2 <- subset(LORdataFULLv2, select=-c(Miete_H1_wmean,
                                                Miete_H2_wmean,
                                                HK_NZOrd,
                                                WLNZORD,
                                                EinfWhnlageLaerm,
                                                MigHinter_u18))
names(LORdataFULLv2)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOR wide Datensatz ------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ExDF                <- LORattrFULLwide4shape
names(ExDF)
# Änderung der Wohndauer
ExDF$PDAU5chg       <- ExDF$PDAU5.2008-ExDF$PDAU5.2013
ExDF$PDAU10chg      <- ExDF$PDAU10.2008-ExDF$PDAU10.2013
# Mietniveau 2013 mitteln 
ExDF$Miete.2013     <- (ExDF$Miete_H1_wmean.2013 + ExDF$Miete_H2_wmean.2013)/2
# Mietpreisänderung
ExDF$MIETE_H1chg    <- ExDF$Miete_H1_wmean.2013 - ExDF$Miete_H1_wmean.2008
ExDF$MIETE_H1chgr   <- ((ExDF$Miete_H1_wmean.2013 - ExDF$Miete_H1_wmean.2008)/ExDF$Miete_H1_wmean.2008)*100
ExDF$MIETE_H2chg    <- ExDF$Miete_H2_wmean.2013 - ExDF$Miete_H2_wmean.2008
ExDF$MIETE_H2chgr   <- ((ExDF$Miete_H2_wmean.2013 - ExDF$Miete_H2_wmean.2008)/ExDF$Miete_H2_wmean.2008)*100
ExDF$MIETE_chg      <- (ExDF$MIETE_H1chg+ExDF$MIETE_H2chg)/2
ExDF$MIETE_chgr     <- round((ExDF$MIETE_H1chgr+ExDF$MIETE_H2chgr)/2,digits=0)
# Änderung Arbeitslosigkeit
ExDF$Alosechg      <- ExDF$Alose.2013 - ExDF$Alose.2008
ExDF$Alose_u25chg  <- ExDF$Alose_u25.2013 - ExDF$Alose_u25.2008
ExDF$Alose_Hartzchg<-ExDF$Alose_Hartz.2013 - ExDF$Alose_Hartz.2008
ExDF$Hartz_u15chg  <- ExDF$Hartz_u15.2013 - ExDF$Hartz_u15.2008
# Ausländeranteil
ExDF$E_Ar.2013        <- round((ExDF$E_A.2013       /ExDF$E_E.2013)*100,digits=1)
ExDF$HK_Turkr.2013    <- round((ExDF$HK_Turk.2013   /ExDF$E_E.2013)*100,digits=1)
ExDF$HK_Arabr.2013    <- round((ExDF$HK_Arab.2013   /ExDF$E_E.2013)*100,digits=1)
ExDF$HK_EU15r.2013    <- round((ExDF$HK_EU15.2013   /ExDF$E_E.2013)*100,digits=1)
ExDF$HK_EU27r.2013    <- round((ExDF$HK_EU27.2013   /ExDF$E_E.2013)*100,digits=1)
ExDF$HK_EheJugr.2013  <- round((ExDF$HK_EheJug.2013 /ExDF$E_E.2013)*100,digits=1)
ExDF$HK_EheSUr.2013   <- round((ExDF$HK_EheSU.2013  /ExDF$E_E.2013)*100,digits=1)
# Änderung Ausländeranteil
ExDF$E_Achgr        <- round((((ExDF$E_A.2013         /ExDF$E_E.2013)*100) -  ((ExDF$E_A.2008       /ExDF$E_E.2008)*100)),digits=1)
ExDF$HK_Turkchgr    <- round((((ExDF$HK_Turk.2013     /ExDF$E_E.2013)*100) -  ((ExDF$HK_Turk.2008   /ExDF$E_E.2008)*100)),digits=1)
ExDF$HK_Arabchgr    <- round((((ExDF$HK_Arab.2013     /ExDF$E_E.2013)*100) -  ((ExDF$HK_Arab.2008   /ExDF$E_E.2008)*100)),digits=1)
ExDF$HK_EU15chgr    <- round((((ExDF$HK_EU15.2013     /ExDF$E_E.2013)*100) -  ((ExDF$HK_EU15.2008   /ExDF$E_E.2008)*100)),digits=1)
ExDF$HK_EU27chgr    <- round((((ExDF$HK_EU27.2013     /ExDF$E_E.2013)*100) -  ((ExDF$HK_EU27.2008   /ExDF$E_E.2008)*100)),digits=1)
ExDF$HK_EheJugchgr  <- round((((ExDF$HK_EheJug.2013   /ExDF$E_E.2013)*100) -  ((ExDF$HK_EheJug.2008 /ExDF$E_E.2008)*100)),digits=1)
ExDF$HK_EheSUchgr   <- round((((ExDF$HK_EheSU.2013    /ExDF$E_E.2013)*100) -  ((ExDF$HK_EheSU.2008  /ExDF$E_E.2008)*100)),digits=1)
names(ExDF)