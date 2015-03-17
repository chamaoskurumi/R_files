#********************************************
#                                           #
#   Variablengenerierung für Analyse        #
#                                           #
#********************************************

library("plyr")
library("reshape")
library("reshape2")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOR long Datensatz ------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

LORdataFULLv2 <- LORdataFULLv1

#names(LORdataFULLv2)
View(LORdataFULLv2)

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
View(LORdataFULLv2ORD) 

LORdataFULLv2ORD <- data.frame(LORdataFULLv2ORD,Miete2merge$Miete)
colnames(LORdataFULLv2ORD)[dim(LORdataFULLv2ORD)[2]] <- "Miete"

LORdataFULLv2 <- LORdataFULLv2ORD[order(LORdataFULLv2ORD[,"RAUMID"],LORdataFULLv2ORD[,"ZEIT"]), ]
View(LORdataFULLv2)

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

###### g.) Wohndauer ######

LORdataFULLv2              <- ddply(LORdataFULLv2,"RAUMID", transform,
                                    PDAU10chg = c(NA,diff(PDAU10)))
LORdataFULLv2              <- ddply(LORdataFULLv2,"RAUMID", transform,
                                    PDAU5chg  = c(NA,diff(PDAU5)))


###### h.) Unnötige Vars droppen & Var order ändern ######

names(LORdataFULLv2)
LORdataFULLv3 <- subset(LORdataFULLv2, select=-c(Miete_H1_wmean,
                                                Miete_H2_wmean,
                                                HK_NZOrd,
                                                WLNZORD,
                                                EinfWhnlageLaerm,
                                                MigHinter_u18,
                                                E_U1, E_1U6, E_6U15, E_15U18, E_18U25, E_25U55, E_55U65, E_65U80, E_80U110,
                                                E_U1R, E_1U6R, E_6U15R, E_15U18R, E_18U25R, E_25U55R, E_55U65R, E_65U80R, E_80U110R,  
                                                E_AU1, E_A1U6, E_A6U15, E_A15U18, E_A18U25, E_A25U55, E_A55U65, E_A65U80, E_A80U110,
                                                E_AU1R, E_A1U6R, E_A6U15R, E_A15U18R, E_A18U25R, E_A25U55R, E_A55U65R, E_A65U80R, E_A80U110R, 
                                                MH_U1, MH_1U6, MH_6U15, MH_15U18, MH_18U25, MH_25U55, MH_55U65, MH_65U80, MH_80U110,
                                                MH_U1R, MH_1U6R, MH_6U15R, MH_15U18R, MH_18U25R, MH_25U55R, MH_55U65R, MH_65U80R, MH_80U110R))
names(LORdataFULLv3)

LORdataFULLv4 <- LORdataFULLv3[c("ZEIT", 
               # ID Variablen
               "RAUMID",     "RAUMID_NAME",
               "BZR",        "BZR_NAME", 
               "PGR",        "PRG_NAME",
               "BEZ",        "BEZ_NAME",       
               "STADTRAUM",  "FL_HA",
               # Gesamteinwohner
               "E_E",        
               "E_U18",      "E_18U65",     "E_65U110",
               "E_U18R",     "E_18U65R",    "E_65U110R",
               # Ausländer
               "E_A",         "E_AR",       
               "E_AU18",      "E_A18U65",      "E_A65U110",
               "E_AU18R",     "E_A18U65R",     "E_A65U110R",
               "E_AU18RU18",  "E_A18U65R18U65","E_A65U110R65U110", 
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
               "WLEINFOL",   "WLEINFML",   
               "WLMITOL",    "WLMITML",   
               "WLGUTOL",    "WLGUTML",    
               "WLEINFOLR",  "WLEINFMLR",
               "WLMITOLR",   "WLMITMLR",
               "WLGUTOLR",   "WLGUTMLR",
               "WLOL",       "WLML",                  
               "WLOLR",      "WLMLR",  
               "WLEINF",     "WLMIT",      "WLGUT",
               "WLEINFR",    "WLMITR",     "WLGUTR",
               # Sozialindikatoren
               "Alose",      "Alose_u25",  "Alose_langzeit", "nicht_Alose_Hartz", "Hartz_u15", 
               "Veraend_HartzEmpf_D",    "Veraend_HartzEmpf_Ausl",  "Veraend_Hartz_u15", "StaedtWohnungen",
               "AlleinerzHH",            "Altersarmut", 
               # Wanderdaten
               "WanderVol",       "WanderSaldo",       "WanderSaldo_u6",    
               # SanierungsGebiete
               "SanGebiet",       "SanGebiet_NAME",    "SanGebiet_KLASSE",       
               # Mietdaten
               "Miete", "Mietechg", "Mietechgr", "MieteNom")]

remove(LORdataFULLv2)
remove(LORdataFULLv3)

LORdataFULL <- LORdataFULLv4
remove(LORdataFULLv4)
head(LORdataFULL)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOR wide Datensatz ------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

###### a.) Reshape long to wide ######

LORdataFULL4wide     <- arrange(LORdataFULL, RAUMID, ZEIT)
LORdataFULLwidev1    <- reshape(LORdataFULL4wide,
                            idvar = c("RAUMID",  "RAUMID_NAME", "BZR",
                             "BZR_NAME","PGR",     "PRG_NAME","BEZ",    
                             "BEZ_NAME","STADTRAUM","FL_HA"),
                            v.names = c(
                              # Gesamteinwohner
                              "E_E",        
                              "E_U18",      "E_18U65",     "E_65U110",
                              "E_U18R",     "E_18U65R",    "E_65U110R",
                              # Ausländer
                              "E_A",         "E_AR",       
                              "E_AU18",      "E_A18U65",      "E_A65U110",
                              "E_AU18R",     "E_A18U65R",     "E_A65U110R",
                              "E_AU18RU18",  "E_A18U65R18U65","E_A65U110R65U110", 
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
                              "WLEINFOL",   "WLEINFML",   
                              "WLMITOL",    "WLMITML",   
                              "WLGUTOL",    "WLGUTML",    
                              "WLEINFOLR",  "WLEINFMLR",
                              "WLMITOLR",   "WLMITMLR",
                              "WLGUTOLR",   "WLGUTMLR",
                              "WLOL",       "WLML",                  
                              "WLOLR",      "WLMLR",  
                              "WLEINF",     "WLMIT",      "WLGUT",
                              "WLEINFR",    "WLMITR",     "WLGUTR",
                              # Sozialindikatoren
                              "Alose",      "Alose_u25",  "Alose_langzeit", "nicht_Alose_Hartz", "Hartz_u15", 
                              "Veraend_HartzEmpf_D",    "Veraend_HartzEmpf_Ausl",  "Veraend_Hartz_u15", "StaedtWohnungen",
                              "AlleinerzHH",            "Altersarmut",  
                              # Wanderdaten
                              "WanderVol",       "WanderSaldo",       "WanderSaldo_u6",    
                              # SanierungsGebiete
                              "SanGebiet",       "SanGebiet_NAME",    "SanGebiet_KLASSE",       
                              # Mietdaten
                              "Miete", "Mietechg", "Mietechgr", "MieteNom"),
                   timevar = "ZEIT",
                   direction = "wide") 
#names(LORdataFULLwidev1)

###### b.) Generierung von speziellen WIDE Variablen #####

LORdataFULLwidev2   <- LORdataFULLwidev1
#names(LORdataFULLwidev2)

# Rentner 
LORdataFULLwidev2$E_65U110chg       <- LORdataFULLwidev2$E_65U110.2007-  LORdataFULLwidev2$E_65U110.2012
LORdataFULLwidev2$E_65U110Rchg      <- LORdataFULLwidev2$E_65U110R.2007-  LORdataFULLwidev2$E_65U110R.2012

# Änderung der Wohndauer
LORdataFULLwidev2$DAU5chg        <- LORdataFULLwidev2$DAU5.2007-  LORdataFULLwidev2$DAU5.2012
LORdataFULLwidev2$DAU10chg       <- LORdataFULLwidev2$DAU10.2007- LORdataFULLwidev2$DAU10.2012
LORdataFULLwidev2$PDAU5chg       <- LORdataFULLwidev2$PDAU5.2007- LORdataFULLwidev2$PDAU5.2012
LORdataFULLwidev2$PDAU10chg      <- LORdataFULLwidev2$PDAU10.2007-LORdataFULLwidev2$PDAU10.2012

# Änderung Wohnlage
LORdataFULLwidev2$WLEINFchg        <- LORdataFULLwidev2$WLEINF.2007-LORdataFULLwidev2$WLEINF.2012
LORdataFULLwidev2$WLMITchg         <- LORdataFULLwidev2$WLMIT.2007-LORdataFULLwidev2$WLMIT.2012
LORdataFULLwidev2$WLGUTchg         <- LORdataFULLwidev2$WLGUT.2007-LORdataFULLwidev2$WLGUT.2012

LORdataFULLwidev2$WLEINFRchg        <- LORdataFULLwidev2$WLEINFR.2007-LORdataFULLwidev2$WLEINFR.2012
LORdataFULLwidev2$WLMITRchg         <- LORdataFULLwidev2$WLMITR.2007-LORdataFULLwidev2$WLMITR.2012
LORdataFULLwidev2$WLGUTRchg         <- LORdataFULLwidev2$WLGUTR.2007-LORdataFULLwidev2$WLGUTR.2012

# Änderung Arbeitslosigkeit
LORdataFULLwidev2$Alosechg          <- LORdataFULLwidev2$Alose.2007 -            LORdataFULLwidev2$Alose.2012
LORdataFULLwidev2$Alose_langzeitchg <- LORdataFULLwidev2$Alose_langzeit.2007  -  LORdataFULLwidev2$Alose_langzeit.2012
LORdataFULLwidev2$Alose_u25chg      <- LORdataFULLwidev2$Alose_u25.2007 -        LORdataFULLwidev2$Alose_u25.2012
LORdataFULLwidev2$Hartz_u15chg      <- LORdataFULLwidev2$Hartz_u15.2007 -        LORdataFULLwidev2$Hartz_u15.2012

# Änderung Ausländeranteil
LORdataFULLwidev2$MH_Echg           <-  LORdataFULLwidev2$MH_E.2007 -            LORdataFULLwidev2$MH_E.2012
LORdataFULLwidev2$MH_ERchg          <-  LORdataFULLwidev2$MH_ER.2007 -           LORdataFULLwidev2$MH_ER.2012

LORdataFULLwidev2$MH_U18chg           <-  LORdataFULLwidev2$MH_U18.2007 -           LORdataFULLwidev2$MH_U18.2012
LORdataFULLwidev2$MH_U18RU18chg       <-  LORdataFULLwidev2$MH_U18RU18.2007 -       LORdataFULLwidev2$MH_U18RU18.2012
LORdataFULLwidev2$MH_65U110R65U110chg <-  LORdataFULLwidev2$MH_65U110R65U110.2007-  LORdataFULLwidev2$MH_65U110R65U110.2012

LORdataFULLwidev2$HK_EU15chg        <-  LORdataFULLwidev2$HK_EU15.2007 -         LORdataFULLwidev2$HK_EU15.2012
LORdataFULLwidev2$HK_EU27hg         <-  LORdataFULLwidev2$HK_EU27.2007 -         LORdataFULLwidev2$HK_EU27.2012
LORdataFULLwidev2$HK_Turkchg        <-  LORdataFULLwidev2$HK_Turk.2007 -         LORdataFULLwidev2$HK_Turk.2012
LORdataFULLwidev2$HK_Arabchg        <-  LORdataFULLwidev2$HK_Arab.2007 -         LORdataFULLwidev2$HK_Arab.2012
LORdataFULLwidev2$HK_EheJugchg      <-  LORdataFULLwidev2$HK_EheJug.2007 -       LORdataFULLwidev2$HK_EheJug.2012

LORdataFULLwidev2$HK_EU15Rchg        <-  LORdataFULLwidev2$HK_EU15R.2007 -         LORdataFULLwidev2$HK_EU15R.2012
LORdataFULLwidev2$HK_EU27Rhg         <-  LORdataFULLwidev2$HK_EU27R.2007 -         LORdataFULLwidev2$HK_EU27R.2012
LORdataFULLwidev2$HK_TurkRchg        <-  LORdataFULLwidev2$HK_TurkR.2007 -         LORdataFULLwidev2$HK_TurkR.2012
LORdataFULLwidev2$HK_ArabRchg        <-  LORdataFULLwidev2$HK_ArabR.2007 -         LORdataFULLwidev2$HK_ArabR.2012
LORdataFULLwidev2$HK_EheJugRchg      <-  LORdataFULLwidev2$HK_EheJugR.2007 -       LORdataFULLwidev2$HK_EheJugR.2012

LORdataFULLwidev2$HK_EU15RMHchg        <-  LORdataFULLwidev2$HK_EU15RMH.2007 -         LORdataFULLwidev2$HK_EU15RMH.2012
LORdataFULLwidev2$HK_EU27RMHhg         <-  LORdataFULLwidev2$HK_EU27RMH.2007 -         LORdataFULLwidev2$HK_EU27RMH.2012
LORdataFULLwidev2$HK_TurkRMHchg        <-  LORdataFULLwidev2$HK_TurkRMH.2007 -         LORdataFULLwidev2$HK_TurkRMH.2012
LORdataFULLwidev2$HK_ArabRMHchg        <-  LORdataFULLwidev2$HK_ArabRMH.2007 -         LORdataFULLwidev2$HK_ArabRMH.2012
LORdataFULLwidev2$HK_EheJugRMHchg      <-  LORdataFULLwidev2$HK_EheJugRMH.2007 -       LORdataFULLwidev2$HK_EheJugRMH.2012

# Wanderungssaldo Summe in Prozent (ungenau)
LORdataFULLwidev2$WanderSaldosum <-   (LORdataFULLwidev2$WanderSaldo.2007 +
                                       LORdataFULLwidev2$WanderSaldo.2008 +
                                       LORdataFULLwidev2$WanderSaldo.2009 +
                                       LORdataFULLwidev2$WanderSaldo.2010 +
                                       LORdataFULLwidev2$WanderSaldo.2011 +
                                       LORdataFULLwidev2$WanderSaldo.2012)

# Mietpreisänderung
LORdataFULLwidev2$Mietechg    <-   LORdataFULLwidev2$Miete.2012 - LORdataFULLwidev2$Miete.2007
LORdataFULLwidev2$Mietechgr   <-   round(((LORdataFULLwidev2$Miete.2012 - LORdataFULLwidev2$Miete.2007)/
                                            LORdataFULLwidev2$Miete.2007)*100, digits=0)
LORdataFULLwidev2$MieteNomchg <-   LORdataFULLwidev2$MieteNom.2012 - LORdataFULLwidev2$MieteNom.2007
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

library(foreign)
write.dbf(dataframe = LOR@data, file = "/home/dao/Desktop/MasterArbeit/GentriMap/4 Geodaten/LOR/LORinfo.dbf")

#View(LOR@data)

