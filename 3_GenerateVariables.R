#********************************************
#                                           #
#   Variablengenerierung für Analyse        #
#                                           #
#********************************************


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Zusätzliche Variablen erstellen im LOR FULL long Datensatz ------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

names(LORdataFULL)
#View(LORdataFULL)

LORdataFULL$Miete         <- (LORdataFULL$Miete_H1_wmean+LORdataFULL$Miete_H2_wmean)/2

LORdataFULL$E_AR         <- round(( LORdataFULL$E_A        / LORdataFULL$E_E )*100,digits=1)
LORdataFULL$HK_TurkR     <- round(( LORdataFULL$HK_Turk    / LORdataFULL$E_E )*100,digits=1)
LORdataFULL$HK_ArabR     <- round(( LORdataFULL$HK_Arab    / LORdataFULL$E_E )*100,digits=1)
LORdataFULL$HK_EU15R     <- round(( LORdataFULL$HK_EU15    / LORdataFULL$E_E )*100,digits=1)
LORdataFULL$HK_EU27R     <- round(( LORdataFULL$HK_EU27    / LORdataFULL$E_E )*100,digits=1)
LORdataFULL$HK_EheJugR   <- round(( LORdataFULL$HK_EheJug  / LORdataFULL$E_E )*100,digits=1)
LORdataFULL$HK_EheSUR    <- round(( LORdataFULL$HK_EheSU   / LORdataFULL$E_E )*100,digits=1)

# schleife zur ---R generierung der variablen
"E_U1",     "E_1U6",    "E_6U15",  
"E_15U18",  "E_18U25",  "E_25U55",  "E_55U65",  "E_65U80", 
"E_80U110", "E_A",      "E_AU1",    "E_A1U6",   "E_A6U15", 
"E_A15U18", "E_A18U25", "E_A25U55", "E_A55U65", "E_A65U80",
"E_A80U110","MH_E",     "HK_EU15",  "HK_EU27",  "HK_Polen",
"HK_EheJug","HK_EheSU", "HK_Turk",  "HK_Arab",  "HK_Sonst"  

dim(LORdata)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Zusätzliche Variablen erstellen im LOR FULL wide Datensatz ------
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