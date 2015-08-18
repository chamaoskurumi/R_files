#################
# Daten für NICEONE
#################

load("/home/dao/Desktop/MasterArbeit/R_files/KNITR/FINAL_WORKSPACE.Rdata")

MieteNAMES <- paste("Miete", 2007:2012, sep=".")
ArmutNAMES <- paste("Armut", 2007:2012, sep=".")
FortzuegeRNAMES    <- paste("FortzuegeR", 2007:2012, sep=".")
ZuzuegeRNAMES      <- paste("ZuzuegeR", 2007:2012, sep=".")
FortzuegeUDARNAMES <- paste("FortzuegeUDAR", 2007:2012, sep=".")
ZuzuegeUDARNAMES   <- paste("ZuzuegeUDAR", 2007:2012, sep=".")
E_18U35RNAMES      <- paste("E_18U35R", 2007:2012, sep=".")

WIDE4NICEONE <- LOR4reg@data[c("RAUMID","RAUMID_NAME", MieteNAMES,ArmutNAMES,
                               FortzuegeRNAMES,ZuzuegeRNAMES,
                               FortzuegeUDARNAMES,ZuzuegeUDARNAMES,
                               E_18U35RNAMES,
                               "Mietechgr","Armutchg",
                               "FortzuegeR","FortzuegeUDAR",
                               "ZuzuegeR","ZuzuegeUDAR",
                               "OekoVerdraengungA","OekoVerdraengungB",
                               "Gentri",
                               "StaedtWohnungen.2012",
                               "AlleinerzHH.2012")]

WIDE4NICEONE$OekoVerdraengungA <- round(WIDE4NICEONE$OekoVerdraengungA,2)
WIDE4NICEONE$OekoVerdraengungB <- round(WIDE4NICEONE$OekoVerdraengungB,2)

MieteNAMESn <- paste("Miete", 2007:2012, sep="_")
ArmutNAMESn <- paste("Armut", 2007:2012, sep="_")
FortzuegeRNAMESn    <- paste("FortzuegeR", 2007:2012, sep="_")
ZuzuegeRNAMESn      <- paste("ZuzuegeR", 2007:2012, sep="_")
FortzuegeUDARNAMESn <- paste("FortzuegeUDAR", 2007:2012, sep="_")
ZuzuegeUDARNAMESn   <- paste("ZuzuegeUDAR", 2007:2012, sep="_")
E_18U35RNAMESn     <- paste("E18U35R", 2007:2012, sep="_")

colnames(WIDE4NICEONE) <- c("RAUMID","RAUMID_NAME", MieteNAMESn,ArmutNAMESn,
                            FortzuegeRNAMESn,ZuzuegeRNAMESn,
                            FortzuegeUDARNAMESn,ZuzuegeUDARNAMESn,
                            E_18U35RNAMESn,
                            "Mietechgr","Armutchg",
                            "FortzuegeR","FortzuegeUDAR",
                            "ZuzuegeR","ZuzuegeUDAR",
                            "OekoVerdraengungA","OekoVerdraengungB",
                            "Gentri",
                            "StaedtWohnungen2012",
                            "AlleinerzHH2012")
names(WIDE4NICEONE)

write.csv(WIDE4NICEONE, "/home/dao/Dropbox/Daten4Christian/WIDE4NICEONE.csv")


