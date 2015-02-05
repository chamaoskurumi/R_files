##################################################
# LORinfo File Grundlagen schreiben
##################################################

LORinfo <- read.dbf("/home/dao/Desktop/temp.dbf")
LORinfo <- subset(LORinfo, select=-c(gml_id,spatial_ty))
LORinfo <- subset(LORinfo, !is.na(spatial_na))
colnames(LORinfo)[1] <- "SCHLUESSEL"
colnames(LORinfo)[6] <- "BEZIRK_NAME"
LORinfo <- subset(LORinfo, select=-c(spatial_al))
View(LORinfo)

EW2013info <- subset(EW, ZEIT==2013)
EW2013info <- subset(EW2013info, select=c(RAUMID, BEZ, PGR, BZR, PLR, STADTRAUM, E_E))
as.factor(EW2013info[,1]) -> EW2013info[,1]
EW2013info$E_E <- as.numeric(gsub(",","", EW2013info$E_E))
(as.numeric(EW2013info$E_E)/100) -> EW2013info$E_E
colnames(EW2013info)[1] <- "SCHLUESSEL"
colnames(EW2013info)[2] <- "BEZIRK"
colnames(EW2013info)[7] <- "EW2013"
str(EW2013info)

identical(EW2013info$SCHLUESSEL, LORinfo$SCHLUESSEL)

as.numeric(as.character(LORinfo$SCHLUESSEL))
formatC(EW2013info$SCHLUESSEL, width=8, format="d", flag="0")

LORinfoFULL <- (data.frame(cbind(LORinfo, EW2013info)))
LORinfoFULL <- subset(LORinfoFULL, select=-c(PLR, SCHLUESSEL.1))

LORinfoFULL2 <- data.frame(LORinfoFULL$SCHLUESSEL, LORinfoFULL$PLR_NAME, LORinfoFULL$BZR, LORinfoFULL$BZR_NAME, LORinfoFULL$PGR, LORinfoFULL$PRG_NAME, LORinfoFULL$BEZIRK,
                           LORinfoFULL$BEZIRK_NAME, LORinfoFULL$STADTRAUM, LORinfoFULL$FL_HA, LORinfoFULL$EW2013)

colnames(LORinfoFULL2) <- c("SCHLUESSEL","PLR_NAME","BZR","BZR_NAME", "PGR", "PGR_NAME", 
                            "BEZIRK", "BEZIRK_NAME", "STADTRAUM", "FL_HA", "EW2013")

View(LORinfoFULL2)

LOR@data <- merge(LOR@data, LORinfo)
write.dbf(dataframe = LORinfo, file = "/home/dao/Desktop/MasterArbeit/GentriMap/4 Geodaten/LOR/LORinfo.dbf")
