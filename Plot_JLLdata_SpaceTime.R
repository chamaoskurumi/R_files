library("spacetime")

JLLdata$Miete <- (JLLdata$Miete_H1 + JLLdata$Miete_H2)/2
yrs <- 2004:2014
JLLtime <- as.POSIXct(paste(yrs, "-01-01", sep = ""), tz = "GMT")
PLZslim <- PLZ
source("/home/dao/Desktop/MasterArbeit/R_files/functions/merge_with_order_FUNCTION.R")
PLZslim_df             <- as(PLZslim, "data.frame")
JLLdata4PLZ_04_14      <- merge.with.order(PLZslim_df, JLLdata, 
                                                 by.x="PLZ", by.y="PLZ", 
                                                 all.x=T, all.y=F,
                                                 sort=F, keep_order=1)
JLLdata4PLZ_04_14 <- subset(JLLdata4PLZ_04_14, select=-c(Miete_H1,Miete_H2,FLAECHE_HA))
JLLdata4PLZ_04_14$order <- rep(1:190, each=11)
JLLdata4PLZ_04_14 <- JLLdata4PLZ_04_14[order(JLLdata4PLZ_04_14$Zeit,
                                             JLLdata4PLZ_04_14$order),]
#View(JLLdata4PLZ_04_14)

JLLtimeST <- ST(PLZ, JLLtime, delta(JLLtime))
Miete4STDF <- data.frame(JLLdata4PLZ_04_14$Miete)
colnames(Miete4STDF) <- c("Miete")

JLLstfdf  <- STFDF(PLZ, JLLtime, Miete4STDF, endTime = delta(JLLtime))
stplot(JLLstfdf, mode="xy")
stplot(JLLstfdf, animate=2)
names(JLLstfdf@data)



library("plotGoogleMaps")
library("RColorBrewer")
stplotGMAPS <- stplotGoogleMaps(JLLstfdf,
                                zcol="Miete",
                                stfilename='JLLMiete.htm',
                                colPalette=brewer.pal(9, "YlOrRd"),
                                at = seq(from=2, to=15, length.out=9),
                                mapTypeId='ROADMAP',
                                w='49%',
                                h='49%', 
                                fillOpacity=0.6)

JLLstfdf[JLLstfdf@time[c(1,6,11)]
JLLstfdf_subset <- JLLstfdf[JLLstfdf@time[c(1,6,11)]]

JLLstfdf@time[c(1,6,11)]
