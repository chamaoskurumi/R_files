install.packages("spacetime")
library("spacetime")

sp = cbind(x = c(0,0,1), y = c(0,1,1));sp
row.names(sp) = paste("point", 1:nrow(sp), sep="")
library(sp)
sp = SpatialPoints(sp);sp
time = as.POSIXct("2010-08-05")+3600*(10:13)
m = c(10,20,30);m # means for each of the 3 point locations
mydata = rnorm(length(sp)*length(time),mean=rep(m, 4));mydata
IDs = paste("ID",1:length(mydata));IDs
mydata = data.frame(values = signif(mydata,3), ID=IDs);mydata
stidf = as(STFDF(sp, time, mydata), "STIDF");stidf
stidf[1:2,]
all.equal(stidf, stidf[stidf,])
stplot(obj = stidf, cex=10)
names(stidf@data)
mydata
time
