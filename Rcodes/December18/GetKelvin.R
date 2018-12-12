rm(list=ls())

library('dplyr')
library('purrr')
load("dataFS/Main/DaTS.RData")
library(dplyr); library(tseries); library(plm); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
DaTS$AvgTempK_MarMay<-(DaTS$AvgTemp_MarMay + 273.15)
DaTS$AvgTempK_MarAug<-(DaTS$AvgTemp_MarAug + 273.15)
DaTS$AvgTempK_OctDec<-(DaTS$AvgTemp_OctDec + 273.15)

DaTS$CVTempK_MarMay<-DaTS$SDTemp_MarMay/DaTS$AvgTempK_MarMay
DaTS$CVTempK_MarAug<-DaTS$SDTemp_MarAug/DaTS$AvgTempK_MarAug
DaTS$CVTempK_OctDec<-DaTS$SDTemp_OctDec/DaTS$AvgTempK_OctDec

# laggs of OND season
lagged<-DaTS[,c("AvgTempK_OctDec","CVTempK_OctDec")]
lagged<-data.frame(sapply(c(162,165) ,function(x)  lag(DaTS[,x],1)  ))
names(lagged)<-c("AvgTempK_OctDec_L1", "CVTempK_OctDec_L1")
rm(DaTS)
DaTS<-cbind.data.frame(DaTS,lagged)
DaTS<-pdata.frame(DaTS,index=c("ID1","Year"))
DaTS[DaTS$Year==1981, 162:165]<-NA

#--------------------------------------------------------------------------------------------------------------------------------------------------------------
# a bit of testing:
plot(DaTS$AvgTempK_OctDec_L1,DaTS$AvgTempK_OctDec)
plot(DaTS$AvgTempK_OctDec,DaTS$AvgTemp_OctDec)

plot(DaTS$CVTempK_OctDec,DaTS$SDTemp_OctDec)
plot(DaTS$SDTemp_OctDec,DaTS$CVTempK_OctDec)

plot(DaTS$AvgTempK_MarMay,DaTS$AvgTemp_MarMay)
plot(DaTS$CVTempK_MarMay,DaTS$SDTemp_MarMay)
plot(DaTS$SDTemp_MarMay,DaTS$CVTempK_MarMay)

plot(DaTS$AvgTempK_MarAug,DaTS$AvgTemp_MarAug)
plot(DaTS$CVTempK_MarAug,DaTS$SDTemp_MarAug)
plot(DaTS$SDTemp_MarAug,DaTS$CVTempK_MarAug)

# now scaling...

