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

DaTS<-cbind.data.frame(DaTS,lagged)
DaTS<-pdata.frame(DaTS,index=c("ID1","Year"))
DaTS[DaTS$Year==1981, 166:167]<-NA

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
# now seasons   ........   ........        ..........      ..... . .   . . . . . .



DaTS$AvgTempK[DaTS$ASAL==TRUE]<-( (DaTS$AvgTempK_MarMay[DaTS$ASAL==TRUE]+DaTS$AvgTempK_OctDec_L1[DaTS$ASAL==TRUE]) /2)
DaTS$CVTempK[DaTS$ASAL==TRUE]<-( (DaTS$CVTempK_MarMay[DaTS$ASAL==TRUE]+DaTS$CVTempK_OctDec_L1[DaTS$ASAL==TRUE]) /2)

DaTS$AvgTempK[DaTS$ASAL==FALSE]<-(DaTS$AvgTempK_MarAug[DaTS$ASAL==FALSE])
DaTS$CVTempK[DaTS$ASAL==FALSE]<-(DaTS$CVTempK_MarAug[DaTS$ASAL==FALSE])

# now scaling...   ........   ........        ..........      ..... . .   . . . . . .
rm(ScaledTS)
ScaledTS<-DaTS
ScaledTS[,c(5:76,81:83,85:124)]<-scale(DaTS[,c(5:76,81:83,85:124)])
ScaledTS$Yield0<-DaTS$Yield
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo


remove(list=setdiff(ls(),c("DaTS","ScaledTS")))
# save.image("dataFS/Main/DaTS.RData")
# save.image("Rcodes/December18/DaTS.RData")  # names???