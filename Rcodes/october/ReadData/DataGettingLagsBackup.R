rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

# setwd(WDuni)
# setwd(WDhome)


library('tseries')
library(plm)
library(lme4)
library(lattice)
library(car)
library(lmerTest)
library(optimx)


load("Main/dataOctober.RData")
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# I may have to scale evrything, otherwise it will never converge..


dataAllTS<-pdata.frame(dataAll,index=c("ID1","Year"))
dataAllScTS<-dataAllTS
dataAllScTS[,-c(1:4,89:91,95)]<-scale(dataAllTS[,-c(1:4,89:91,95)])

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

lagI<-setdiff(seq(1,ncol(dataAllScTS)),c(1:4,89:91,95))
lagged<-data.frame(sapply(lagI ,function(x)  lag(dataAllScTS[,x],1)  ))
names(lagged)<-paste0( names(dataAllScTS[,lagI]),"_L1")

dataAllSc<-cbind(dataAllScTS,lagged)
dataAllScTS<-pdata.frame(dataAllSc,index=c("ID1","Year"))

#checking
# better checking:
write.csv(dataAllSc,file="dataAllSc.csv")
# dataAllScTS[dataScTS$ID1==11& dataScTS$Year %in% 1990:2000,c("MarchT_L1","MarchT")] from previous code
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ok, turns out that  I may also need the unscaled...so just lagged

lagI<-setdiff(seq(1,ncol( dataAllTS )),c(1:5,32:34,38))
lagged<-data.frame(sapply(lagI ,function(x)  lag(dataAllTS[,x],1)  ))
names(lagged)<-paste0( names(dataAllTS[,lagI]),"_L1")

dataAllUs<-cbind(dataAllTS,lagged)
dataAllUsTS<-pdata.frame(dataAllUs,index=c("ID1","Year"))

#checking
dataUsTS[dataUsTS$ID1==4& dataUsTS$Year %in% 1999:2017,c("PrecStDev_MAM_L1","PrecStDev_MAM","Year","ID1","Yield","west1")]