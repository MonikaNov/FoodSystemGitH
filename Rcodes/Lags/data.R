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
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# I may have to scale evrything, otherwise it will never converge..

load("Main/MaizeClimate.RData")

MaizeClimateTS<-pdata.frame(MaizeClimate,index=c("ID1","Year"))
MaizeClimateScTS<-MaizeClimateTS
MaizeClimateScTS[,-c(1:5,32:34,38)]<-scale(MaizeClimateTS[,-c(1:5,32:34,38)])

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

MaizeClimateScTS[,-c(1:5,32:34,38)]

lagI<-setdiff(seq(1,ncol( MaizeClimateScTS )),c(1:5,32:34,38))
lagged<-data.frame(sapply(lagI ,function(x)  lag(MaizeClimateScTS[,x],1)  ))
names(lagged)<-paste0( names(MaizeClimateScTS[,lagI]),"_L1")

leads<-data.frame(sapply(6:10 ,function(x)  lag(MaizeClimateScTS[,x],-1)  ))
names(leads)<-paste0( names(MaizeClimateScTS[,6:10]),"_Lead1")

dataSc<-cbind(MaizeClimateScTS,lagged,leads)
dataScTS<-pdata.frame(dataSc,index=c("ID1","Year"))

#checking
dataScTS[dataScTS$ID1==11& dataScTS$Year %in% 1990:2000,c("MarchT_L1","MarchT")]
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ok, turns out that  I may also need the unscaled...

lagI<-setdiff(seq(1,ncol( MaizeClimateTS )),c(1:5,32:34,38))
lagged<-data.frame(sapply(lagI ,function(x)  lag(MaizeClimateTS[,x],1)  ))
names(lagged)<-paste0( names(MaizeClimateTS[,lagI]),"_L1")

leads<-data.frame(sapply(6:10 ,function(x)  lag(MaizeClimateTS[,x],-1)  ))
names(leads)<-paste0( names(MaizeClimateTS[,6:10]),"_Lead1")

dataUs<-cbind(MaizeClimateTS,lagged,leads)
dataUsTS<-pdata.frame(dataUs,index=c("ID1","Year"))

#checking
dataUsTS[dataUsTS$ID1==4& dataUsTS$Year %in% 1999:2017,c("PrecStDev_MAM_L1","PrecStDev_MAM","Year","ID1","Yield","west1")]
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# and now only center
MaizeClimateCeTS<-MaizeClimateTS
MaizeClimateCeTS[,-c(1:5,32:34,38)]<-scale(MaizeClimateTS[,-c(1:5,32:34,38)],scale=FALSE)

lagI<-setdiff(seq(1,ncol( MaizeClimateTS )),c(1:5,32:34,38))
lagged<-data.frame(sapply(lagI ,function(x)  lag(MaizeClimateCeTS[,x],1)  ))
names(lagged)<-paste0( names(MaizeClimateCeTS[,lagI]),"_L1")

leads<-data.frame(sapply(6:10 ,function(x)  lag(MaizeClimateCeTS[,x],-1)  ))
names(leads)<-paste0( names(MaizeClimateCeTS[,6:10]),"_Lead1")

dataCe<-cbind(MaizeClimateCeTS,lagged,leads)
dataCeTS<-pdata.frame(dataCe,index=c("ID1","Year"))

#checking
dataCeTS[dataCeTS$ID1==49& dataCeTS$Year %in% 1989:2017,c("MaxTemp_OctMar_Lead1","MaxTemp_OctMar_L1","MaxTemp_OctMar","ID1","Yield","west1")]
dataScTS[dataUsTS$ID1==4& dataUsTS$Year %in% 1999:2017,c("PrecStDev_MAM_L1","PrecStDev_MAM","Year","ID1","Yield","west1")]
dataUsTS[dataUsTS$ID1==4& dataUsTS$Year %in% 1999:2017,c("PrecStDev_MAM_L1","PrecStDev_MAM","Year","ID1","Yield","west1")]
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# it may be useful to export the dataset as csv so that I can look at it in excel
write.csv(dataSc, file="Main/dataSc.csv")
write.csv(dataScTS, file="Main/dataScTS.csv")
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

rm(list=setdiff(ls(),c("dataSc","dataScTS","dataUs","dataUsTS","dataCe","dataCeTS")))
save.image("Main/data.RData")