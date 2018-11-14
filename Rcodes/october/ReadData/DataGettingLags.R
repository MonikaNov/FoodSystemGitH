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


load("Main/dataOct.RData")
load("Main/dataOctober.RData")
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# I may have to scale evrything, otherwise it will never converge..




# it may actually be better first to get the aggregates over months I need and then scalling...
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
dataAll$PrecCV_MAM<-(dataAll$PrecCVMarch+dataAll$PrecCVApril+dataAll$PrecCVMay)/3
dataAll$PrecCV_OND<-(dataAll$PrecCVOctober+dataAll$PrecCVNovember+dataAll$PrecCVDecember)/3
dataAll$Spell10_MAM<-(dataAll$Spell10March+dataAll$Spell10April+dataAll$Spell10May)
dataAll$Spell10_OND<-(dataAll$Spell10October+dataAll$Spell10November+dataAll$Spell10December)
dataAll$Spell20_MAM<-(dataAll$Spell20March+dataAll$Spell20April+dataAll$Spell20May)
dataAll$Spell20_OND<-(dataAll$Spell20October+dataAll$Spell20November+dataAll$Spell20December)
dataAll$DrySpell_MAM<-pmax(dataAll$DrySpellMarch,dataAll$DrySpellApril,dataAll$DrySpellMay)
dataAll$DrySpell_OND<-pmax(dataAll$DrySpellOctober,dataAll$DrySpellNovember,dataAll$DrySpellDecember)
dataAll$MaxRain_MAM<-pmax(dataAll$MaxRainMarch,dataAll$MaxRainApril,dataAll$MaxRainMay)
dataAll$MaxRain_OND<-pmax(dataAll$MaxRainOctober,dataAll$MaxRainNovember,dataAll$MaxRainDecember)
dataAll$PrecTot_MAM<-(dataAll$CumulPrecMarch+dataAll$CumulPrecApril+dataAll$CumulPrecMay)
dataAll$PrecTot_OND<-(dataAll$CumulPrecOctober+dataAll$CumulPrecNovember+dataAll$CumulPrecDecember)
dataAll$Prec2m_MAM<-(dataAll$CumulPrecMarch+dataAll$CumulPrecApril)
dataAll$Prec2m_OND<-(dataAll$CumulPrecOctober+dataAll$CumulPrecNovember)
#-------------------
dataAll$TempAvgMx_MAM<-(dataAll$TempAvgMxMarch+dataAll$TempAvgMxApril+dataAll$TempAvgMxMay)/3
dataAll$TempAvgMx_OND<-(dataAll$TempAvgMxOctober+dataAll$TempAvgMxNovember+dataAll$TempAvgMxDecember)/3
dataAll$TempAvg_MAM<-(dataAll$TempAvgMarch+dataAll$TempAvgApril+dataAll$TempAvgMay)/3
dataAll$TempAvg_OND<-(dataAll$TempAvgOctober+dataAll$TempAvgNovember+dataAll$TempAvgDecember)/3
dataAll$TempDD_MAM<-(dataAll$TempDDMarch+dataAll$TempDDApril+dataAll$TempDDMay)
dataAll$TempDD_OND<-(dataAll$TempDDOctober+dataAll$TempDDNovember+dataAll$TempDDDecember)
dataAll$TempHW_MAM<-(dataAll$TempHwDaysMarch+dataAll$TempHwDaysApril+dataAll$TempHwDaysMay)
dataAll$TempHW_OND<-(dataAll$TempHwDaysOctober+dataAll$TempHwDaysNovember+dataAll$TempHwDaysDecember)
dataAll$TempSD_MAM<-(dataAll$TempSDMarch+dataAll$TempSDApril+dataAll$TempSDMay)/3
dataAll$TempSD_OND<-(dataAll$TempSDOctober+dataAll$TempSDNovember+dataAll$TempSDDecember)/3

# COOL, THE SEASON AGGREGATES ALL CHECKED 18.10.2018  G*R*O*O*T

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dataAllTS<-pdata.frame(dataAll,index=c("ID1","Year"))
dataAllScTS<-dataAllTS
dataAllScTS[,-c(1:4,149:151,155)]<-scale(dataAllTS[,-c(1:4,149:151,155)])

lagI<-setdiff(seq(1,ncol(dataAllScTS)),c(1:4,149:151,155))
lagged<-data.frame(sapply(lagI ,function(x)  lag(dataAllScTS[,x],1)  ))
names(lagged)<-paste0( names(dataAllScTS[,lagI]),"_L1")

dataAllSc<-cbind(dataAllScTS,lagged)
dataAllScTS<-pdata.frame(dataAllSc,index=c("ID1","Year"))

#checking
# better checking:
write.csv(dataAllSc,file="dataOct.csv")
# dataAllScTS[dataScTS$ID1==11& dataScTS$Year %in% 1990:2000,c("MarchT_L1","MarchT")] from previous code
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ok, turns out that  I may also need the unscaled...so just lagged

lagI<-setdiff(seq(1,ncol( dataAllTS )),c(1:4,149:151,155))
lagged<-data.frame(sapply(lagI ,function(x)  lag(dataAllTS[,x],1)  ))
names(lagged)<-paste0( names(dataAllTS[,lagI]),"_L1")

dataAllUs<-cbind(dataAllTS,lagged)
dataAllUsTS<-pdata.frame(dataAllUs,index=c("ID1","Year"))
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

#checking
dataAllUsTS[dataAllUsTS$ID1==4& dataAllUsTS$Year %in% 1999:2017,c("TempAvgMxJune","TempAvgMxMarch","Year","ID1","Yield","west1","code")]
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
rm(list=c("lagged","lagI",'WDhome','WDuni'))
save.image("~/foodSystems/dataFS/Main/dataOct.RData")