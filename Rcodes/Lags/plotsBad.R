rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

# setwd(WDuni)
# setwd(WDhome)


library('plyr')
library('dplyr')
library('tseries')
library('data.table')
library(plm)
library(lme4)
library(lattice)
library(car)
library(lmerTest)
library(optimx)

load("Main/data.RData")
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
names(dataScTS)
n<-paste0(names(dataScTS)[-c(1:5,32:34,38)])
rm(n)
n<-noquote(paste(noquote(names(dataScTS))[c(6:10)],collapse=", "))
n

plot(Yield~AvgTemp_OctMar, dataScTS)
plot(Yield~AvgTemp_OctMar, dataScTS,type="l") 
  

grupa2<-aggregate(Yield~ ID1,data=dataScTS,FUN=mean)
grupa2<-aggregate(cbind(n)~ ID1,data=dataScTS,FUN=mean)


laggy0<-lm(Yield ~ I(SeasRain_OND_L1*(1-west1))+I(SeasRain_MAM*(1-west1)) +I(SeasRain_MAM*(west1)) 
           +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1),data=dataScTS )
summary(laggy0)     

laggy1<-lmer(Yield ~ I(SeasRain_OND_L1*(1-west1))+I(SeasRain_MAM*(1-west1)) +I(SeasRain_MAM*(west1)) 
             +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1) 
             +(1+ I(SeasRain_OND_L1*(1-west1))+I(SeasRain_MAM*(1-west1)) +I(SeasRain_MAM*(west1))
               +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1)|ID1)
             ,data=dataScTS )
summary(laggy1)    
nobs(laggy1)   