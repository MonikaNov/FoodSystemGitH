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
plot(Yield~HeatWDays_OctMar, dataScTS)
plot(Yield~AvgTemp_OctMar, dataScTS,type="l") 
  
names(dataScTS)
Xi<-setdiff((1:124),c(1:5,32:38))
i<-0

i<-i+1
plot(Yield~dataScTS[,Xi[i]], dataScTS,xlab=names(dataScTS[Xi[i]])) 
names(dataScTS[Xi[i]])
summary(lm(Yield~dataScTS[,Xi[i]],dataScTS));
summary(lm(Yield~dataScTS[,Xi[i]]+I(dataScTS[,Xi[i]]^2),dataScTS))
names(dataScTS[Xi[i]])


#  +   "AvgTemp_OctMar"    "CumDD_OctMar"     "HeatWDays_OctMar"  ("AvgTemp_OctMar")


#  -


#  hill:  "MaxTemp_OctMar"     "MaxTemp_OctMar"  "AvgTemp_OctMar"


#  U  ("HeatWDays_OctMar")





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