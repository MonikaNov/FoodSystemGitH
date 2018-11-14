rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

# setwd(WDuni)
# setwd(WDhome)

library('dplyr')
library('tseries')
library(plm)
library(lme4)
library(lattice)
library(car)
library(lmerTest)
library(optimx)

load("Main/data.RData")

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# Peggy45 is the best at the moment.



Peggy453<-lmer(Yield ~ I((SeasRain_MAM_L1+SeasRain_OND_L1)*(1-west1)) + I((SeasRain_MAM_L1)*west1)+ I(AvgTemp_MarSep_L1*(1-west1)) + I(AvgTemp_MarSep_L1*(west1))
               +I(SDtemp_OctMar_L1*(1-west1))+    +I(SDtemp_OctMar_L1*west1)
               
               +   I(MaxRain_OND*(1-west1)) +   I(MaxRain_OND*west1) 
               
               +(1+MaxRain_OND+I(SeasRain_MAM_L1+SeasRain_OND_L1) +I(AvgTemp_MarSep_L1)|ID1) ,data=dataUsTS )
summary(Peggy453) 
nobs(Peggy453)

#  koefs<-as.numeric(summary(Peggy45)$coeff[,1]) ; koefs
koefs<-as.numeric(summary(Peggy453)$coeff[,1])  ; koefs

# WEST
aa<-function(x,k=koefs[c(1,3,5,7,9)]) k[1]+sum(x*k[-1])

xx<-c(mean(dataUsTS$SeasRain_MAM_L1,na.rm=TRUE),mean(dataUsTS$AvgTemp_MarSep_L1,na.rm=TRUE), 
      mean(dataUsTS$SDtemp_OctMar_L1,na.rm=TRUE),mean(dataUsTS$MaxRain_OND,na.rm=TRUE) )

DPeggy453<-dataUsTS[rownames(dataUsTS)%in% rownames(  model.frame(Peggy453)),]

xx<-c(mean(DPeggy453$SeasRain_MAM_L1,na.rm=TRUE),mean(DPeggy453$AvgTemp_MarSep_L1,na.rm=TRUE), 
      mean(DPeggy453$SDtemp_OctMar_L1,na.rm=TRUE),mean(DPeggy453$MaxRain_OND,na.rm=TRUE) )

aa(xx)
mean(dataUsTS$SeasRain_MAM_L1,na.rm=TRUE)
mean(dataUsTS$AvgTemp_MarSep_L1,na.rm=TRUE)
mean(dataUsTS$SDtemp_OctMar_L1,na.rm=TRUE)
mean(dataUsTS$MaxRain_OND,na.rm=TRUE)

(-0.05879*sd(dataUsTS$Yield,na.rm=TRUE))+ mean(dataUsTS$Yield,na.rm=TRUE) # what yield should be equal to if everything at mean


(-0.5820957 *sd(dataUsTS$Yield,na.rm=TRUE))+ mean(dataUsTS$Yield,na.rm=TRUE) 


xxx<-c(mean(DPeggy453$SeasRain_MAM_L1,na.rm=TRUE)-sd(DPeggy453$SeasRain_MAM_L1,na.rm=TRUE)   ,
      mean(DPeggy453$AvgTemp_MarSep_L1,na.rm=TRUE)-sd(DPeggy453$AvgTemp_MarSep_L1,na.rm=TRUE)  , 
      mean(DPeggy453$SDtemp_OctMar_L1,na.rm=TRUE)- sd(DPeggy453$SDtemp_OctMar_L1,na.rm=TRUE),
      mean(DPeggy453$MaxRain_OND,na.rm=TRUE)-sd(DPeggy453$MaxRain_OND,na.rm=TRUE)   )

aa(xxx)