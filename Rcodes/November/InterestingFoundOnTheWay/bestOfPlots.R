rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

library(dplyr); library(tseries); library(plm); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)
setwd(WDuni)
load("Main/Da.RData")
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

par(mfrow=c(2,1))
hist(DaTS$SeasPr[DaTS$ASAL==1],40)
hist(DaTS$SeasPr[DaTS$ASAL==0],40)

par(mfrow=c(1,1))


plot(Yield~AvgTemp, data=DaTS)
plot(Yield~I(AvgTemp^2), data=DaTS)
plot(Yield~AvgTemp, data=DaTS)


plot(Yield~I(AvgTemp^2), data=DaTS,col=4,type='p',xlim=c(0,800)) 
lines(Yield~AvgTemp, data=DaTS,col=1,type='p') 