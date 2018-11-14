rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma
library(dplyr); library(tseries); library(plm); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)

load("Main/Da.RData")
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

Varga42<-lmer(Yield~SeasPr+AvgTemp+I(SeasPr^2)+(AvgTemp|ID1),data=DaTS)   
summary(Varga42) 
ranova(Varga42)  # NICE

#--------------------------------------------------------------------------------------------


Varga04ln<-lm(log(Yield)~SeasPr+AvgTemp+I(SeasPr^2),data=DaTS[DaTS$ASAL==1,])
summary(Varga04ln)
hist(DaTS$SeasPr[DaTS$ASAL==1],40)
hist(DaTS$SeasPr[DaTS$ASAL==0],40)
myF<-function(x) {x=exp(0.0005834*x)*exp(-3.846e-07*x^2) }
plot(myF(0:1500),type='l')
plot(myF(0:1000),type='l') #pretty good and accurate. above 1000 happens rarely at ASAL>>if yes, probably a lot at once. flood sand so..


#--------------------------------------------------------------------------------------------


Varga44<-lmer(Yield~SeasPr+AvgTemp+I(SeasPr^2)+(AvgTemp|ID1),data=DaTS[DaTS$ASAL==TRUE,])
summary(Varga44)
nobs(Varga44)
ranova(Varga44)
Varga45<-lmer(Yield~SeasPr+AvgTemp+(AvgTemp|ID1),data=DaTS[DaTS$ASAL==TRUE,])
summary(Varga45)
nobs(Varga45)
ranova(Varga45)

Varga45<-lmer(Yield~SeasPr+AvgTemp+(0+AvgTemp|ID1),data=DaTS[DaTS$ASAL==TRUE,])
summary(Varga45)
nobs(Varga45)
ranova(Varga45,reduce=FALSE) 