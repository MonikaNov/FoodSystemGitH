rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma
library(dplyr); library(tseries); library(plm); library(lme4);library(nlme); library(lattice); library(car); library(lmerTest); library(sandwich); library(lmtest)
setwd(WDhome)
setwd(WDuni)
load("Main/isdataTS.RData")
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#the best
AmBEST<-lmer(log(isdataTS$Yield)~SeasPr + I(SeasPr^2) + I(SeasPr * AvgTemp) + 
               AvgTemp + CVPrec + SDTemp + (1 | ID1),data=isdataScTS)
summary(AmBEST)

AmArE21<-lme(log(Yield0)~SeasPr + I(SeasPr^2) + I(SeasPr * AvgTemp) + 
               AvgTemp + CVPrec + SDTemp, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=2,q=1),
             data=isdataScTS,na.action=na.exclude)
summary(AmArE21)
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 1. ASAL

AmBEST<-lmer(log(Yield0)~SeasPr + I(SeasPr^2) + I(SeasPr * AvgTemp) + 
               AvgTemp + CVPrec + SDTemp + (1 | ID1),data=isdataScTS[as.logical(isdataScTS$ASAL),])
summary(AmBEST)

AmArE21<-lme(log(Yield0)~SeasPr + I(SeasPr^2) + I(SeasPr * AvgTemp) + 
               AvgTemp + CVPrec + SDTemp, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=2,q=1),
             data=isdataScTS[as.logical(isdataScTS$ASAL),],na.action=na.exclude)
summary(AmArE21)
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 2. non-ASAL

AmBEST<-lmer(log(Yield0)~SeasPr + I(SeasPr^2) + I(SeasPr * AvgTemp) + 
               AvgTemp + CVPrec + SDTemp + (1 | ID1),data=isdataScTS[!as.logical(isdataScTS$ASAL),])
summary(AmBEST)

AmArE21<-lme(log(Yield0)~SeasPr + I(SeasPr^2) + I(SeasPr * AvgTemp) + 
               AvgTemp + CVPrec + SDTemp, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=2,q=1),
             data=isdataScTS[!as.logical(isdataScTS$ASAL),],na.action=na.exclude)
summary(AmArE21)