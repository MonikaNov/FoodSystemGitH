rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma
library(dplyr); library(tseries); library(plm); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)

load("Main/isdataTS.RData")
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

Amelie0B<-lm(Yield~SeasPr+AvgTemp,data=isdataScTS) # this is basically the same as if unscaled
summary(Amelie0B) 

Amelie0Bln<-lm(log(Yield)~SeasPr+AvgTemp,data=isdataScTS)
summary(Amelie0Bln) 
#--------------------------
Violet0<-lmer(Yield~SeasPr+AvgTemp+(SeasPr+AvgTemp|ASAL),data=isdataScTS)
summary(Violet0) 

Violet0ln<-lmer(log(Yield)~SeasPr+AvgTemp+(SeasPr+AvgTemp|ASAL),data=isdataScTS)
summary(Violet0ln)
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Violet1<-lmer(Yield~SeasPr+AvgTemp+(SeasPr+I(SeasPr^2)+AvgTemp+I(AvgTemp^2)|ASAL),data=isdataScTS)
summary(Violet1) 
anova(Violet1,Violet0)
ranova(Violet1)
    Violet1b<-lmer(Yield~SeasPr+AvgTemp+(SeasPr+I(SeasPr^2)|ASAL),data=isdataScTS)
    summary(Violet1b) 
    anova(Violet1,Violet1b)
    ranova(Violet1b) 
    ranova(Violet1b,reduce.terms=FALSE) 
Violet1ln<-lmer(log(Yield0)~SeasPr+AvgTemp+(SeasPr+I(SeasPr^2)+AvgTemp+I(AvgTemp^2)|ASAL),data=isdataScTS)
summary(Violet1ln)