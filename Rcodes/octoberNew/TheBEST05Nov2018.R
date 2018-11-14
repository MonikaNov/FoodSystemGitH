rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma
library(dplyr); library(tseries); library(plm); library(lme4);library(nlme); library(lattice); library(car); library(lmerTest); library(sandwich); library(lmtest)
setwd(WDhome)
setwd(WDuni)
load("Main/isdataTS.RData")
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
AmBEST2<-lmer(log(isdataTS$Yield[(!(rownames(isdataTS)=="27-1995"))])~SeasPr + I(SeasPr^2) + I(SeasPr * AvgTemp) + AvgTemp + CVPrec + SDTemp + (1 | ID1),data=isdataScTS[(!(rownames(isdataScTS)=="27-1995")),])
rem<-c("27-1995",names(which(resid(AmBEST2,type="pearson")<(-2))))

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#the best

AmBEST<-lmer(log(isdataTS$Yield)~SeasPr + I(SeasPr^2) + I(SeasPr * AvgTemp) + 
               AvgTemp + CVPrec + SDTemp + (1 | ID1),data=isdataScTS)
summary(AmBEST)

  AmBEST3<-lmer(log(isdataTS$Yield[!(rownames(isdataScTS) %in% rem)])~SeasPr + I(SeasPr^2) + I(SeasPr * AvgTemp) + 
                AvgTemp + CVPrec + SDTemp + (1 | ID1),data=isdataScTS[!(rownames(isdataScTS) %in% rem),])
  plot(AmBEST3)
  
AmArE21<-lme(log(Yield0)~SeasPr + I(SeasPr^2) + I(SeasPr * AvgTemp) + 
               AvgTemp + CVPrec + SDTemp, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=2,q=1),
             data=isdataScTS,na.action=na.exclude)
summary(AmArE21)
plot(AmArE21)

  AmArE213<-lme(log(Yield0)~SeasPr + I(SeasPr^2) + I(SeasPr * AvgTemp) + 
               AvgTemp + CVPrec + SDTemp, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=2,q=1),
             data=isdataScTS[!(rownames(isdataScTS) %in% rem),],na.action=na.exclude)
  summary(AmArE213)
  plot(AmArE213)

