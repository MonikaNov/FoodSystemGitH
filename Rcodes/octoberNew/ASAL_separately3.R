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
AmBEST00<-lmer(log(isdataTS$Yield)~SeasPr + I(SeasPr^2)  + 
                 AvgTemp+ (1 | ID1),data=isdataScTS)
summary(AmBEST00)

AmArE2100<-lme(log(Yield0)~SeasPr + I(SeasPr^2) + AvgTemp, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=2,q=1),
               data=isdataScTS,na.action=na.exclude)

summary(AmArE2100)
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 1. ASAL

Gale1<-lmer(log(Yield0)~SeasPr + I(SeasPr^2)  + 
                 AvgTemp+ (1 | ID1),data=isdataScTS[as.logical(isdataScTS$ASAL),])
summary(Gale1)

Gale12<-lmer(log(Yield0)~SeasPr   + 
              AvgTemp+ (1 | ID1),data=isdataScTS[as.logical(isdataScTS$ASAL),])
summary(Gale12) #ook
nobs(Gale12)

Gale13<-lmer(log(Yield0)~SeasPr   + 
               AvgTemp+ (1 | ID1),data=isdataScTS[(! (rownames(isdataScTS) %in% rem) & as.logical(isdataScTS$ASAL)),])
summary(Gale13) #ook
nobs(Gale13)

GaleAR1<-lme(log(Yield0)~SeasPr + I(SeasPr^2) + AvgTemp, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=2,q=1),
               data=isdataScTS[as.logical(isdataScTS$ASAL),],na.action=na.exclude)

summary(GaleAR1)

GaleAR12<-lme(log(Yield0)~SeasPr +  AvgTemp, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=2,q=1),
             data=isdataScTS[as.logical(isdataScTS$ASAL),],na.action=na.exclude)

summary(GaleAR12) #welp, this seems to be reasonable

GaleAR13<-lme(log(Yield0)~SeasPr +  AvgTemp, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=2,q=1),
              data=isdataScTS[(! (rownames(isdataScTS) %in% rem) & as.logical(isdataScTS$ASAL)),],na.action=na.exclude)

summary(GaleAR13) 
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# 1. non-ASAL



Pale1<-lmer(log(Yield0)~SeasPr + I(SeasPr^2)  + 
              AvgTemp+ (1 | ID1),data=isdataScTS[!as.logical(isdataScTS$ASAL),])
summary(Pale1)

Pale12<-lmer(log(Yield0)~SeasPr   + 
               AvgTemp+ (1 | ID1),data=isdataScTS[!as.logical(isdataScTS$ASAL),])
summary(Pale12) 
nobs(Pale12)

Pale13<-lmer(log(Yield0)~SeasPr   + I(SeasPr^2)+
               AvgTemp+ (1 | ID1),data=isdataScTS[(! (rownames(isdataScTS) %in% rem) & (!as.logical(isdataScTS$ASAL))),])
summary(Pale13)
nobs(Pale13)

PaleAR1<-lme(log(Yield0)~SeasPr + I(SeasPr^2) + AvgTemp, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=2,q=1),
             data=isdataScTS[!as.logical(isdataScTS$ASAL),],na.action=na.exclude)

summary(PaleAR1)

PaleAR12<-lme(log(Yield0)~SeasPr +   I(SeasPr^2), random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=2,q=1),
              data=isdataScTS[!as.logical(isdataScTS$ASAL),],na.action=na.exclude)

summary(PaleAR12) 

PaleAR13<-lme(log(Yield0)~SeasPr +   I(SeasPr^2) +  AvgTemp, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=2,q=1),
              data=isdataScTS[(! (rownames(isdataScTS) %in% rem) & (!as.logical(isdataScTS$ASAL))),],na.action=na.exclude)

summary(PaleAR13) 

PaleAR14<-lme(log(Yield0)~SeasPr +   I(SeasPr^2) , random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=2,q=1),
              data=isdataScTS[(! (rownames(isdataScTS) %in% rem) & (!as.logical(isdataScTS$ASAL))),],na.action=na.exclude)

summary(PaleAR14) 