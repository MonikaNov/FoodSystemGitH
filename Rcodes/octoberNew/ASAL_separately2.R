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


Donny1<-lmer(log(Yield0)~SeasPr +I(SeasPr^2)+
               AvgTemp + CVPrec  + (1 | ID1),data=isdataScTS[as.logical(isdataScTS$ASAL),])
summary(Donny1)
nobs(Donny1)


Donny12<-lmer(log(Yield0)~SeasPr +SDTemp+I(SeasPr^2)+
               AvgTemp + CVPrec  + (1 | ID1),data=isdataScTS[(! (rownames(isdataScTS) %in% rem) & as.logical(isdataScTS$ASAL)),])
summary(Donny12)
nobs(Donny12)

Donny12<-lmer(Yield0~SeasPr +SDTemp+I(SeasPr^2)+
                AvgTemp + CVPrec  + (1 | ID1),data=isdataScTS[(! (rownames(isdataScTS) %in% rem) & as.logical(isdataScTS$ASAL)),])
summary(Donny12)
nobs(Donny12)
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


Donny1AR<-lme(log(Yield0)~SeasPr + I(SeasPr^2) + 
               AvgTemp + CVPrec + SDTemp, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=2,q=1),
             data=isdataScTS[as.logical(isdataScTS$ASAL),],na.action=na.exclude)
summary(Donny1AR)


Donny2AR<-lme(log(Yield0)~SeasPr + I(SeasPr^2) +
                AvgTemp + CVPrec , random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=2,q=1),
              data=isdataScTS[as.logical(isdataScTS$ASAL),],na.action=na.exclude)
summary(Donny2AR)
nobs(Donny2AR)

Donny3AR<-lme(log(Yield0)~SeasPr + I(SeasPr^2) +
                AvgTemp + CVPrec , random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=2,q=1),
              data=isdataScTS[(! (rownames(isdataScTS) %in% rem) & as.logical(isdataScTS$ASAL)),],na.action=na.exclude)
summary(Donny3AR)
nobs(Donny3AR)

Donny3AR<-lme(Yield~SeasPr + I(SeasPr^2) +
                AvgTemp + CVPrec , random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=2,q=1),
              data=isdataScTS[(! (rownames(isdataScTS) %in% rem) & as.logical(isdataScTS$ASAL)),],na.action=na.exclude)
summary(Donny3AR)
nobs(Donny3AR)


Donny3AR<-lme(log(Yield0)~SeasPr +
                AvgTemp + CVPrec , random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=2,q=1),
              data=isdataScTS[(! (rownames(isdataScTS) %in% rem) & as.logical(isdataScTS$ASAL)),],na.action=na.exclude)
summary(Donny3AR)
nobs(Donny3AR)
# strenge, for ASAL prec not significant
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# 2. non-ASAL

AmBEST<-lmer(log(Yield0)~SeasPr + I(SeasPr^2) + I(SeasPr * AvgTemp) + 
               AvgTemp + CVPrec + SDTemp + (1 | ID1),data=isdataScTS[!as.logical(isdataScTS$ASAL),])
summary(AmBEST)

AmArE21<-lme(log(Yield0)~SeasPr + I(SeasPr^2) + I(SeasPr * AvgTemp) + 
               AvgTemp + CVPrec + SDTemp, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=2,q=1),
             data=isdataScTS[!as.logical(isdataScTS$ASAL),],na.action=na.exclude)
summary(AmArE21)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Dani1<-lmer(log(Yield0)~SeasPr + I(SeasPr^2)  + 
               AvgTemp + CVPrec + SDTemp + (1 | ID1),data=isdataScTS[!as.logical(isdataScTS$ASAL),])
summary(Dani1) # I guess you could call this ok??
nobs(Dani1)

Dani2<-lmer(log(Yield0)~SeasPr + I(SeasPr^2)  + 
              AvgTemp + CVPrec + SDTemp + (1 | ID1),
            data=isdataScTS[((! (rownames(isdataScTS) %in% rem)) & (!as.logical(isdataScTS$ASAL))),])
summary(Dani2) 
nobs(Dani2)

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Dani1AR<-lme(log(Yield0)~SeasPr + I(SeasPr^2) + I(SeasPr * AvgTemp) + 
               AvgTemp + CVPrec + SDTemp, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=2,q=1),
             data=isdataScTS[!as.logical(isdataScTS$ASAL),],na.action=na.exclude)
summary(Dani1AR)

  Dani3AR<-lme(log(Yield0)~SeasPr + I(SeasPr^2) + I(SeasPr * AvgTemp) + 
                CVPrec + SDTemp, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=2,q=1),
             data=isdataScTS[!as.logical(isdataScTS$ASAL),],na.action=na.exclude)
  summary(Dani3AR)

Dani1AR<-lme(log(Yield0)~SeasPr + I(SeasPr^2) + I(SeasPr * AvgTemp) + 
               AvgTemp + CVPrec + SDTemp, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=2,q=1),
             data=isdataScTS[((! (rownames(isdataScTS) %in% rem)) & (!as.logical(isdataScTS$ASAL))),],na.action=na.exclude)
summary(Dani1AR)

Dani12AR<-lme(log(Yield0)~SeasPr + I(SeasPr^2) + I(SeasPr * AvgTemp) + 
                CVPrec + SDTemp, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=2,q=1),
             data=isdataScTS[((! (rownames(isdataScTS) %in% rem)) & (!as.logical(isdataScTS$ASAL))),],na.action=na.exclude)
summary(Dani12AR)

Dani2AR<-lme(log(Yield0)~SeasPr + I(SeasPr^2) + 
               AvgTemp + CVPrec + SDTemp, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=2,q=1),
             data=isdataScTS[((! (rownames(isdataScTS) %in% rem)) & (!as.logical(isdataScTS$ASAL))),],na.action=na.exclude)
summary(Dani2AR)