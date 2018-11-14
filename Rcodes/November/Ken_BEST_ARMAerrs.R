rm(list=ls())
setwd("foodSystems/dataFS") 
setwd("dataFS") # home
library(dplyr); library(tseries); library(plm); library(nlme); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)

load("Main/DaTS.RData")
# arma(11) and ar(2) the best. AIC a bit better for 11>> claiming KenARe11 the best !!!
KenARe11<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
              +AvgTemp + SDTemp  + HWDays, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
              data=ScaledTS,na.action=na.exclude)

acf(summary(KenARe11)$resid)
pacf(summary(KenARe11)$resid)

acf(summary(KenARe20)$resid)
pacf(summary(KenARe20)$resid)
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

Kendalln3<-lmer(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
                +AvgTemp + SDTemp  + HWDays+(1|ID1),data=ScaledTS) 
summary(Kendalln3)
acf(summary(Kendalln3)$resid)
pacf(summary(Kendalln3)$resid)
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
reML<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
              +AvgTemp + SDTemp  + HWDays, random= ~1 | ID1,na.action=na.exclude,
          data=ScaledTS); summary(reML)

AmArE<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
           +AvgTemp + SDTemp  + HWDays, random= ~1 | ID1,correlation=corAR1(0,form=~as.numeric(Year)|ID1) ,
           data=ScaledTS,na.action=na.exclude)

summary(AmArE)
acf(summary(AmArE)$resid)
pacf(summary(AmArE)$resid)

anova(reML,AmArE)
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
KenARe01<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
           +AvgTemp + SDTemp  + HWDays, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=0,q=1),
           data=ScaledTS,na.action=na.exclude)

summary(KenARe01)
anova(reML,AmArE,KenARe01)
anova(reML,KenARe01)
acf(summary(KenARe01)$resid)
pacf(summary(KenARe01)$resid)

KenARe11<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
              +AvgTemp + SDTemp  + HWDays, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
              data=ScaledTS,na.action=na.exclude)
summary(KenARe11); anova(reML,AmArE,KenARe11); anova(reML,KenARe11); anova(KenARe01,KenARe11)
plot(ACF(KenARe11),alpha=0.05)
acf(summary(KenARe11)$resid)
pacf(summary(KenARe11)$resid)

KenARe21<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
              +AvgTemp + SDTemp  + HWDays, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=2,q=1),
              data=ScaledTS,na.action=na.exclude)
summary(KenARe21); anova(reML,AmArE,KenARe21); anova(reML,KenARe21); anova(KenARe01,KenARe21); anova(KenARe11,KenARe21)

acf(summary(KenARe21)$resid)
pacf(summary(KenARe21)$resid)
plot(ACF(KenARe11),alpha=0.05)

KenARe20<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
              +AvgTemp + SDTemp  + HWDays, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=2,q=0),
              data=ScaledTS,na.action=na.exclude)
summary(KenARe20); anova(reML,AmArE,KenARe20); anova(KenARe11,KenARe20); anova(KenARe01,KenARe20); anova(KenARe21,KenARe20)
acf(summary(KenARe20)$resid)
pacf(summary(KenARe20)$resid)
#maybe ar(2) based on acf, pacf?
#..or 11
plot(ACF(KenARe1),alpha=0.05)

KenARe02<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
              +AvgTemp + SDTemp  + HWDays, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=2,q=0),
              data=ScaledTS,na.action=na.exclude)
summary(KenARe02); anova(reML,AmArE,KenARe02); anova(KenARe11,KenARe02); anova(KenARe01,KenARe02); anova(KenARe21,KenARe02)
acf(summary(KenARe02)$resid)
pacf(summary(KenARe02)$resid)
#maybe ar(2) based on acf, pacf?



KenARe12<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4 # really no good
              +AvgTemp + SDTemp  + HWDays, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=2),
              data=ScaledTS,na.action=na.exclude)
summary(KenARe12); anova(reML,AmArE,KenARe12); anova(KenARe11,KenARe12); anova(KenARe01,KenARe12); anova(KenARe02,KenARe12)
acf(summary(KenARe12)$resid)
pacf(summary(KenARe12)$resid)


KenARe22<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4 #no good
              +AvgTemp + SDTemp  + HWDays, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=2,q=2),
              data=ScaledTS,na.action=na.exclude)
summary(KenARe22); anova(reML,AmArE,KenARe22); anova(KenARe11,KenARe22); anova(KenARe21,KenARe22); anova(KenARe12,KenARe22)
acf(summary(KenARe22)$resid)
pacf(summary(KenARe22)$resid)
