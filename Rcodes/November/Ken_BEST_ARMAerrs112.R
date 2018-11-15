rm(list=ls())
library(dplyr); library(tseries); library(plm); library(nlme); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)
load("dataFS/Main/DaTS.RData")
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# arma(11) and ar(2) the best. AIC a bit better for 11>> claiming KenARe11 the best !!!

KenARe11<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
              +AvgTemp + SDTemp  + HWDays, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
              data=ScaledTS,na.action=na.exclude); summary(KenARe11)
acf(summary(KenARe11)$resid); pacf(summary(KenARe11)$resid)
anova(KenARe11)
exp(summary(KenARe11)$coef[[1]])
#----------------------------------------------------------------
# subsets of ASAL and non ASAL

KenARe114<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
              +AvgTemp + SDTemp  + HWDays, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
              data=ScaledTS[ScaledTS$ASAL==1,],na.action=na.exclude); summary(KenARe11)
exp(summary(KenARe114)$coef[[1]])



KenARe115<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
               +AvgTemp + SDTemp, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
               data=ScaledTS[ScaledTS$ASAL==0,],na.action=na.exclude); summary(KenARe11)
exp(summary(KenARe115)$coef[[1]])
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

# arma(11) and ar(2) the best. AIC a bit better for 11>> claiming KenARe11 the best !!!
KenARe11<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
              +AvgTemp + SDTemp  + HWDays, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
              data=ScaledTS,na.action=na.exclude); summary(KenARe11)
acf(summary(KenARe11)$resid); pacf(summary(KenARe11)$resid)
anova(KenARe11)
exp(summary(KenARe11)$coef[[1]])
#----------------------------------------------------------------
Ken112<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
            +AvgTemp   + HWDays, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
            data=ScaledTS,na.action=na.exclude)
summary(Ken112) # removed the spare..(insig.)

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# now maybe variants with all covariates and with just  seas precip and temp:

        KenARe113<-lme(log(Yield0)~SeasPr+I(SeasPr^2) +AvgTemp , random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
                      data=ScaledTS,na.action=na.exclude); summary(KenARe113)
        acf(summary(KenARe113)$resid); pacf(summary(KenARe113)$resid)
        curve(exp(summary(KenARe113)$coef[[1]]["SeasPr"]*x)*exp(summary(KenARe113)$coef[[1]]["I(SeasPr^2)"]*x^2 ),-2,3) # N.I.C.E.
        
        KenARe113<-lme(log(Yield0)~SeasPr+AvgTemp , random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
                       data=ScaledTS,na.action=na.exclude); summary(KenARe113)
        acf(summary(KenARe113)$resid); pacf(summary(KenARe113)$resid)
        
        KenARe114<-lme(log(Yield0)~SeasPr+I(SeasPr^2) +AvgTemp + Prec2m+CVPrec+Spell+Spell4 +MaxP
                       + SDTemp + DDays + HWDays+MaxT, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
                       data=ScaledTS,na.action=na.exclude); summary(KenARe114)
        acf(summary(KenARe114)$resid); pacf(summary(KenARe114)$resid)
        acf(summary(KenARe114)$resid); pacf(summary(KenARe114)$resid)
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

        KenARe118<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
                      +AvgTemp + SDTemp  + HWDays, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
                      data=ScaledTS[ScaledTS$ASAL==1,],na.action=na.exclude); summary(KenARe118)
        exp(summary(KenARe118)$coef[[1]])
        
        KenARe118<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
                       +AvgTemp + SDTemp  , random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
                       data=ScaledTS[ScaledTS$ASAL==0,],na.action=na.exclude); summary(KenARe118)
        exp(summary(KenARe118)$coef[[1]])