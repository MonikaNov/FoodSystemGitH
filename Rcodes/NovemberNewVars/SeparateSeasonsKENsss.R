rm(list=ls())
library(ggeffects);library(ggplot2);library(dplyr); library(tseries); library(plm); library(nlme); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)
load("dataFS/Main/DaTS.RData")

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# these models are the best ones for now:

KEN11a<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
            +AvgTemp + SDTemp, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1) ,
            data=ScaledTS,na.action=na.exclude); summary(KEN11a)
# and subsamples
KEN11a_ASAL<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
                 +AvgTemp + SDTemp, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
                 data=ScaledTS[ScaledTS$ASAL==1,],na.action=na.exclude); summary(KEN11a_ASAL)

KEN11a_nonASAL<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
                    +AvgTemp + SDTemp, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
                    data=ScaledTS[ScaledTS$ASAL==0,],na.action=na.exclude); summary(KEN11a_nonASAL)
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#1.ond


KEN11b<-lme(log(Yield0)~SeasPr_ond+I(SeasPr_ond^2)+CVPrec_ond+Spell_ond+Spell4_ond
            +AvgTemp_ond + SDTemp_ond, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1) ,
            data=ScaledTS,na.action=na.exclude); summary(KEN11b)

KEN11c<-lme(log(Yield0)~SeasPr_mam+I(SeasPr_mam^2)+CVPrec_mam+Spell_mam+Spell4_mam
            +AvgTemp_mam + SDTemp_mam, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1) ,
            data=ScaledTS,na.action=na.exclude); summary(KEN11c)