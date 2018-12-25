rm(list=ls())
library(ggeffects);library(ggplot2);library(dplyr); library(tseries); library(plm); library(nlme); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)
load("dataFS/Main/DaTS.RData")

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# these models are the best ones for now:
# I am oing to estimate linear regression to get some estimates of R^2

KEN11d<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
            +AvgTemp + CVTempK, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1) ,
            data=ScaledTS,na.action=na.exclude); summary(KEN11d);vif(KEN11d)
# and subsamples
KEN11d_ASAL<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
                 +AvgTemp + CVTempK, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
                 data=ScaledTS[ScaledTS$ASAL==1,],na.action=na.exclude); summary(KEN11d_ASAL);vif(KEN11d_ASAL)

KEN11d_nonASAL<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
                    +AvgTemp + CVTempK, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
                    data=ScaledTS[ScaledTS$ASAL==0,],na.action=na.exclude); summary(KEN11d_nonASAL);vif(KEN11d_nonASAL)
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

KEN11d_lm<-lm(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
            +AvgTemp + CVTempK+ ID1,data=ScaledTS,na.action=na.exclude)
summary(KEN11d_lm)

KEN11d_lm_justID<-lm(log(Yield0)~ ID1,data=ScaledTS,na.action=na.exclude)
summary(KEN11d_lm_justID) # ehm...


KEN11d_lm_noID<-lm(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
              +AvgTemp + CVTempK,data=ScaledTS,na.action=na.exclude)
summary(KEN11d_lm_noID)

#------------------------------------------------------------------------------------------------------------------------------------------------------
ASAL11d_lm<-lm(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
              +AvgTemp + CVTempK+ ID1,data=ScaledTS[ScaledTS$ASAL==1,],na.action=na.exclude)
summary(ASAL11d_lm)

ASAL11d_lm_justID<-lm(log(Yield0)~ ID1,data=ScaledTS,na.action=na.exclude)
summary(ASAL11d_lm_justID[ScaledTS$ASAL==1,]) # ehm...


ASAL11d_lm_noID<-lm(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
                   +AvgTemp + CVTempK,data=ScaledTS[ScaledTS$ASAL==1,],na.action=na.exclude)
summary(ASAL11d_lm_noID)
#------------------------------------------------------------------------------------------------------------------------------------------------------

nonASAL11d_lm<-lm(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
              +AvgTemp + CVTempK+ ID1,data=ScaledTS[ScaledTS$ASAL==0,],na.action=na.exclude)
summary(nonASAL11d_lm)

nonASAL11d_lm_justID<-lm(log(Yield0)~ ID1,data=ScaledTS[ScaledTS$ASAL==0,],na.action=na.exclude)
summary(nonASAL11d_lm_justID) # ehm...


nonASAL11d_lm_noID<-lm(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
                   +AvgTemp + CVTempK,data=ScaledTS[ScaledTS$ASAL==0,],na.action=na.exclude)
summary(nonASAL11d_lm_noID)