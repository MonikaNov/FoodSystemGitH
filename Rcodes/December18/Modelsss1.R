rm(list=ls())
library(dplyr); library(tseries); library(plm); library(nlme); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)
load("dataFS/Main/DaTS.RData")

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# these models are the best ones for now:

KEN11d<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
            +AvgTemp + CVTempK, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
            data=ScaledTS,na.action=na.exclude); summary(KEN11d); exp(summary(KEN11d)$coef[[1]])
# and subsamples
KEN11d_ASAL<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
                 +AvgTemp + CVTempK, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
                 data=ScaledTS[ScaledTS$ASAL==1,],na.action=na.exclude); summary(KEN11d_ASAL); exp(summary(KEN11d_ASAL)$coef[[1]])

KEN11d_nonASAL<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
                    +AvgTemp + CVTempK, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
                    data=ScaledTS[ScaledTS$ASAL==0,],na.action=na.exclude); summary(KEN11d_nonASAL); exp(summary(KEN11d_nonASAL)$coef[[1]])
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
KEN11h<-update(KEN11d,.~.,method="ML")
KEN11h_ASAL<-update(KEN11d_ASAL,.~.,method="ML")
KEN11h_nonASAL<-update(KEN11d_nonASAL,.~.,method="ML")


drop1(KEN11d)
drop1(KEN11h)
ranova(KEN11h)

roza1<-step(KEN11h)

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
KEN11d3<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
            +AvgTemp + CVTempK, random= ~1 +SeasPr| ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
            data=ScaledTS,na.action=na.exclude)

anova(KEN11d,KEN11d3)

KENd_lmer<-lmer(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4+AvgTemp + 
                 CVTempK+(1 | ID1),data=ScaledTS)
summary(KENd_lmer);  exp(summary(KENd_lmer)$coef[[1]])
  
ranova(KENd_lmer)
drop1(KENd_lmer)

KENd_lmerRE<-lmer(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4+AvgTemp + 
                  CVTempK+(SeasPr +I(SeasPr^2)+CVPrec+Spell+Spell4+AvgTemp + 
                             CVTempK| ID1),data=ScaledTS)
ran1<-ranova(KENd_lmerRE)