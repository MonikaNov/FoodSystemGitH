rm(list=ls())
library(MASS);library(dplyr); library(tseries); library(plm); library(nlme); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)
load("dataFS/Main/DaTS.RData")
# load("Rcodes/DecemberNew/KEN11d_stepNice.RData")
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# these models are the best ones for now:

KEN11d<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
            +AvgTemp + CVTempK, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
            data=ScaledTS,na.action=na.exclude); summary(KEN11d)

KEN11d2<-lme(log(Yield0)~SeasPr +AvgTemp +I(SeasPr^2)+CVPrec+Spell+Spell4
           + CVTempK+HWDays+MaxT, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
            data=ScaledTS,na.action=na.exclude); summary(KEN11d2)

extractAIC(KEN11d);extractAIC(KEN11d2)
anova(KEN11d2,KEN11d)
vif(KEN11d2)
#-------  have to refit with ML ----------- --------------------

KEN11d_ML<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
            +AvgTemp + CVTempK, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
            data=ScaledTS,na.action=na.exclude,method="ML"); summary(KEN11d_ML)


KEN11d2_ML<-lme(log(Yield0)~SeasPr +AvgTemp +I(SeasPr^2)+CVPrec+Spell+Spell4
                              + CVTempK+HWDays+MaxT, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
                data=ScaledTS,na.action=na.exclude,method="ML"); summary(KEN11d2_ML)  ##coel, now exatly the same as the result of the step..

extractAIC(KEN11d_ML);extractAIC(KEN11d2_ML)
anova(KEN11d2_ML,KEN11d_ML) #ehm...this is in the favour of the second one. try now without the maxT
vif(KEN11d2_ML)

KEN11d3_ML<-lme(log(Yield0)~SeasPr +AvgTemp +I(SeasPr^2)+CVPrec+Spell+Spell4
                + CVTempK+HWDays, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
                data=ScaledTS,na.action=na.exclude,method="ML"); summary(KEN11d3_ML)

extractAIC(KEN11d_ML);extractAIC(KEN11d3_ML)
anova(KEN11d3_ML,KEN11d_ML)
vif(KEN11d3_ML)