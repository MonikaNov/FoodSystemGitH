rm(list=ls())
library(dplyr); library(tseries); library(plm); library(nlme); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)
load("dataFS/Main/DaTS.RData")

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# these models are the best ones for now:

KEN11a<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
            +AvgTemp + SDTemp, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
            data=ScaledTS,na.action=na.exclude); summary(KEN11a); exp(summary(KEN11a)$coef[[1]])

AIC(KEN11a)
# and subsamples
KEN11a_ASAL<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
                 +AvgTemp + SDTemp, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
                 data=ScaledTS[ScaledTS$ASAL==1,],na.action=na.exclude); summary(KEN11a_ASAL); exp(summary(KEN11a_ASAL)$coef[[1]])

KEN11a_nonASAL<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
                    +AvgTemp + SDTemp, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
                    data=ScaledTS[ScaledTS$ASAL==0,],na.action=na.exclude); summary(KEN11a_nonASAL); exp(summary(KEN11a_nonASAL)$coef[[1]])
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
KEN11a_checking<-lme(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec+Spell+Spell4 +MaxP
                     + SDTemp + DDays + HWDays+MaxT, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
            data=ScaledTS,na.action=na.exclude); summary(KEN11a_checking)

KEN11a_checking<-lme(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec+Spell+Spell4 +MaxP
                     + SDTemp + DDays +MaxT, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
                     data=ScaledTS,na.action=na.exclude); summary(KEN11a_checking)
AIC(KEN11a_checking)

KEN11a_checking<-lme(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec+Spell+Spell4 +MaxP
                     + SDTemp + DDays, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
                     data=ScaledTS,na.action=na.exclude);AIC(KEN11a_checking)

KEN11a_checking<-lme(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec+Spell+Spell4 +MaxP
                     + SDTemp , random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
                     data=ScaledTS,na.action=na.exclude);AIC(KEN11a_checking)


KEN11a_checking<-lme(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec+Spell+Spell4
                     + SDTemp , random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
                     data=ScaledTS,na.action=na.exclude);AIC(KEN11a_checking)

KEN11a_checking<-lme(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+CVPrec+Spell+Spell4
                     + SDTemp , random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
                     data=ScaledTS,na.action=na.exclude);AIC(KEN11a_checking)

AIC(KEN11e);AIC(KEN11d);AIC(KEN11a)

KEN11a_checkingg<-lme(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+CVPrec+Spell
                      + CVTempK     , random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
                     data=ScaledTS,na.action=na.exclude,method="ML");AIC(KEN11a_checkingg)

anova(KEN11a_checkingg,KEN11h)

cor.test(ScaledTS$AvgTemp,ScaledTS$MaxT)
cor.test(ScaledTS$AvgTemp,ScaledTS$HWDays)
cor.test(ScaledTS$AvgTemp,ScaledTS$SeasPr)
AIC(KEN11e);AIC(KEN11d);AIC(KEN11a)
# Prec2m  MaxP  DDays MaxT,
KEN11a_checkingg<-lme(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+ CVPrec+Spell+Spell4 +CVTempK+
                        HWDays, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
                     data=ScaledTS,na.action=na.exclude,method="ML");AIC(KEN11a_checkingg)

anova(KEN11a_checkingg,KEN11h)


AIC(KEN11e);AIC(KEN11d);AIC(KEN11a)
# Prec2m  MaxP  DDays MaxT,
KEN11a_checkingg<-lme(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+ CVPrec+Spell+Spell4 +CVTempK+
                        MaxT, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
                      data=ScaledTS,na.action=na.exclude,method="ML");AIC(KEN11a_checkingg)

anova(KEN11a_checkingg,KEN11h)


KEN11a_checkingg<-lme(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+ CVPrec+Spell+Spell4 +
                        MaxT, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
                      data=ScaledTS,na.action=na.exclude,method="ML");AIC(KEN11a_checkingg)

anova(KEN11a_checkingg,KEN11g)