rm(list=ls())
library(dplyr); library(tseries); library(plm); library(nlme); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)
load("dataFS/Main/DaTS.RData")
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
pbltest(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4+AvgTemp + CVTempK  + HWDays,data=ScaledTS,alternative='onesided')#ok, significant
pbltest(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4+AvgTempK + CVTempK  + HWDays,data=ScaledTS,alternative='onesided')#ok, significant


KEN11d<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
            +AvgTemp + CVTempK, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
            data=ScaledTS,na.action=na.exclude); summary(KEN11d); exp(summary(KEN11d)$coef[[1]])



KEN00d<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
            +AvgTemp + CVTempK, random= ~1 | ID1,
            data=ScaledTS,na.action=na.exclude); summary(KEN00d); exp(summary(KEN00d)$coef[[1]])

anova(KEN11d,KEN00d)

KEN10d<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
            +AvgTemp + CVTempK, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=0),
            data=ScaledTS,na.action=na.exclude);

KEN01d<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
            +AvgTemp + CVTempK, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=0,q=1),
            data=ScaledTS,na.action=na.exclude);

KEN02d<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
            +AvgTemp + CVTempK, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=0,q=2),
            data=ScaledTS,na.action=na.exclude);


KEN20d<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
            +AvgTemp + CVTempK, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=2,q=0),
            data=ScaledTS,na.action=na.exclude);


KEN21d<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
            +AvgTemp + CVTempK, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=2,q=1),
            data=ScaledTS,na.action=na.exclude);


KEN12d<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
            +AvgTemp + CVTempK, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=2),
            data=ScaledTS,na.action=na.exclude);

KEN22d<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
            +AvgTemp + CVTempK, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=2,q=2),
            data=ScaledTS,na.action=na.exclude);


KEN31d<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
            +AvgTemp + CVTempK, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=2,q=1),
            data=ScaledTS,na.action=na.exclude);


KEN13d<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
            +AvgTemp + CVTempK, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=2),
            data=ScaledTS,na.action=na.exclude);


KEN32d<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
            +AvgTemp + CVTempK, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=3,q=2),
            data=ScaledTS,na.action=na.exclude);

KEN23d<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
            +AvgTemp + CVTempK, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=2,q=3),
            data=ScaledTS,na.action=na.exclude);

KEN33d<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
            +AvgTemp + CVTempK, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=3,q=3),
            data=ScaledTS,na.action=na.exclude);
anova(KEN00d,KEN10d,KEN01d,KEN11d,KEN21d,KEN12d,KEN22d,KEN23d,KEN32d)

anova(KEN00d,KEN11d); anova(KEN10d,KEN11d);anova(KEN01d,KEN11d)
anova(KEN21d,KEN11d); anova(KEN12d,KEN11d);anova(KEN22d,KEN11d);
anova(KEN31d,KEN11d); anova(KEN13d,KEN11d);
anova(KEN23d,KEN11d);anova(KEN32d,KEN11d);anova(KEN33d,KEN11d)