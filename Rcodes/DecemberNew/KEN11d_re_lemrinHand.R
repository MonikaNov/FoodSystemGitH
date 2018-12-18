rm(list=ls())
library(MASS);library(dplyr); library(tseries); library(plm); library(nlme); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)
load("dataFS/Main/DaTS.RData")
# load("Rcodes/DecemberNew/KEN11d_re.RData")
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# these models are the best ones for now:

KEN11d<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
            +AvgTemp + CVTempK, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
            data=ScaledTS,na.action=na.exclude); summary(KEN11d); exp(summary(KEN11d)$coef[[1]])

KENd_lmer<-lmer(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4+AvgTemp + CVTempK+(1 | ID1),data=ScaledTS,na.action=na.exclude)

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

KENd_lmer_reFullish<-lmer(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4+AvgTemp + CVTempK+
                            (1+SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4+AvgTemp + CVTempK | ID1),data=ScaledTS,na.action=na.exclude)

anova(KENd_lmer,KENd_lmer_reFullish)

KENd_lmer_re2<-lmer(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4+AvgTemp + CVTempK+
                            (1+SeasPr+AvgTemp + CVTempK | ID1),data=ScaledTS,na.action=na.exclude)

anova(KENd_lmer,KENd_lmer_re2)

KENd_lmer_re2<-lmer(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4+AvgTemp + CVTempK+
                      (1+SeasPr+AvgTemp  | ID1),data=ScaledTS,na.action=na.exclude)

anova(KENd_lmer,KENd_lmer_re2)



KENd_lmer_re2<-lmer(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4+AvgTemp + CVTempK+
                      (1+CVTempK+AvgTemp  | ID1),data=ScaledTS,na.action=na.exclude)

anova(KENd_lmer,KENd_lmer_re2)


KENd_lmer_re2<-lmer(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4+AvgTemp + CVTempK+
                      (1+CVTempK+SeasPr  | ID1),data=ScaledTS,na.action=na.exclude)

anova(KENd_lmer,KENd_lmer_re2)

KENd_lmer_re2<-lmer(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4+AvgTemp + CVTempK+
                      (1+SeasPr+I(SeasPr^2)  | ID1),data=ScaledTS,na.action=na.exclude)

anova(KENd_lmer,KENd_lmer_re2)


KENd_lmer_re2<-lmer(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4+AvgTemp + CVTempK+
                      (1+I(SeasPr^2)  | ID1),data=ScaledTS,na.action=na.exclude)

anova(KENd_lmer,KENd_lmer_re2)


KENd_lmer_re2<-lmer(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4+AvgTemp + CVTempK+
                      (1+Prec2m  | ID1),data=ScaledTS,na.action=na.exclude)

anova(KENd_lmer,KENd_lmer_re2)