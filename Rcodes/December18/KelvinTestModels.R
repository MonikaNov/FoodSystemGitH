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
cor.test(ScaledTS$CVTempK,ScaledTS$AvgTemp)
cor.test(ScaledTS$SDTemp,ScaledTS$AvgTemp)
cor.test(ScaledTS$SDTemp,ScaledTS$CVTempK)

plot(ScaledTS$SDTemp,ScaledTS$CVTempK)
plot(ScaledTS$CVTempK~ScaledTS$AvgTemp)

vif(KEN11d)
vif(KEN11a)

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

# now I can try to remove the SD or CV of temp and try anova

KEN11e<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
            +AvgTemp, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
            data=ScaledTS,na.action=na.exclude); summary(KEN11e); exp(summary(KEN11e)$coef[[1]])
# and subsamples
KEN11g<-update(KEN11e,.~.,method="ML")
summary(KEN11g)

anova(KEN11e,KEN11a); anova(KEN11e,KEN11d)
compareFits(KEN11e,KEN11a)
# and subsamples
KEN11e_ASAL<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
                 +AvgTemp, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
                 data=ScaledTS[ScaledTS$ASAL==1,],na.action=na.exclude); summary(KEN11e_ASAL); exp(summary(KEN11e_ASAL)$coef[[1]])
anova(KEN11e_ASAL,KEN11a_ASAL); anova(KEN11e_ASAL,KEN11d_ASAL)

KEN11e_nonASAL<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
                    +AvgTemp, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
                    data=ScaledTS[ScaledTS$ASAL==0,],na.action=na.exclude); summary(KEN11e_nonASAL); exp(summary(KEN11e_nonASAL)$coef[[1]])

anova(KEN11e_nonASAL,KEN11a_nonASAL); anova(KEN11e_nonASAL,KEN11d_nonASAL)