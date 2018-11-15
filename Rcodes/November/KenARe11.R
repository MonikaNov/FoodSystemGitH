rm(list=ls())
library(dplyr); library(tseries); library(plm); library(nlme); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)
load("dataFS/Main/DaTS.RData")

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

# KenARe11 is the model which I presented on the meeting 14.11.2018
# arma(11) and ar(2) the best. AIC a bit better for 11>> claiming KenARe11 the best !!!


KenARe11<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
              +AvgTemp + SDTemp  + HWDays, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
              data=ScaledTS,na.action=na.exclude); summary(KenARe11)
acf(summary(KenARe11)$resid); pacf(summary(KenARe11)$resid)
anova(KenARe11)
vif(KenARe11)
exp(summary(KenARe11)$coef[[1]])

#----------------------------------------------------------------------------------------------------------------------------------------------

# so now try changes suggested during the meeting:

KEN11a<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
              +AvgTemp + SDTemp, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
              data=ScaledTS,na.action=na.exclude); summary(KEN11a)
nobs(KEN11a); anova(KEN11a)
summary(KEN11a)$tTable[,1]
summary(KEN11a)$tTable[,2]


                    KENDAL11a<-lmer(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
                                    +AvgTemp + SDTemp  +(1|ID1),data=ScaledTS) 
                    summary(KENDAL11a)
                    anova(KENDAL11a,type="I")
                          # Just playing:
                                KAL11a<-lm(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4+AvgTemp+SDTemp,data=ScaledTS)
                                anova(KAL11a)



KEN11b<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
            +AvgTemp + I(sign(AvgTemp)*(AvgTemp^2))+SDTemp, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
            data=ScaledTS,na.action=na.exclude); summary(KEN11b)  # ok, square not significant
