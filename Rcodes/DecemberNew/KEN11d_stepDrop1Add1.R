rm(list=ls())
library(MASS);library(dplyr); library(tseries); library(plm); library(nlme); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)
load("dataFS/Main/DaTS.RData")
# load("Rcodes/DecemberNew/KEN11d_stepNice.RData")
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# these models are the best ones for now:

KEN11d<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
            +AvgTemp + CVTempK, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
            data=ScaledTS,na.action=na.exclude); summary(KEN11d); exp(summary(KEN11d)$coef[[1]])
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# drop1 undefined for restricted ML, have to refit with ML

KEN11d_ML<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
            +AvgTemp + CVTempK, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
            data=ScaledTS,na.action=na.exclude,method="ML"); summary(KEN11d_ML); exp(summary(KEN11d_ML)$coef[[1]])

drop1_KEN11d_ML<-drop1(KEN11d_ML)

scopefullish=formula(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
                     +AvgTemp + CVTempK+I(AvgTemp^2)+ Prec2m +MaxP + DDays,data=ScaleTS)

add1_KEN11d_ML<-add1(KEN11d_ML,scope=scopefullish)

# what about drop1 and lmer?
KENd_lmer<-lmer(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4+AvgTemp + CVTempK+(1 | ID1),data=ScaledTS,na.action=na.exclude)
drop1(KENd_lmer)
anova(KENd_lmer)
add1(KENd_lmer,scope=scopefullish)

#----  ---  --   ----      -----     -----    ---  ----   ----       ----      ------        ---- -- - - - - -   - -- - - - - -- - - - - -- - - -- - - - -- - - - -- - -- -- - - --- - -- - - - -- - - 
# now add1 full..
scopefull=formula(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
                     +AvgTemp + CVTempK+I(AvgTemp^2)+ Prec2m +MaxP + DDays+HWDays+MaxT,data=ScaleTS)

add1_KEN11d_ML2<-add1(KEN11d_ML,scope=scopefull)
add1_KEN11d_ML2
add1(KENd_lmer,scope=scopefull)
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
save.image("Rcodes/DecemberNew/KEN11d_stepDrop1Add1.RData")