rm(list=ls())
library(MASS);library(dplyr); library(tseries); library(plm); library(nlme); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)
load("dataFS/Main/DaTS.RData")
# load("Rcodes/DecemberNew/KEN11d_re.RData")
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

KEN11d<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
            +AvgTemp + CVTempK, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
            data=ScaledTS,na.action=na.exclude); summary(KEN11d); exp(summary(KEN11d)$coef[[1]])

# - -  --  - - - - - - ------- - - - - - - - -- - - - -- - - - -- - - -- - - -  --  - - - - - - ------- - - - - - - - -- - - - -- - - - -- - - -- - - -  --  - - - - - - ------- - - - - - - - -- - - - -- - - - -- - - -- - 
KENd_lmer<-lmer(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4+AvgTemp + CVTempK+(1 | ID1),data=ScaledTS,na.action=na.exclude)
step(KENd_lmer)

KENd_lmer_reFull<-lmer(log(Yield0)~SeasPr+I(SeasPr^2)+AvgTemp +CVPrec+Spell+Spell4+ CVTempK
                       +(SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4+AvgTemp + CVTempK +MaxP+MaxT+ Prec2m+DDays +HWDays| ID1)
                       ,data=ScaledTS,na.action=na.exclude)
KENd_lmer_reFull_step<-step(KENd_lmer_reFull,reduce.fixed=FALSE)

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
save.image("Rcodes/DecemberNew/KEN11d_re_step.RData")