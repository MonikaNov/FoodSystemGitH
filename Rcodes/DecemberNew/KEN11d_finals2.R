rm(list=ls())
library(MASS);library(dplyr); library(tseries); library(plm); library(nlme); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)
load("dataFS/Main/DaTS.RData")
# load("Rcodes/DecemberNew/KEN11d_stepNice.RData")
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# these models are the best ones for now:

KEN11d<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
            +AvgTemp + CVTempK, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
            data=ScaledTS,na.action=na.exclude); summary(KEN11d)
anova(KEN11d,type="marginal")

# how does it look if I replace temperature in Celsius with temperature in Kelvin??
KEN11dK<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
              +AvgTempK + CVTempK, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
              data=ScaledTS,na.action=na.exclude); summary(KEN11dK)
    # quite similar, but not yet completely the same...

anova(KEN11dK,type="marginal")

#---  -- --  --  -- -- -- -- -- -- -- - -- -- -- - -- - - - -- -- --  -- --  --  -- -- -- -- -- -- -- - -- -- -- - -- - - - -- -- --  -- --  --  -- -- -- -- -- -- -- - -- -- -- - -- - - - -- -- -
# now ASAL and nonASAL separately

KEN11dK_ASAL<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
             +AvgTempK + CVTempK, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
             data=ScaledTS[ScaledTS$ASAL==1,],na.action=na.exclude); summary(KEN11dK_ASAL)

anova(KEN11dK_ASAL,type="marginal")


KEN11dK_nonASAL<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
                  +AvgTempK + CVTempK, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
                  data=ScaledTS[ScaledTS$ASAL==0,],na.action=na.exclude); summary(KEN11dK_nonASAL)

anova(KEN11dK_nonASAL,type="marginal")