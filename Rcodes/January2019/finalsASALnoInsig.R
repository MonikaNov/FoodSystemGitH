rm(list=ls())
library(cvTools);library(MASS);library(dplyr); library(ggeffects);library(tseries); library(plm); library(nlme); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)
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
exp(summary(KEN11dK)$coef[[1]])
#---  -- --  --  -- -- -- -- -- -- -- - -- -- -- - -- - - - -- -- --  -- --  --  -- -- -- -- -- -- -- - -- -- -- - -- - - - -- -- --  -- --  --  -- -- -- -- -- -- -- - -- -- -- - -- - - - -- -- -
# now ASAL and nonASAL separately

KEN11dK_ASAL<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
                  +AvgTempK + CVTempK, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
                  data=ScaledTS[ScaledTS$ASAL==1,],na.action=na.exclude); summary(KEN11dK_ASAL)

anova(KEN11dK_ASAL,type="marginal")
exp(summary(KEN11dK_ASAL)$coef[[1]])


KEN11dK_nonASAL<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
                     +AvgTempK + CVTempK, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
                     data=ScaledTS[ScaledTS$ASAL==0,],na.action=na.exclude); summary(KEN11dK_nonASAL)

anova(KEN11dK_nonASAL,type="marginal")
exp(summary(KEN11dK_nonASAL)$coef[[1]])

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# Now I will try to remove insignificant variables from the ASAL and nonASAL counties to see if I get the same results

# 1.ASAL

KEN11dK_ASAL2<-lme(log(Yield0)~Spell+Spell4
                  +AvgTempK, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
                  data=ScaledTS[ScaledTS$ASAL==1,],na.action=na.exclude); summary(KEN11dK_ASAL2)

# yes, very similar results here.
#---  -- --  --  -- -- -- -- -- -- -- - -- -- -- - -- - - - -- -- --  -- --  --  -- -- -- -- -- -- -- - -- -- -- - -- - - - -- -- --  -- --  --  -- -- -- -- -- -- -- - -- -- -- - -- - - - -- -- -
# 2. non-ASAL

KEN11dK_nonASAL2<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec
                      + CVTempK, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
                     data=ScaledTS[ScaledTS$ASAL==0,],na.action=na.exclude); summary(KEN11dK_nonASAL2)
# kind of similar. CVprec at the boundary of significance. now I will try to remove it..

KEN11dK_nonASAL3<-lme(log(Yield0)~SeasPr+I(SeasPr^2)
                      + CVTempK, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
                      data=ScaledTS[ScaledTS$ASAL==0,],na.action=na.exclude); summary(KEN11dK_nonASAL3)

    # CVtempK now at the boundary..try to remove that?
            KEN11dK_nonASAL4<-lme(log(Yield0)~SeasPr+I(SeasPr^2),
                  random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
                  data=ScaledTS[ScaledTS$ASAL==0,],na.action=na.exclude); summary(KEN11dK_nonASAL4)
            
# I guess I can say approximately the same if insignif. removed. As for the non-ASAL, KEN11dK_nonASAL2 propably the most valid