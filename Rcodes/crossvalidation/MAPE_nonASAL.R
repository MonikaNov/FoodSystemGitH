rm(list=ls())
library(cvTools);library(MASS);library(dplyr); library(ggeffects);library(tseries); library(plm); library(nlme); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)
load("dataFS/Main/DaTS.RData")
# load("Rcodes/DecemberNew/KEN11d_stepNice.RData")

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#
KEN11dK_nonASAL<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
                     +AvgTempK + CVTempK, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
                     data=ScaledTS[ScaledTS$ASAL==0,],na.action=na.exclude); summary(KEN11dK_nonASAL)

foo<-lme(Yield0~Year+ID1+SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
         +AvgTempK + CVTempK, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
         data=ScaledTS[ScaledTS$ASAL==0,],na.action=na.omit);


frnonASAL<-model.frame(foo,data=ScaledTS[ScaledTS$ASAL==0,])[complete.cases(model.frame(foo,data=ScaledTS[ScaledTS$ASAL==0,])),]
    #test:
        bar<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
             +AvgTempK + CVTempK, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
             data=frnonASAL,na.action=na.exclude);summary(bar)
        #v.good
        
MAPE_nonASAL<-cvFit(KEN11dK_nonASAL,data=frnonASAL,y=log(frnonASAL$Yield0),K=602,cost=mape);MAPE_nonASAL;summary(MAPE_nonASAL)

# and base

KEN11dK_nonASALb<-lme(log(Yield0)~SeasPr+AvgTempK , random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
                  data=ScaledTS[ScaledTS$ASAL==0,],na.action=na.exclude); summary(KEN11dK_nonASALb)
MAPE_nonASAL_base<-cvFit(KEN11dK_nonASALb,data=frnonASAL,y=log(frnonASAL$Yield0),K=602,cost=mape);MAPE_nonASAL_base;summary(MAPE_nonASAL_base)
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
save.image("~/FoodSystemGitH/Rcodes/crossvalidation/MAPE_nonASAL.RData")