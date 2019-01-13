rm(list=ls())
library(MASS);library(dplyr); library(ggeffects);library(tseries); library(plm); library(nlme); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)
load("dataFS/Main/DaTS.RData")
# load("Rcodes/DecemberNew/KEN11d_stepNice.RData")

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#

KEN11dK_ASAL<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
                  +AvgTempK + CVTempK, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
                  data=ScaledTS[ScaledTS$ASAL==1,],na.action=na.exclude); summary(KEN11dK_ASAL)

foo<-lme(Yield0~Year+ID1+SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
         +AvgTempK + CVTempK, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
         data=ScaledTS[ScaledTS$ASAL==1,],na.action=na.omit);


frASAL<-model.frame(foo,data=ScaledTS[ScaledTS$ASAL==1,])[complete.cases(model.frame(foo,data=ScaledTS[ScaledTS$ASAL==1,])),]
    #test:
        bar<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
             +AvgTempK + CVTempK, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
             data=frASAL,na.action=na.exclude);summary(bar)
        #v.good
        
MAPE_ASAL<-cvFit(KEN11dK_ASAL,data=frASAL,y=log(frASAL$Yield0),K=698,cost=mape);MAPE_ASAL;summary(MAPE_ASAL)

# and base

KEN11dK_ASALb<-lme(log(Yield0)~SeasPr+AvgTempK , random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
                  data=ScaledTS[ScaledTS$ASAL==1,],na.action=na.exclude); summary(KEN11dK_ASALb)
MAPE_ASAL_base<-cvFit(KEN11dK_ASALb,data=frASAL,y=log(frASAL$Yield0),K=698,cost=mape);MAPE_ASAL_base;summary(MAPE_ASAL_base)
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
save.image("~/FoodSystemGitH/Rcodes/crossvalidation/MAPE_ASAL.RData")