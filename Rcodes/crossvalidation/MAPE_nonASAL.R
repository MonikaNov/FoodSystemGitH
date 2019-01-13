rm(list=ls())
library(cvTools);library(MASS);library(dplyr); library(ggeffects);library(tseries); library(plm); library(nlme); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)
load("dataFS/Main/DaTS.RData")
# load("Rcodes/DecemberNew/KEN11d_stepNice.RData")
load("~/FoodSystemGitH/Rcodes/crossvalidation/MAPE_nonASAL.RData")

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
        frnonASAL<-pdata.frame(frnonASAL,index=c("ID1","Year"))
        Yield0lag<-lag(frnonASAL$Yield0)
        frnonASAL<-cbind.data.frame(frnonASAL,Yield0lag)
        frnonASAL$Yield0lag[frnonASAL$Year==1981]<-NA
        #ehm..malformed factor..:
        frnonASAL$Year<-as.numeric(frnonASAL$Year);frnonASAL$Year<-as.factor(frnonASAL$Year)
        frnonASAL$ID1<-as.numeric(frnonASAL$ID1);frnonASAL$ID1<-as.factor(frnonASAL$ID1)
        mean(abs(log(frnonASAL$Yield0)-log(frnonASAL$Yield0lag)),na.rm=TRUE) 
# and base

KEN11dK_nonASALb<-lme(log(Yield0)~SeasPr+AvgTempK , random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
                  data=ScaledTS[ScaledTS$ASAL==0,],na.action=na.exclude); summary(KEN11dK_nonASALb)
MAPE_nonASAL_base<-cvFit(KEN11dK_nonASALb,data=frnonASAL,y=log(frnonASAL$Yield0),K=602,cost=mape);MAPE_nonASAL_base;summary(MAPE_nonASAL_base)
  
  MAPE_nonASAL$cv/IQR(log(frnonASAL$Yield0))
  KEN11dK_nonASALb$cv/IQR(log(frnonASAL$Yield0))
  mean(abs(log(frnonASAL$Yield0)-log(frnonASAL$Yield0lag)),na.rm=TRUE) /IQR(log(frnonASAL$Yield0))
  #oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
save.image("~/FoodSystemGitH/Rcodes/crossvalidation/MAPE_nonASAL.RData")