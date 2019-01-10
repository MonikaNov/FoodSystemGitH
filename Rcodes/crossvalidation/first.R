rm(list=ls())
library(cvTools);library(MASS);library(dplyr); library(ggeffects);library(tseries); library(plm); library(nlme); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)
load("dataFS/Main/DaTS.RData")
# load("Rcodes/DecemberNew/KEN11d_stepNice.RData")

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

# from the file 'Rcodes/DecemberNew/'.. the so far final version (7.1.2019):

ScaledTS[complete.cases(ScaledTS),]

dim(ScaledTS[complete.cases(ScaledTS),])

KEN11dK<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
             +AvgTempK + CVTempK, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
             data=ScaledTS,na.action=na.omit); summary(KEN11dK)
#trying:
            foo<-lme(Yield0~Year+ID1+SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
              +AvgTempK + CVTempK, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
               data=ScaledTS,na.action=na.omit); summary(KEN11dK)


        framik<-model.frame(foo,data=ScaledTS)[complete.cases(model.frame(foo,data=ScaledTS)),]
        KEN11dK_fr<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
                     +AvgTempK + CVTempK, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
                     data=framik,na.action=na.omit);summary(KEN11dK_fr)
        
        
        CVfit1<-cvFit(KEN11dK,data=ScaledTS,y=log(ScaledTS$Yield0))
        CVfit1<-cvFit(KEN11dK,data=ScaledTS[complete.cases(ScaledTS),])
        CVfit1<-cvFit(KEN11dK_fr,data=framik)
        
CVfit2<-cvFit(KEN11dK,data=ScaledTS[complete.cases(ScaledTS),],y=log(ScaledTS[complete.cases(ScaledTS),]$Yield0))
CVfit2;summary(CVfit2)

CVfit3<-cvFit(KEN11dK,data=framik,y=log(framik$Yield0));CVfit3
CVfit4<-cvFit(KEN11dK,data=framik,y=log(framik$Yield0),K=1300);CVfit4
#--  ---   --   --- -- - - -
predict(KEN11dK); summary(predict(KEN11dK)); length(predict(KEN11dK))
predict(KEN11dK,asList=TRUE); summary(unlist(predict(KEN11dK,asList=TRUE)))
summary(CVfit3)

# -- -- -- -- -
# ok, I will try to obtain the same prediction as the function predict...