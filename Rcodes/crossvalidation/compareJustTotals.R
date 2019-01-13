rm(list=ls())
library(cvTools);library(MASS);library(dplyr);library(tseries); library(plm); library(nlme); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)
load("dataFS/Main/DaTS.RData")
# load("Rcodes/DecemberNew/KEN11d_stepNice.RData")
# load("Rcodes/crossvalidation/KEN11d_stepNice.RData")

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

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
CVfit4<-cvFit(KEN11dK,data=framik,y=log(framik$Yield0),K=1300);CVfit4;summary(CVfit4)
CVfit5<-cvFit(KEN11dK,data=framik,y=log(framik$Yield0),K=1300,cost=rtmspe);CVfit5;summary(CVfit5)
CVfit6<-cvFit(KEN11dK,data=framik,y=log(framik$Yield0),K=1300,cost=mape);CVfit6;summary(CVfit6)
CVfit7<-cvFit(KEN11dK,data=framik,y=log(framik$Yield0),K=1300,cost=mspe);CVfit7;summary(CVfit7)
CVfit8<-cvFit(KEN11dK,data=framik,y=log(framik$Yield0),K=1300,cost=tmspe);CVfit8;summary(CVfit8)
summary(log(framik$Yield0))
hist(log(framik$Yield0))
IQR(log(framik$Yield0))
max(framik$Yield0)-min(framik$Yield0)

CVfit5$cv/IQR(log(framik$Yield0))
CVfit5$cv/mean(log(framik$Yield0))
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# now I want to compare the prediction error to models which just include the seasonal totals (averages)
KEN11dK_base<-lme(log(Yield0)~SeasPr+AvgTempK, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
             data=ScaledTS,na.action=na.omit); summary(KEN11dK_base)
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
CVbase1<-cvFit(KEN11dK_base,data=framik,y=log(framik$Yield0),K=1300);CVbase1;summary(CVbase1)
CVbase2<-cvFit(KEN11dK_base,data=framik,y=log(framik$Yield0),K=1300,cost=rtmspe);CVbase2;summary(CVbase2)

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

save.image("~/FoodSystemGitH/Rcodes/crossvalidation/compareJustTotals.RData")