rm(list=ls())
library(cvTools);library(MASS);library(dplyr);library(tseries); library(plm); library(nlme); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)
load("dataFS/Main/DaTS.RData")
load("Rcodes/crossvalidation/compareJustTotals3.RData")

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
CVbase3<-cvFit(KEN11dK_base,data=framik,y=log(framik$Yield0),K=1300,cost=mape);CVbase3;summary(CVbase3)
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
KEN11dK_base2<-lme(log(Yield0)~SeasPr+AvgTempK+I(SeasPr^2), random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
                  data=ScaledTS,na.action=na.omit); summary(KEN11dK_base2)

CVbase21<-cvFit(KEN11dK_base2,data=framik,y=log(framik$Yield0),K=1300);CVbase21;summary(CVbase21)
CVbase22<-cvFit(KEN11dK_base2,data=framik,y=log(framik$Yield0),K=1300,cost=rtmspe);CVbase22;summary(CVbase22)
CVbase23<-cvFit(KEN11dK_base2,data=framik,y=log(framik$Yield0),K=1300,cost=mape);CVbase23;summary(CVbase23)

save.image("~/FoodSystemGitH/Rcodes/crossvalidation/compareJustTotals2.RData")
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
KEN11dK_base3<-lme(log(Yield0)~SeasPr+AvgTempK, random= ~1 | ID1,
                  data=ScaledTS,na.action=na.omit); summary(KEN11dK_base3)
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
CVbase31<-cvFit(KEN11dK_base3,data=framik,y=log(framik$Yield0),K=1300);CVbase31;summary(CVbase31)
CVbase32<-cvFit(KEN11dK_base3,data=framik,y=log(framik$Yield0),K=1300,cost=rtmspe);CVbase32;summary(CVbase32)
CVbase33<-cvFit(KEN11dK_base3,data=framik,y=log(framik$Yield0),K=1300,cost=mape);CVbase33;summary(CVbase33)

save.image("~/FoodSystemGitH/Rcodes/crossvalidation/compareJustTotals2.RData")

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
KEN11dK_base4<-lme(log(Yield0)~SeasPr+AvgTempK+I(SeasPr^2), random= ~1 | ID1,
                   data=ScaledTS,na.action=na.omit); summary(KEN11dK_base4)

CVbase41<-cvFit(KEN11dK_base4,data=framik,y=log(framik$Yield0),K=1300);CVbase41;summary(CVbase41)
CVbase42<-cvFit(KEN11dK_base4,data=framik,y=log(framik$Yield0),K=1300,cost=rtmspe);CVbase42;summary(CVbase42)
CVbase43<-cvFit(KEN11dK_base4,data=framik,y=log(framik$Yield0),K=1300,cost=mape);CVbase43;summary(CVbase43)

save.image("~/FoodSystemGitH/Rcodes/crossvalidation/compareJustTotals3.RData")
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

save.image("~/FoodSystemGitH/Rcodes/crossvalidation/compareJustTotals.RData")