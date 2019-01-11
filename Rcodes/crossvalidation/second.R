rm(list=ls())
library(cvTools);library(MASS);library(dplyr);library(tseries); library(plm); library(nlme); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)
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

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
CVfit4<-cvFit(KEN11dK,data=framik,y=log(framik$Yield0),K=1300);CVfit4;summary(CVfit4)
CVfit5<-cvFit(KEN11dK,data=framik,y=log(framik$Yield0),K=1300,cost=rtmspe);CVfit5;summary(CVfit5)
CVfit6<-cvFit(KEN11dK,data=framik,y=log(framik$Yield0),K=1300,cost=mape);CVfit6;summary(CVfit6)
CVfit7<-cvFit(KEN11dK,data=framik,y=log(framik$Yield0),K=1300,cost=mape);CVfit7;summary(CVfit7)
CVfit8<-cvFit(KEN11dK,data=framik,y=log(framik$Yield0),K=1300,cost=mape);CVfit8;summary(CVfit8)
summary(log(framik$Yield0))
hist(log(framik$Yield0))

save.image("\\\\smbhome.uscs.susx.ac.uk\\mn301\\FoodSystemGitH\\Rcodes\\crossvalidation\\KEN11d_stepNice.RData")
