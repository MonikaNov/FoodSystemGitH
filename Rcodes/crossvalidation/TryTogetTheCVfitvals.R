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
        
        
CVfit9<-cvFit(KEN11dK_fr,data=framik,y=log(framik$Yield0),K=2,cost=mape,foldType="consecutive");CVfit9;summary(CVfit9)

myFolds<-cvFolds(1300, K=2,R=1)
myFolds2<-cvFolds(1300, K=2,R=1,type="consecutive")

framik2<-merge(framik,myFolds,by.y=V)

CVfit9<-cvFit(KEN11dK_fr,data=framik,y=log(framik$Yield0),K=2,cost=mape,folds=myFolds);CVfit9;summary(CVfit9)


MyFolds1<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
                +AvgTempK + CVTempK, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
                data=framik[myFolds$which==1,],na.action=na.omit);summary(MyFolds1)

MyPredicts2<-predict(MyFolds1,newdata=framik[myFolds$which==2,])
log(framik$Yield0[myFolds$which==2])
mean(abs(log(framik$Yield0[myFolds$which==2])-MyPredicts2))
# PERFECT, the same as CVfit9
#--  ---   --   --- -- - - -
predict(KEN11dK); summary(predict(KEN11dK)); length(predict(KEN11dK))
predict(KEN11dK,asList=TRUE); summary(unlist(predict(KEN11dK,asList=TRUE)))
summary(CVfit3)

# -- -- -- -- -
# ok, I will try to obtain the same prediction as the function predict...

summary(KEN11dK)
summary(KEN11dK)$coeff$random
i<-1
fixedfit<-summary(KEN11dK)$coeff$fixed[1]+summary(KEN11dK)$coeff$fixed[2]*ScaledTS$SeasPr[i]+
  summary(KEN11dK)$coeff$fixed[3]*ScaledTS$SeasPr[i]^2+summary(KEN11dK)$coeff$fixed[4]*ScaledTS$CVPrec[i]+
  summary(KEN11dK)$coeff$fixed[5]*ScaledTS$Spell[i]+summary(KEN11dK)$coeff$fixed[6]*ScaledTS$Spell4[i]+ 
  summary(KEN11dK)$coeff$fixed[7]*ScaledTS$AvgTempK[i]+ summary(KEN11dK)$coeff$fixed[8]*ScaledTS$CVTempK[i]
fixedfit+unlist(summary(KEN11dK)$coeff$random)[[1]]
predict(KEN11dK)[1] # GROOT