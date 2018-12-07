rm(list=ls())
library(dplyr); library(tseries); library(plm); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)
setwd("~/FoodSystemGitH")
load("dataFS/Main/DaTS.RData")

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

Kendalln3<-lmer(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
                +AvgTemp + SDTemp +(1|ID1),data=ScaledTS) 
summary(Kendalln3); anova(Kendalln3)
vif(Kendalln3)

      KenUSl3<-lmer(log(Yield)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
                      +AvgTemp + SDTemp +(1|ID1),data=DaTS) 
      summary(KenUSl3); anova(KenUSl3)
      
      KenUSl4<-lmer(log(Yield)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
                    +AvgTemp + SDTemp +(1|ID1),data=DaTS[DaTS$ASAL==1,]) 
      summary(KenUSl4); anova(KenUSl4)
      
      KenUSl5<-lmer(log(Yield)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
                    +AvgTemp + SDTemp +(1|ID1),data=DaTS[DaTS$ASAL==0,]) 
      summary(KenUSl5); 
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

anova(Kendalln3)

xyplot(profile(Kendalln3)) #looks linear/quadratic>>good ?? not sure about interpretation of this, though...
coef(Kendalln3)
ranef(Kendalln3)

plot(Kendalln3)
plot(Kendalln3,type=c("p","smooth")) # maybe remove the extreme resid obervation???
plot(Kendalln3,sqrt(abs(resid(.)))~fitted(.), type=c("p","smooth"))
sqrt(abs(resid(.)))
qqmath(Kendalln3,id=0.05)

plot(resid(Kendalln3,type="pearson"))
which(resid(Kendalln3,type="pearson")<(-6))
which(resid(Kendalln3,type="pearson")<(-2))
# yep, the same as in the previous case...
which(resid(AmBEST,type="pearson")<(-6))
# test of AR(1) and MA(1) errors
pbltest(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4+AvgTemp + SDTemp  + HWDays,data=ScaledTS,alternative='onesided')#ok, significant

pbltest(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4+AvgTemp + SDTemp  ,data=ScaledTS,alternative='onesided')#ok, significant