rm(list=ls())
setwd("foodSystems/dataFS") 
# load("../Rcodes/octoberNew/Models1.RData")

library(dplyr); library(tseries); library(plm); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)

load("Main/DaTS.RData")
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#

Emmit123ln<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+(1|ID1),data=ScaledTS) 
summary(Emmit123ln)
ranova(Emmit123ln) 


Kendalln0<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec+Spell+Spell4 +MaxP
                + SDTemp + DDays + HWDays+MaxT+(1|ID1),data=ScaledTS) # if without log, the random effects equation would be different, I think SeasPr would be there
summary(Kendalln0)
vif(Kendalln0)

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Kendalln1<-lmer(log(Yield0)~SeasPr+I(SeasPr^2)+AvgTemp+Prec2m+I(Prec2m^2)+CVPrec+Spell+Spell4 +MaxP
                + SDTemp + DDays + HWDays+MaxT+(1|ID1),data=ScaledTS);  summary(Kendalln1)
# nope, square of 2m prec no..

Kendalln2<-lmer(log(Yield0)~SeasPr+I(SeasPr^2)+AvgTemp+Prec2m+CVPrec+Spell+Spell4 +MaxP
                + SDTemp + DDays + HWDays+MaxT+(1|ID1),data=ScaledTS);  summary(Kendalln2)
vif(Kendalln2)


Kendalln2<-lmer(log(Yield0)~SeasPr+AvgTemp+Prec2m+CVPrec+Spell+Spell4 +MaxP
                + SDTemp + DDays + HWDays+MaxT+(1|ID1),data=ScaledTS);  summary(Kendalln2)
vif(Kendalln2)

Kendalln2<-lmer(log(Yield0)~SeasPr+AvgTemp+Prec2m+CVPrec+Spell+Spell4 +MaxP
                + SDTemp + DDays + HWDays+MaxT+(SeasPr+AvgTemp|ID1),data=ScaledTS);  summary(Kendalln2)
vif(Kendalln2)
ranova(Kendalln2,reduce.terms = TRUE)

Kendalln2<-lmer(log(Yield0)~SeasPr+I(SeasPr^2)+AvgTemp+Prec2m+CVPrec+Spell+Spell4 +MaxP
                + SDTemp + DDays + HWDays+MaxT+(1|ID1),data=ScaledTS);  summary(Kendalln2)
vif(Kendalln2)
ranova(Kendalln2,reduce.terms = TRUE)



# ok, I will try to remove the ones with high vifs............................................................................................


Kendalln0<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec+Spell+Spell4 +MaxP
                + SDTemp + DDays + HWDays+MaxT+(1|ID1),data=ScaledTS) 
summary(Kendalln0)
vif(Kendalln0)



Kendalln3<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec+Spell+Spell4 +MaxP
                + SDTemp + DDays + HWDays+(1|ID1),data=ScaledTS) 
summary(Kendalln3)
vif(Kendalln3)

Kendalln3<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec+Spell+Spell4 +MaxP
                + SDTemp  + HWDays+(1|ID1),data=ScaledTS) 
summary(Kendalln3)
vif(Kendalln3)

Kendalln3<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+CVPrec+Spell+Spell4 +MaxP
                + SDTemp  + HWDays+(1|ID1),data=ScaledTS) 
summary(Kendalln3)
vif(Kendalln3)


#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# ok, so far Kendalln3 the best.. try to replace seas pr. with 2 months.. or something..

Kendalln3<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+CVPrec+Spell+Spell4
                + SDTemp  + HWDays+(1|ID1),data=ScaledTS) 
summary(Kendalln3)
vif(Kendalln3)
    hist(ScaledTS$SeasPr,40)
    curve(exp(0.06198*x)*exp(-0.02927*(x^2)),-2,3 )