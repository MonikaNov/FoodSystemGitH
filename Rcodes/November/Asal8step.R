rm(list=ls())
setwd("dataFS") 
library(dplyr); library(tseries); library(plm); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)

load("Main/DaTS.RData")
# load("../Rcodes/octoberNew/Models1.RData")

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# ok, so far Kendalln3 the best.. 

# HERE I WILL TRY TO ESTIMATE THE MODELS>> FIND THE BEST SUBSET BUT USING GROUPING INTO ASAL AND NON-ASAL INSTEAD OF HTE COUNTIES

Shiv0<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec+Spell+Spell4 +MaxP
                + SDTemp + DDays + HWDays+MaxT+(1|ASAL),data=ScaledTS) # if without log, the random effects equation would be different, I think SeasPr would be there
summary(Shiv0)

Shiv0step<-step(Shiv0, keep=attr(terms(Shiv0), "term.labels")[1:2])
Shiv0step<-step(Shiv0, ddf = c("Satterthwaite","Kenward-Roger"), alpha.random = 0.1, alpha.fixed = 0.05,
     reduce.fixed = TRUE, reduce.random = TRUE)
summary(get_model(Shiv0step))

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
load("../Rcodes/November/Models7.RData")
#  Shiv12 should be loaded:
           # Shiv12<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec+Spell+Spell4 +MaxP+ SDTemp + DDays + HWDays+MaxT+(SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec+Spell+Spell4 +MaxP + SDTemp + DDays + HWDays+MaxT+I(SeasPr^2)+Prec2m|ASAL),data=ScaledTS) 
summary(Shiv12); vif(Shiv12); ranova(Shiv12) #

Shiv12step<-step(Shiv12, ddf = c("Satterthwaite","Kenward-Roger"), alpha.random = 0.1, alpha.fixed = 0.05,
                reduce.fixed = TRUE, reduce.random = TRUE)
summary(get_model(Shiv12step))
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Shiv12s<-get_model(Shiv12step)
ranova(Shiv12s);vif(Shiv12s)
Shiv12s<-lmer(log(Yield0) ~ SeasPr + AvgTemp + Prec2m + CVPrec + DDays + MaxT + 
  (I(SeasPr^2) + Prec2m + Spell4 | ASAL),data=ScaledTS)

Shs<-lmer(log(Yield0) ~ SeasPr + AvgTemp + Prec2m + CVPrec + DDays + MaxT + 
                (Prec2m + Spell4 | ASAL),data=ScaledTS)
summary(Shs);ranova(Shs);vif(Shs)# good, the only problem are vifs

Shs2<-lmer(log(Yield0) ~ SeasPr + AvgTemp + Prec2m + CVPrec + DDays+
            (Prec2m + Spell4 | ASAL),data=ScaledTS)
summary(Shs2);ranova(Shs2);vif(Shs2)

Shs3<-lmer(log(Yield0) ~ SeasPr + AvgTemp + Prec2m + CVPrec +
             (Prec2m + Spell4 | ASAL),data=ScaledTS)
summary(Shs3);ranova(Shs3);vif(Shs3)
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

save.image("../Rcodes/November/Asal8step.RData")