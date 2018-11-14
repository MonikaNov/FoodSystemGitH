rm(list=ls())
setwd("foodSystems/dataFS") 
# load("../Rcodes/octoberNew/Models1.RData")

library(dplyr); library(tseries); library(plm); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)

load("Main/DaTS.RData")
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# ok, so far Kendalln3 the best.. try to replace seas pr. with 2 months.. or something..

Kendalln0<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec+Spell+Spell4 +MaxP
                + SDTemp + DDays + HWDays+MaxT+(1|ID1),data=ScaledTS) # if without log, the random effects equation would be different, I think SeasPr would be there
summary(Kendalln0)
vif(Kendalln0)

Kendalln3<-lmer(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
                +AvgTemp + SDTemp  + HWDays+(1|ID1),data=ScaledTS) 
summary(Kendalln3)
vif(Kendalln3)

#----------------------------------------------------------------------------------------------------------------------------------------------
# so I can try to put the interaction into Kendalln3 immediately or into Kendalln0 and try to find a new subset.
# maybe I will start with Kendalln3...

Kendalln5<-lmer(log(Yield0)~SeasPr+I(SeasPr^2) +AvgTemp+I(SeasPr*AvgTemp)+CVPrec+Spell+Spell4
                + SDTemp  + HWDays+(1|ID1),data=ScaledTS) 
summary(Kendalln5)
vif(Kendalln5)

# --now the other variant:

Kendalln05<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr*AvgTemp)+I(SeasPr^2)+ Prec2m+CVPrec+Spell+Spell4 +MaxP
                + SDTemp + DDays + HWDays+MaxT+(1|ID1),data=ScaledTS) #
summary(Kendalln05)
vif(Kendalln05)

Kendalln05<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr*AvgTemp)+Prec2m+CVPrec+Spell+Spell4 +MaxP
                 + SDTemp + DDays + HWDays+MaxT+(1|ID1),data=ScaledTS) # if without log, the random effects equation would be different, I think SeasPr would be there
summary(Kendalln05)
vif(Kendalln05)  # I would say no for the interaction. It is correlated with SeasPrecip obviously and it is making precip insignificant and that is not right.
# I may also try ASAL and non ASAL separately

  Kendalln051<-lmer(log(Yield0)~SeasPr+I(SeasPr^2)+AvgTemp+I(SeasPr*AvgTemp)+Prec2m+CVPrec+Spell+Spell4 +MaxP
                 + SDTemp + DDays + HWDays+MaxT+(1|ID1),data=ScaledTS[ScaledTS$ASAL==TRUE,])
  summary(Kendalln051); vif(Kendalln051); nobs(Kendalln051) ;
      # nope
  
  Kendalln51<-lmer(log(Yield0)~SeasPr +AvgTemp+I(SeasPr*AvgTemp)+CVPrec+Spell+Spell4
                  + SDTemp  + HWDays+(1|ID1),data=ScaledTS[ScaledTS$ASAL==TRUE,]) 
  summary(Kendalln51); vif(Kendalln51); nobs(Kendalln51)
  #no