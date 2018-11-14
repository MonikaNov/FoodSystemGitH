rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDuni2<-c("/its-home.uscs.susx.ac.uk/home/mn301/Documents") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma
# load("~/foodSystems/Rcodes/octoberNew/Models1.RData")

library(dplyr); library(tseries); library(plm); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)
setwd(WDhome)
setwd(WDuni2)
setwd("../foodSystems/dataFS")
load("Main/isdataTS.RData")

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#Amelie 4ln is the best, so far..

Amelie4ln<-lmer(log(isdataTS$Yield)~SeasPr+I(SeasPr^2)+ I(SeasPr*AvgTemp)+AvgTemp+(1|ID1),data=isdataScTS)
summary(Amelie4ln)
coef(Amelie4ln)
ranova(Amelie4ln) 

Amelie4<-lmer(isdataTS$Yield~SeasPr+I(SeasPr^2)+ I(SeasPr*AvgTemp)+AvgTemp+(1|ID1),data=isdataScTS)
summary(Amelie4)
coef(Amelie4)
coef(Amelie4ln,Amelie4)

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Amelie40ln<-lmer(log(isdataTS$Yield)~SeasPr+I(SeasPr^2)+ I(SeasPr*AvgTemp)+AvgTemp+       
   Prec2m +CVPrec+Spell+Spell10+Spell20+MaxP+
   SDTemp + DDays + HWDays + MaxT + (1|ID1),data=isdataScTS)

summary(Amelie40ln)

Am40lnstep<-step(Amelie40ln, keep=attr(terms(Amelie40ln), "term.labels")[1:4])
# in a separate file (Amelie5lnStep) I will do the stepwise and try to eliminate random effects...
get_model(Am40lnstep)
summary(get_model(Am40lnstep))
vif(get_model(Am40lnstep)) # so maybe remove MaxT???
plot(Yield~MaxT, data=isdataScTS)  # ehm.. does not look correlated positively with yields at all..also vif very big..maybe not include at all at start?
coef(get_model(Am40lnstep))
print(Am40lnstep)

exp(0.1737*2.8051)*exp(-0.07622*(2.8051)^2) # too much rain is not good either

exp(0.1737*x)*exp(-0.07622*(x)^2)

plot(function(x) exp(0.1737*x)*exp(-0.07622*(x)^2),-1.7,2.806    )

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# so now without maxT

Amelie41ln<-lmer(log(isdataTS$Yield)~SeasPr+I(SeasPr^2)+ I(SeasPr*AvgTemp)+AvgTemp+       
                   Prec2m +CVPrec+Spell+Spell10+Spell20+MaxP+
                   SDTemp + DDays + HWDays + (1|ID1),data=isdataScTS)

summary(Amelie41ln)
Am41lnstep<-step(Amelie41ln, keep=attr(terms(Amelie41ln), "term.labels")[1:4])
AmBEST<-get_model(Am41lnstep)  # so far the best 30.10.2018
summary(AmBEST)
xyplot(profile(AmBEST)) #looks linear/quadratic>>good
coef(AmBEST)
vif(AmBEST)
anova(AmBEST,get_model(Am40lnstep))
#-----------------------------------------
# robustness - without the log....
Amelie40ln<-lmer(Yield~SeasPr+ SeasPr + I(SeasPr^2) + I(SeasPr * AvgTemp) 
                 +      AvgTemp + CVPrec + SDTemp + (1|ID1),data=isdataScTS)

summary(Amelie40ln)  # LOOKING GOOD

# and not scaled 

Lie40ln<-lmer(log(Yield)~SeasPr+ SeasPr + I(SeasPr^2) + I(SeasPr * AvgTemp) 
                 +      AvgTemp + CVPrec + SDTemp + (1|ID1),data=isdataTS)

summary(Lie40ln) 
vif(Lie40ln) 
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
save.image("../Rcodes/octoberNew/Amelie4lnStep.RData")