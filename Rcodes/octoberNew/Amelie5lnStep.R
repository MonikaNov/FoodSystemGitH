rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDuni2<-c("/its-home.uscs.susx.ac.uk/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

library(dplyr); library(tseries); library(plm); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)
setwd(WDhome)
setwd(WDuni)
setwd("../foodSystems/dataFS")
load("Main/isdataTS.RData")
# load("../Rcodes/octoberNew/Amelie5lnStep.RData")

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

Amelie50ln<-lmer(log(isdataTS$Yield)~SeasPr+I(SeasPr^2)+ I(SeasPr*AvgTemp)+AvgTemp+       
                   Prec2m +CVPrec+Spell+Spell10+Spell20+MaxP+
                   SDTemp + DDays + HWDays + MaxT 
                 + (1+SeasPr+I(SeasPr^2)+ I(SeasPr*AvgTemp)+AvgTemp+       
                      Prec2m +CVPrec+Spell+Spell10+Spell20+MaxP+
                      SDTemp + DDays + HWDays + MaxT |ID1),data=isdataScTS)

summary(Amelie50ln)

Am50lnstep<-step(Amelie50ln, keep=attr(terms(Amelie50ln), "term.labels")[1:4])

get_model(Am50lnstep)
#----------------------------------------------------------------------------------------------------------------------------------------------------------------

#ok, only the single previous model took extremely long to estimate...how long would step take???->so maybe a bit less coefficient in random effects:
# and remove max T, cos its not in the best model sofar either...

Amelie51ln<-lmer(log(isdataTS$Yield)~SeasPr+I(SeasPr^2)+ I(SeasPr*AvgTemp)+AvgTemp+       
                   Prec2m +CVPrec+Spell+Spell10+Spell20+MaxP+
                   SDTemp + DDays + HWDays  
                 + (1+Prec2m +CVPrec+Spell+Spell10+Spell20+MaxP+
                      SDTemp + DDays + HWDays  |ID1),data=isdataScTS)

summary(Amelie51ln) # ok, thats shorter

Am51lnstep<-step(Amelie51ln, keep=attr(terms(Amelie51ln), "term.labels")[1:4])
Am51lnstep
get_model(Am51lnstep)
summary(get_model(Am51lnstep))

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
save.image("../Rcodes/octoberNew/Amelie5lnStep.RData")