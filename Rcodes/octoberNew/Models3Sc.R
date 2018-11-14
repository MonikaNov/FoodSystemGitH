rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma
# load("~/foodSystems/Rcodes/octoberNew/Models1.RData")

library(dplyr); library(tseries); library(plm); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)
setwd(WDhome)
load("Main/isdataTS.RData")
#           load("~/foodSystems/Rcodes/octoberNew/Models2Sc.RData")

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

#-----------------------------------------------------------------------------------------------------------------------------------------------------------

Amelie40ln<-lmer(log(isdataTS$Yield)~SeasPr+I(SeasPr^2)+ I(SeasPr*AvgTemp)+AvgTemp+       
                   Prec2m +CVPrec+Spell+Spell10+Spell20+MaxP+
                   SDTemp + DDays + HWDays + MaxT + (1|ID1),data=isdataScTS)


summary(Amelie40ln)