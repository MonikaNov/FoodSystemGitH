rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)
library(plm)
library(lme4)
library(boot)
library(lmerTest)
library(itsadug) # for acf of lmer objects
library(lattice) # lme4 plots
load("Main/Phase23.RData")

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


Phase23ts<-pdata.frame(Phase23,index=c("CountyID","T"))
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# save game

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

fm1<-lmer(Reaction~Days+(Days|Subject),sleepstudy)
summary(fm1)
confint(fm1, method="Wald")
help()
confint(fm1, method="profile")
confint(fm1, method="boot")
profile(fm1)
xyplot(pr<-profile(fm1))

print(vc <- VarCorr(fm1), comp = c("Variance", "Std.Dev."))
nobs(fm1)
ngrps(fm1)
sigma(fm1)