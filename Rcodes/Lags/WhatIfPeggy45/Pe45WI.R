rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

# setwd(WDuni)
# setwd(WDhome)

library('dplyr')
library('tseries')
library(plm)
library(lme4)
library(lattice)
library(car)
library(lmerTest)
library(optimx)

load("Main/data.RData")

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# Peggy45 is the best at the moment.

Peggy45<-lmer(Yield ~ I((SeasRain_MAM_L1+SeasRain_OND_L1)*(1-west1)) + I((SeasRain_MAM_L1)*west1)+ I(AvgTemp_MarSep_L1*(1-west1)) + I(AvgTemp_MarSep_L1*(west1))
              +I(SDtemp_OctMar_L1*(1-west1))+    +I(SDtemp_OctMar_L1*west1)
              
              +   I(MaxRain_OND*(1-west1)) +   I(MaxRain_OND*west1) 
              
              +(1+MaxRain_OND+I(SeasRain_MAM_L1+SeasRain_OND_L1) +I(AvgTemp_MarSep_L1)|ID1) ,data=dataScTS )
summary(Peggy45)  
nobs(Peggy45)

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# now I will try to change the explanatory variables within + - 1  unit (I think that that would be interpreted as standard deviations of each variable) and find a max 
# and a min of yield within that change. Then I will do the same, not considering the change in max rain and sd and I will then show what is the additiional 
# value knowing max prec and sd:

# I think that it is ok to ignore the random effects part as it is assumed to have zero mean.. I think

# I have to do east and west separately as they have different coefficientss.

koefs<-as.numeric(summary(Peggy45)$coeff[,1])

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# 1 WEST:

oAllminW<-optimx(c(rep(0,4)), function(x,k=koefs[c(1,3,5,7,9)]) k[1]+sum(x*k[-1]), gr=NULL, hess=NULL, lower=c(rep(-1,4)), upper=c(rep(1,4)),
              method=c("Nelder-Mead","BFGS"), itnmax=NULL, hessian=FALSE,
              control=list()); oAllminW

oAllmaxW<-optimx(c(rep(0,4)), function(x,k=koefs[c(1,3,5,7,9)]) k[1]+sum(x*k[-1]), gr=NULL, hess=NULL, lower=c(rep(-1,4)), upper=c(rep(1,4)),
              method=c("Nelder-Mead","BFGS"), itnmax=NULL, hessian=FALSE,
              control=list(maximize =TRUE))
oAllmaxW;    oAllminW  

#   ..  .  . .  .  ..   ..  . .  . .. . . . .  . . . . . . . . . . 
# now max rain and sd restricted to 0
              
o2minW<-optimx(c(rep(0,2)), function(x,k=koefs[c(1,3,5)]) k[1]+sum(x*k[-1]), gr=NULL, hess=NULL, lower=c(rep(-1,2)), upper=c(rep(1,2)),
              method=c("Nelder-Mead","BFGS"), itnmax=NULL, hessian=FALSE,
              control=list())  ; o2minW

o2maxW<-optimx(c(rep(0,2)), function(x,k=koefs[c(1,3,5)]) k[1]+sum(x*k[-1]), gr=NULL, hess=NULL, lower=c(rep(-1,2)), upper=c(rep(1,2)),
              method=c("Nelder-Mead","BFGS"), itnmax=NULL, hessian=FALSE,
              control=list(maximize =TRUE))
o2maxW ;   o2minW  # and compare to previous unrestricted:
oAllmaxW;    oAllminW  

#   ..  .  . .  .  ..   ..  . .  . .. . . . .  . . . . . . . . . . 
# EAST: 

oAllminE<-optimx(c(rep(0,4)), function(x,k=koefs[c(1,2,4,6,8)]) k[1]+sum(x*k[-1]), gr=NULL, hess=NULL, lower=c(rep(-1,4)), upper=c(rep(1,4)),
              method=c("Nelder-Mead","BFGS"), itnmax=NULL, hessian=FALSE,
              control=list()) ; oAllminE

oAllmaxE<-optimx(c(rep(0,4)), function(x,k=koefs[c(1,2,4,6,8)]) k[1]+sum(x*k[-1]), gr=NULL, hess=NULL, lower=c(rep(-1,4)), upper=c(rep(1,4)),
              method=c("Nelder-Mead","BFGS"), itnmax=NULL, hessian=FALSE,
              control=list(maximize =TRUE))
oAllminE;oAllmaxE

#   ..  .  . .  .  ..   ..  . .  . .. . . . .  . . . . . . . . . . 
# now max rain and sd restricted to 0

o2minE<-optimx(c(rep(0,2)), function(x,k=koefs[c(1,2,4)]) k[1]+sum(x*k[-1]), gr=NULL, hess=NULL, lower=c(rep(-1,2)), upper=c(rep(1,2)),
              method=c("Nelder-Mead","BFGS"), itnmax=NULL, hessian=FALSE,
              control=list()); o2minE

o2maxE<-optimx(c(rep(0,2)), function(x,k=koefs[c(1,2,4)]) k[1]+sum(x*k[-1]), gr=NULL, hess=NULL, lower=c(rep(-1,2)), upper=c(rep(1,2)),
              method=c("Nelder-Mead","BFGS"), itnmax=NULL, hessian=FALSE,
              control=list(maximize =TRUE))
o2minE; o2maxE

#   ..  .  . .  .  ..   ..  . .  . .. . . . .  . . . . . . . . . ...  .  . .  .  ..   ..  . .  . .. . . . .  . . . . . . . . . ...  .  . .  .  ..   ..  . .  . .. . . . .  . . . . . . . . . ...  .  . .  .  ..   ..  . .  . .. . . . .  . . . . . . . . . .
# compare the models above again:

oAllminW; oAllmaxW   # unrestricted           # West
o2minW; o2maxW       # restricted

oAllminE; oAllmaxE    # unrestricted           #East
o2minE; o2maxE        # restricted
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
              
        
DataP45<-dataScTS[rownames(dataScTS)%in% rownames(  model.frame(Peggy45)),]
DataUsP45<-dataUsTS[rownames(dataUsTS)%in% rownames(  model.frame(Peggy45)),]

summary(DataUsP45$Yield)

mean(dataUsTS$Yield,na.rm=TRUE)
mean(dataUsTS$Yield,na.rm=TRUE)
mean(dataUsTS$Yield,na.rm=TRUE)
mean(dataUsTS$Yield,na.rm=TRUE)
mean(dataUsTS$Yield,na.rm=TRUE)
mean(dataUsTS$Yield,na.rm=TRUE)
mean(dataUsTS$Yield,na.rm=TRUE)
mean(dataUsTS$Yield,na.rm=TRUE)
sd(dataUsTS$Yield,na.rm=TRUE)

