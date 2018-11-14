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
# Peggy453 unscaled. (Peggy45 is the best at the moment.)



Peggy453<-lmer(Yield ~ I((SeasRain_MAM_L1+SeasRain_OND_L1)*(1-west1)) + I((SeasRain_MAM_L1)*west1)+ I(AvgTemp_MarSep_L1*(1-west1)) + I(AvgTemp_MarSep_L1*(west1))
               +I(SDtemp_OctMar_L1*(1-west1))+    +I(SDtemp_OctMar_L1*west1)
               
               +   I(MaxRain_OND*(1-west1)) +   I(MaxRain_OND*west1) 
               
               +(1+MaxRain_OND+I(SeasRain_MAM_L1+SeasRain_OND_L1) +I(AvgTemp_MarSep_L1)|ID1) ,data=dataUsTS )
summary(Peggy453) 
nobs(Peggy453)

#  koefs<-as.numeric(summary(Peggy45)$coeff[,1]) ; koefs
koefs<-as.numeric(summary(Peggy453)$coeff[,1])  ; koefs

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# 1 WEST:

uAllminW<-optimx(c(rep(0,4)), function(x,k=koefs[c(1,3,5,7,9)]) k[1]+sum(x*k[-1]), gr=NULL, hess=NULL, lower=c(rep(-1,4)), upper=c(rep(1,4)),
                 method=c("Nelder-Mead","BFGS"), itnmax=NULL, hessian=FALSE,
                 control=list()); uAllminW

uAllmaxW<-optimx(c(rep(0,4)), function(x,k=koefs[c(1,3,5,7,9)]) k[1]+sum(x*k[-1]), gr=NULL, hess=NULL, lower=c(rep(-1,4)), upper=c(rep(1,4)),
                 method=c("Nelder-Mead","BFGS"), itnmax=NULL, hessian=FALSE,
                 control=list(maximize =TRUE))
uAllmaxW;    uAllminW  

#   ..  .  . .  .  ..   ..  . .  . .. . . . .  . . . . . . . . . . 
# now max rain and sd restricted to 0

u2minW<-optimx(c(rep(0,2)), function(x,k=koefs[c(1,3,5)]) k[1]+sum(x*k[-1]), gr=NULL, hess=NULL, lower=c(rep(-1,2)), upper=c(rep(1,2)),
               method=c("Nelder-Mead","BFGS"), itnmax=NULL, hessian=FALSE,
               control=list())  ; u2minW

u2maxW<-optimx(c(rep(0,2)), function(x,k=koefs[c(1,3,5)]) k[1]+sum(x*k[-1]), gr=NULL, hess=NULL, lower=c(rep(-1,2)), upper=c(rep(1,2)),
               method=c("Nelder-Mead","BFGS"), itnmax=NULL, hessian=FALSE,
               control=list(maximize =TRUE))
u2maxW ;   u2minW  # and compare to previous unrestricted:
uAllmaxW;    uAllminW  

#   ..  .  . .  .  ..   ..  . .  . .. . . . .  . . . . . . . . . . 
# EAST: 

uAllminE<-optimx(c(rep(0,4)), function(x,k=koefs[c(1,2,4,6,8)]) k[1]+sum(x*k[-1]), gr=NULL, hess=NULL, lower=c(rep(-1,4)), upper=c(rep(1,4)),
                 method=c("Nelder-Mead","BFGS"), itnmax=NULL, hessian=FALSE,
                 control=list()) ; uAllminE

uAllmaxE<-optimx(c(rep(0,4)), function(x,k=koefs[c(1,2,4,6,8)]) k[1]+sum(x*k[-1]), gr=NULL, hess=NULL, lower=c(rep(-1,4)), upper=c(rep(1,4)),
                 method=c("Nelder-Mead","BFGS"), itnmax=NULL, hessian=FALSE,
                 control=list(maximize =TRUE))
uAllminE;uAllmaxE

#   ..  .  . .  .  ..   ..  . .  . .. . . . .  . . . . . . . . . . 
# now max rain and sd restricted to 0

u2minE<-optimx(c(rep(0,2)), function(x,k=koefs[c(1,2,4)]) k[1]+sum(x*k[-1]), gr=NULL, hess=NULL, lower=c(rep(-1,2)), upper=c(rep(1,2)),
               method=c("Nelder-Mead","BFGS"), itnmax=NULL, hessian=FALSE,
               control=list()); u2minE

u2maxE<-optimx(c(rep(0,2)), function(x,k=koefs[c(1,2,4)]) k[1]+sum(x*k[-1]), gr=NULL, hess=NULL, lower=c(rep(-1,2)), upper=c(rep(1,2)),
               method=c("Nelder-Mead","BFGS"), itnmax=NULL, hessian=FALSE,
               control=list(maximize =TRUE))
u2minE; u2maxE

#   ..  .  . .  .  ..   ..  . .  . .. . . . .  . . . . . . . . . ...  .  . .  .  ..   ..  . .  . .. . . . .  . . . . . . . . . ...  .  . .  .  ..   ..  . .  . .. . . . .  . . . . . . . . . ...  .  . .  .  ..   ..  . .  . .. . . . .  . . . . . . . . . .
# compare the models above again:

uAllminW; uAllmaxW   # unrestricted           # West
u2minW; u2maxW       # restricted

uAllminE; uAllmaxE    # unrestricted           #East
u2minE; u2maxE        # restricted
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo



