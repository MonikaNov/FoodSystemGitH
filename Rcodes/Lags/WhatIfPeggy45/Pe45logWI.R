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
# Peggy45ln log yields with yield unscaled (needed for log) unscaled. (Peggy45 is the best at the moment.)
        #  Peggy459ln with all variables unscaled and yield in log could be an alternative, but this model failed to converge...

Peggy45ln<-lmer(log(dataUsTS$Yield) ~ I((SeasRain_MAM_L1+SeasRain_OND_L1)*(1-west1)) + I((SeasRain_MAM_L1)*west1)+ I(AvgTemp_MarSep_L1*(1-west1)) + I(AvgTemp_MarSep_L1*(west1))
                +I(SDtemp_OctMar_L1*(1-west1))+    +I(SDtemp_OctMar_L1*west1)
                
                +   I(MaxRain_OND*(1-west1)) +   I(MaxRain_OND*west1) 
                
                +(1+MaxRain_OND+I(SeasRain_MAM_L1+SeasRain_OND_L1) +I(AvgTemp_MarSep_L1)|ID1) ,data=dataScTS )
summary(Peggy45ln);nobs(Peggy45ln)  # GROOT, this looks very good.

koefs<-as.numeric(summary(Peggy45ln)$coeff[,1])  ; koefs
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# 1 WEST:
exp(koefs[1])

logAllminW<-optimx(c(rep(0,4)), function(x,k=koefs[c(1,3,5,7,9)]) exp(koefs[1]+sum(x*k[-1])), gr=NULL, hess=NULL, lower=c(rep(-1,4)), upper=c(rep(1,4)),
                 method=c("Nelder-Mead","BFGS"), itnmax=NULL, hessian=FALSE,
                 control=list()); logAllminW

logAllmaxW<-optimx(c(rep(0,4)), function(x,k=koefs[c(1,3,5,7,9)]) exp(koefs[1]+sum(x*k[-1])), gr=NULL, hess=NULL, lower=c(rep(-1,4)), upper=c(rep(1,4)),
                 method=c("Nelder-Mead","BFGS"), itnmax=NULL, hessian=FALSE,
                 control=list(maximize =TRUE))
logAllmaxW;    logAllminW  
exp(koefs[1]) # = the value of yields at means (=zeros of the scaled vars)
#   ..  .  . .  .  ..   ..  . .  . .. . . . .  . . . . . . . . . . 
# now max rain and sd restricted to 0

log2minW<-optimx(c(rep(0,2)), function(x,k=koefs[c(1,3,5)]) exp(k[1]+sum(x*k[-1])), gr=NULL, hess=NULL, lower=c(rep(-1,2)), upper=c(rep(1,2)),
               method=c("Nelder-Mead","BFGS"), itnmax=NULL, hessian=FALSE,
               control=list())  ; log2minW

log2maxW<-optimx(c(rep(0,2)), function(x,k=koefs[c(1,3,5)]) exp(k[1]+sum(x*k[-1])), gr=NULL, hess=NULL, lower=c(rep(-1,2)), upper=c(rep(1,2)),
               method=c("Nelder-Mead","BFGS"), itnmax=NULL, hessian=FALSE,
               control=list(maximize =TRUE))
log2maxW ;   log2minW  # and compare to previous unrestricted:
logAllmaxW;    logAllminW  

#   ..  .  . .  .  ..   ..  . .  . .. . . . .  . . . . . . . . . . 
# EAST: 

logAllminE<-optimx(c(rep(0,4)), function(x,k=koefs[c(1,2,4,6,8)]) exp(k[1]+sum(x*k[-1])), gr=NULL, hess=NULL, lower=c(rep(-1,4)), upper=c(rep(1,4)),
                 method=c("Nelder-Mead","BFGS"), itnmax=NULL, hessian=FALSE,
                 control=list()) ; logAllminE

logAllmaxE<-optimx(c(rep(0,4)), function(x,k=koefs[c(1,2,4,6,8)]) exp(k[1]+sum(x*k[-1])), gr=NULL, hess=NULL, lower=c(rep(-1,4)), upper=c(rep(1,4)),
                 method=c("Nelder-Mead","BFGS"), itnmax=NULL, hessian=FALSE,
                 control=list(maximize =TRUE))
logAllminE;logAllmaxE
exp(koefs[1])
#   ..  .  . .  .  ..   ..  . .  . .. . . . .  . . . . . . . . . . 
# now max rain and sd restricted to 0

log2minE<-optimx(c(rep(0,2)), function(x,k=koefs[c(1,2,4)]) exp(k[1]+sum(x*k[-1])), gr=NULL, hess=NULL, lower=c(rep(-1,2)), upper=c(rep(1,2)),
               method=c("Nelder-Mead","BFGS"), itnmax=NULL, hessian=FALSE,
               control=list()); log2minE

log2maxE<-optimx(c(rep(0,2)), function(x,k=koefs[c(1,2,4)]) exp(k[1]+sum(x*k[-1])), gr=NULL, hess=NULL, lower=c(rep(-1,2)), upper=c(rep(1,2)),
               method=c("Nelder-Mead","BFGS"), itnmax=NULL, hessian=FALSE,
               control=list(maximize =TRUE))
log2minE; log2maxE
exp(koefs[1])
#   ..  .  . .  .  ..   ..  . .  . .. . . . .  . . . . . . . . . ...  .  . .  .  ..   ..  . .  . .. . . . .  . . . . . . . . . ...  .  . .  .  ..   ..  . .  . .. . . . .  . . . . . . . . . ...  .  . .  .  ..   ..  . .  . .. . . . .  . . . . . . . . . .
# compare the models above again:

logAllminW; logAllmaxW   # unrestricted           # West
log2minW; log2maxW       # restricted

logAllminE; logAllmaxE    # unrestricted           #East
log2minE; log2maxE        # restricted
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo



