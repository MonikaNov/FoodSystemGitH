rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDuni2<-"\\\\its-home.uscs.susx.ac.uk/home/mn301/foodSystems/dataFS"
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma


setwd(WDuni)
setwd(WDuni2)
setwd(WDhome)

library('dplyr')
library('tseries')
library(plm)
library(lme4)
library(lattice)
library(car)
library(lmerTest)
library(optimx)

source("/its/home/mn301/foodSystems/Rcodes/NewClimateSept/Models3GettingFrames.R")
source("\\\\its-home.uscs.susx.ac.uk/home/mn301/foodSystems/Rcodes/NewClimateSept/Models3GettingFrames.R")

load("~/foodSystems/Rcodes/NewClimateSept/Models6.RData")
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
##   foobarStep<-step(bazSc) just to see how I got this
summary(get_model(foobarStep))  # 

#  so the following is the same as foobarStep, the chosen one by the step procedure. Now, I just rewrite it as lmer..:

bazSc0<-lmer(Yield ~ MaxRain_OND + Prec2Months_OND + DrSpell10_MAM + DrSpell20_MAM + 
               MaxRain_MAM + SeasRain_MAM + Prec2Months_MAM + (DrSpell10_OND + DrySpell_OND + DrySpell_MAM | ID1),data=basePsc )
summary(bazSc0)
nobs(bazSc0)

# AND I WILL TRY TO ADD ALL THE MONTHS TO SEE IF I CAN GET LESS HILARIOUS RESULTS..
n<-names(baseMonthsSc)[25:48]
xx<-paste(n, collapse = " + ")
f <- as.formula(   paste("Yield ~ MaxRain_OND + Prec2Months_OND + DrSpell10_MAM + DrSpell20_MAM + 
               MaxRain_MAM + SeasRain_MAM + Prec2Months_MAM +", xx ,"+(1+DrSpell10_OND + DrySpell_OND + DrySpell_MAM+ ",  xx,"|ID1)"   )      )

snor<-lmer(f,data=baseMonthsSc )
summary(snor)

#--------------------------------------------------------------------------------------------------------------------------------------------------
# following relative all right I guess:


f <- as.formula(   paste("Yield ~ MaxRain_OND + Prec2Months_OND + DrSpell10_MAM + DrSpell20_MAM + 
               MaxRain_MAM + SeasRain_MAM + Prec2Months_MAM +", xx ,"+(1+DrSpell10_OND + DrySpell_OND + DrySpell_MAM|ID1)"   )      )

snor<-lmer(f,data=baseMonthsSc )
summary(snor)
