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

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
basicME<-lmer(Yield ~ SeasRain_MAM +SeasRain_OND+ MeanTemp+ 
                (SeasRain_MAM +SeasRain_OND+ MeanTemp| ID1),data=MaizeClimateScTS)

summary(basicME) 
nobs(basicME) 


basicME<-lmer(Yield ~ SeasRain_MAM +SeasRain_OND+ MeanTemp+ 
                (MeanTemp+1| ID1),data=MaizeClimateScTS)

summary(basicME) 
nobs(basicME) 


bazSc20<-lmer(Yield ~ MaxRain_OND + Prec2Months_OND + DrSpell10_MAM + DrSpell20_MAM + 
                MaxRain_MAM + SeasRain_MAM + Prec2Months_MAM +SeasRain_OND+ (DrSpell10_OND + DrySpell_OND + DrySpell_MAM +SeasRain_OND| ID1),data=MaizeClimateScTS)
summary(bazSc20)  # didnt fail to converge!!!

bazSc21<-lmer(Yield ~ MaxRain_OND + Prec2Months_OND  + DrSpell20_MAM + 
                MaxRain_MAM + SeasRain_MAM + Prec2Months_MAM +SeasRain_OND+ (DrSpell10_OND +SeasRain_OND| ID1),data=MaizeClimateScTS)
summary(bazSc21)  # didnt fail to converge!!!

bazSc21<-lmer(Yield ~ MaxRain_OND + Prec2Months_OND  + DrSpell20_MAM + AvgTemp_MarSep
              +  MaxRain_MAM + SeasRain_MAM + Prec2Months_MAM +SeasRain_OND+ (AvgTemp_MarSep+DrSpell10_OND +SeasRain_OND| ID1),data=MaizeClimateScTS)
summary(bazSc21)  


bazSc21<-lmer(Yield ~ MaxRain_OND + Prec2Months_OND  + DrSpell20_MAM + AvgTemp_MarSep+ AvgTemp_OctMar+
              +  MaxRain_MAM + SeasRain_MAM + Prec2Months_MAM +SeasRain_OND+ (AvgTemp_MarSep+ AvgTemp_OctMar+DrSpell10_OND +SeasRain_OND| ID1),data=MaizeClimateScTS)
summary(bazSc21)  
nobs(bazSc21)

basicME<-lmer(Yield ~ SeasRain_MAM +SeasRain_OND+ AvgTemp_MarSep+ 
                (SeasRain_MAM +SeasRain_OND+ AvgTemp_MarSep| ID1),data=MaizeClimateScTS)

summary(basicME) 
nobs(basicME) 