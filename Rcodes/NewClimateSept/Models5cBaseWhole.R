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


#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# unfortunatelly, I have to write the models as follows (foob is the same as foobar) as there is a bug in the Step function...:

baseSc<-MaizeClimateScTS[c( "ID1","ADM2_NAME","Year","ASAL","MaxTemp_OctMar","AvgTemp_OctMar","CumDD_OctMar","HeatWDays_OctMar",
                           "SDtemp_OctMar","MaxTemp_MarSep","AvgTemp_MarSep" ,"CumDD_MarSep","HeatWDays_MarSep","SDtemp_MarSep" ,"PrecCoefVar_OND",
                           "DrSpell10_OND","DrSpell20_OND", "DrySpell_OND", "MaxRain_OND","SeasRain_OND", "Prec2Months_OND","PrecCoefVar_MAM" ,
                           "DrSpell10_MAM","DrSpell20_MAM", "DrySpell_MAM" , "MaxRain_MAM" , "SeasRain_MAM",  "Prec2Months_MAM", "Area" , "Yield" ,"MT" , "west1")]
baseSc<-baseSc[complete.cases(baseSc),]
 
n<-names(baseSc)

xxx<-paste(n[!n %in% c("Yield","ID1","ADM2_NAME","Year","ASAL","Area","MT","west1") ], collapse = " + ")


f <- as.formula(   paste("Yield ~", xxx ,"+(1+",  xxx,"|ID1)"   )      )   
barSc<-lmer(f,data=baseSc)
summary(barSc)  # ok. sp I cannt estimate this....

barrStep<-step(barSc)
summary(get_model(barrStep))

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

