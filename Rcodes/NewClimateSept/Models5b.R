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

load("~/foodSystems/Rcodes/NewClimateSept/Models5b.RData")
load("\\\\its-home.uscs.susx.ac.uk/home/mn301/foodSystems/Rcodes/NewClimateSept/Models5b.RData")
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo


# unfortunatelly, I have to write the models as follows (foob is the same as foobar) as there is a bug in the Step function...:
n<-names(baseP)
xx<-paste(n[!n %in% c("Yield","ID1","ADM2_NAME","Year","ASAL","Area","MT","west1") ], collapse = " + ")

f <- as.formula(   paste("Yield ~", xx ,"+(1+",  xx,"|ID1)"   )      )   
baz<-lmer(f,data=baseP )
summary(baz)
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#now scaled:

bazSc<-lmer(f,data=basePsc )
summary(bazSc)

foobarStep<-step(bazSc)
summary(get_model(foobarStep))  # very good !!!
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# now try to run the step, trying to keep the sums of precipitation

foobarStep2<-step(bazSc,keep=c("SeasRain_OND","SeasRain_MAM") )
summary(get_model(foobarStep2))

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#now log:
f <- as.formula(   paste("log(Yield) ~", xx ,"+(1+",  xx,"|ID1)"   )      )   
bazLn<-lmer(f,data=baseP)
summary(bazLn)
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

save.image("~/foodSystems/Rcodes/NewClimateSept/Models5b.RData")
save.image("\\\\its-home.uscs.susx.ac.uk/home/mn301/foodSystems/Rcodes/NewClimateSept/Models5.RData")