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
n<-names(baseP)
xx<-paste(n[!n %in% c("Yield","ID1","ADM2_NAME","Year","ASAL","Area","MT","west1") ], collapse = " + ")

f <- as.formula(   paste("Yield ~", xx ,"+(1+",  xx,"|ID1)"   )      )   
baz<-lmer(f,data=baseP )
summary(baz)
#-------------
#now scaled:

bazSc<-lmer(f,data=basePsc )
summary(bazSc)


foobarStep<-step(foob)
summary(get_model(step(foobar)))

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

save.image("~/foodSystems/Rcodes/NewClimateSept/Models5.RData")
save.image("\\\\its-home.uscs.susx.ac.uk/home/mn301/foodSystems/Rcodes/NewClimateSept/Models5.RData")