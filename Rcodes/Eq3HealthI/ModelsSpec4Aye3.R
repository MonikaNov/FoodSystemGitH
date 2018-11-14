
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
load("Main/Phase24.RData")

Phase24ts<-pdata.frame(Phase24,index=c("CountyID","T"))

##ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# The best for now is Aye3 
##ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Aye3<-lmer(MUACn~PreMedZl1+PreMedZl2+PreMedZl3+PreMedZl4+PreMedZl5
           +PreMedZl6+TemMedZl6+(PreMedZl4+PreMedZl5+PreMedZl6|CountyID),data=Phase24ts)
summary(Aye3) 
plot(Aye3)
dotplot(ranef(Aye3))