rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)

library('dplyr')
library('tseries')
library(plm)
library(lme4)
library(lattice)
library(car)
library(lmerTest)
library(optimx)
library(lmmlasso)
library(lmmen)
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# I may have to scale evrything, otherwise it will never converge..

load("Main/MaizeClimate.RData")

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
basePnum<-MaizeClimate[c( "ID1","HeatWDays_OctMar"
                         ,"HeatWDays_MarSep","PrecCoefVar_OND",
                         "DrSpell10_OND","DrSpell20_OND", "DrySpell_OND", 
                         "MaxRain_OND","SeasRain_OND", "Prec2Months_OND","PrecCoefVar_MAM" ,
                         "DrSpell10_MAM","DrSpell20_MAM", "DrySpell_MAM" , "MaxRain_MAM" , 
                         "SeasRain_MAM",  "Prec2Months_MAM", "Area" , "Yield" ,"MT" )]

basePnum<-basePnum[complete.cases(basePnum),]

basePnum2<-basePnum
basePnum2[,-c(1:4, 24)]<-scale(basePnum[,-c(1:4, 24)])
xyz<-cbind(seq(1),basePnum2)
xyz<-as.matrix(xyz)
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
l1=lmmlasso(x=xyz[,c(1,3:18)],  y=xyz[,20], grp=xyz[,2],lambda=10)
summary(l1)

