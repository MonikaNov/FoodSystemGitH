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
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# I may have to scale evrything, otherwise it will never converge..


load("Main/MaizeClimate.RData")

MaizeClimateTS<-pdata.frame(MaizeClimate,index=c("ID1","Year"))
MaizeClimateScTS<-MaizeClimateTS

MaizeClimateScTS[,-c(1:5,32:34,38)]<-scale(MaizeClimateTS[,-c(1:5,32:34,38)])

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# now frame for the months only

FrameMonths<-MaizeClimateTS[c( names(MaizeClimateTS)[c(1,2,36,39:62)])]
FrameMonths<-FrameMonths[complete.cases(FrameMonths)==TRUE,]
FrameMonthsSc<-FrameMonths
FrameMonthsSc[,-c(1:3)]<-scale(FrameMonths[,-c(1:3)])

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# now months and all other vars together

FrameAll<-MaizeClimateTS[c( names(MaizeClimateTS)[c(1,2,4:21,23:29,31,35:62)])]
FrameAll<-FrameAll[complete.cases(FrameAll)==TRUE,]
FrameAllSc<-FrameAll
FrameAllSc[,-c(1:4, 32)]<-scale(FrameAll[,-c(1:4, 32)])

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#the following subset has relatively many observations and relatively enough variables:

baseP<-MaizeClimateTS[c( "ID1","ADM2_NAME","Year","ASAL","HeatWDays_OctMar"
                           ,"HeatWDays_MarSep","PrecCoefVar_OND",
                           "DrSpell10_OND","DrSpell20_OND", "DrySpell_OND", 
                           "MaxRain_OND","SeasRain_OND", "Prec2Months_OND","PrecCoefVar_MAM" ,
                           "DrSpell10_MAM","DrSpell20_MAM", "DrySpell_MAM" , "MaxRain_MAM" , 
                           "SeasRain_MAM",  "Prec2Months_MAM", "Area" , "Yield" ,"MT" , "west1")]

baseP<-baseP[complete.cases(baseP),]

basePsc<-baseP
basePsc[,-c(1:4, 24)]<-scale(baseP[,-c(1:4, 24)])

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# so I might as well combine it with months
baseMonths<-MaizeClimateTS[c( "ID1","ADM2_NAME","Year","ASAL","HeatWDays_OctMar"
                         ,"HeatWDays_MarSep","PrecCoefVar_OND",
                         "DrSpell10_OND","DrSpell20_OND", "DrySpell_OND", 
                         "MaxRain_OND","SeasRain_OND", "Prec2Months_OND","PrecCoefVar_MAM" ,
                         "DrSpell10_MAM","DrSpell20_MAM", "DrySpell_MAM" , "MaxRain_MAM" , 
                         "SeasRain_MAM",  "Prec2Months_MAM", "Area" , "Yield" ,"MT" , "west1",names(MaizeClimateTS)[39:62] ) ]

baseMonths<-baseMonths[complete.cases(baseMonths),]  # number of rows relatively ok

baseMonthsSc<-baseMonths
baseMonthsSc[,-c(1:4, 24)]<-scale(baseMonths[,-c(1:4, 24)])
 
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo



#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
