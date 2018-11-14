# rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma
library('dplyr')
library('purrr')
library("reshape")
setwd(WDuni)
setwd(WDhome)

load("Main/dataOctober.RData")

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

sum(is.na(dataAllScTS$Yield))
sum(is.na(dataAllScTS$Year))
table(dataAllScTS$Year[is.na(dataAllScTS$Yield)])

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

sum(is.na(dataAllSc$Yield))
sum(is.na(dataAllSc$Year))
table(dataAllSc$Year[is.na(dataAllSc$Yield)])