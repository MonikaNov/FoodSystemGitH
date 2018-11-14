rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo


load("Main/climate13.RData")
load("Main/CrMaize16.RData")

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# 1. see why is there much more counties in the climate dataset than in the maize dataset

names(climate13[[1]])
countiesClim<-unique(cbind(climate13[[1]]$ID1, climate13[[1]]$ADM2_NAME))

lapply(climate13[2:8], function(x) all.equal(     unique(    climate13[[1]][c("ADM2_NAME","ID1")]) , unique(x[c("ADM2_NAME","ID1")]) ))

head(CrMaize16)
countiesMaize<-unique(cbind(CrMaize16$ID,CrMaize16$county))

setdiff(countiesClim,countiesMaize)

setdiff(countiesClim[1],countiesMaize[1])