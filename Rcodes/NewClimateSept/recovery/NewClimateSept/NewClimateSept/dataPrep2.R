rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
# setwd(WDhome)
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
source("/its/home/mn301/foodSystems/Rcodes/NewClimateSept/DataPrep1.R")
#or just load the processed data..:    load("Main/MaizeClimate.RData")


source("/its/home/mn301/foodSystems/Rcodes/NewClimateSept/ReadAverages.R")
#or just load the processed data..:    load("Main/climateAverages.RData")

names(Prec4)[5:16]<-paste0(names(Prec4)[5:16],"P")
names(Temp4)[5:16]<-paste0(names(Temp4)[5:16],"T")

MaizeClimate2<-merge(MaizeClimate,merge(Prec4,Temp4) ,all.x=TRUE )
MaizeClimate2<-MaizeClimate2[order(MaizeClimate2$ID1,MaizeClimate2$Year) ,]
#checking testing

all.equal(MaizeClimate,MaizeClimate2[1:38])
nrow(unique(MaizeClimate2[c("west1","Year")]))

MaizeClimate2[MaizeClimate2$code==51385,"DecemberT"]

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#goood all checked and tested 20.9.2018
MaizeClimate<-MaizeClimate2
MaizeClimate$MeanPrec<-rowMeans(MaizeClimate[39:50])
MaizeClimate$MeanTemp<-rowMeans(MaizeClimate[51:62])
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
rm(list=setdiff(ls(), "MaizeClimate"))
save.image("~/foodSystems/dataFS/Main/MaizeClimate.RData")