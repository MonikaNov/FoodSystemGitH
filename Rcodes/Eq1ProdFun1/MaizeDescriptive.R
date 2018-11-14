rm(list=ls())

WDuni<-c("/home/m/mn/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

load("~/foodSystems/dataFS/Main/CrMaize5.RData")

boxplot(CrMaize5$Yield)
summary(CrMaize5$Yield)
boxplot(CrMaize5$Yield/CrMaize5$Area)

boxplot(CrMaize5$Yield/CrMaize5$Area~CrMaize5$ID)
hist(CrMaize5$Yield/CrMaize5$Area)
hist(CrMaize5$Yield/CrMaize5$Area,100)


hist(CrMaize5$Yield/CrMaize5$Area,100)

CrMaize5[which(CrMaize5$Yield/CrMaize5$Area>0.005),]


hist(CrMaize5$Yield)

hist(CrMaize5$Yield,100)
boxplot(CrMaize5$Yield~CrMaize5$ID)