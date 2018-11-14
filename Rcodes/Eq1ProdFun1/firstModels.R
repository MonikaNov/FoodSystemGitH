rm(list=ls())

WDuni<-c("/home/m/mn/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

load("~/foodSystems/dataFS/Main/CrMaize5.RData")
load("~/foodSystems/dataFS/Main/climate4.RData")

summary(lm(CrMaize5$Yield~I(CrMaize5$i1)))
summary(lm(CrMaize5$Yield~I(CrMaize5$i1lag)))
summary(lm(CrMaize5$Yield~as.factor(CrMaize5$i1_sum)))

summary(lm(CrMaize5$Yield~I(CrMaize5$i1*CrMaize5$i1lag)))


summary(lm(Yield~(i1*i1lag +icum+iTcum+iTzcum),data=CrMaize5 ))


summary(lm(Yield~(i1*i1lag +icum+iTcum+iTzcum+icum_lag+iTcum_lag+iTzcum_lag),data=CrMaize5 ))
summary(lm(Yield~i1lag +icum+iTcum+iTzcum+icum_lag+iTcum_lag+iTzcum_lag,data=CrMaize5 ))
#nice
summary(lm(Yield~i1lag +icum+iTcum+iTzcum+iTzcum_lag,data=CrMaize5 ))
