rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS/Main") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS/Main") # doma

setwd(WDuni)
setwd(WDhome)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
load("VCI.RData")
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
summary(AllZonesDF)
summary(Phase06)

hist(AllZonesDF$p50)
hist(AllZonesDF$p50,100)
boxplot(AllZonesDF$p50)
boxplot(p50~County, data=AllZonesDF)
boxplot(p50~Month, data=AllZonesDF)
plot(AllZonesDF$p50,AllZonesDF$mean)

hist(AllZonesDF$mean)
hist(AllZonesDF$mean,100)
boxplot(AllZonesDF$mean)
boxplot(mean~County, data=AllZonesDF)
boxplot(mean~Month, data=AllZonesDF)

hist(AllZonesDF$p10,50)
hist(AllZonesDF$p10,100)
boxplot(AllZonesDF$p10)
boxplot(p10~County, data=AllZonesDF)
boxplot(p10~Month, data=AllZonesDF)

hist(AllZonesDF$p70,50)
hist(AllZonesDF$p70,100)
boxplot(AllZonesDF$p70)
boxplot(p70~County, data=AllZonesDF)
boxplot(p70~Month, data=AllZonesDF)


sum(AllZonesDF$p10<0)
sum(AllZonesDF$p10>0)
sum(AllZonesDF$p10==0)

sum(AllZonesDF$p30<0)
sum(AllZonesDF$p30>0)
sum(AllZonesDF$p30==0)




hist(AllZonesDF$p0)
hist(AllZonesDF$p10)
hist(AllZonesDF$p10)