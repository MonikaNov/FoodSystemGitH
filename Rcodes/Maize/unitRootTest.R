rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)
library('plm')
library('dplyr')
library('tseries')
library('urca')
options(max.print=10000)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

load("Main/CrMaize13.RData")
load("Main/CrMaize11.RData")

rm(CrMaize13ts)
CrMaize13ts<-pdata.frame(CrMaize13,index=c("ID","Year"))
CrMaize8<-pdata.frame(CrMaize8,index=c("ID","Year"))
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

boxplot(Yield~Year,data=CrMaize13ts)
boxplot(lag(Yield)~Year,data=CrMaize13ts)


plot(aggregate(Yield ~ Year, CrMaize13ts, sd))
plot(diff(CrMaize13ts$Yield)~CrMaize13ts$Year)

adf.test(seq(1:1000), k=2) 
adf.test(rnorm(1000), k=2) 

library('tseries')
adf.test(CrMaize13$Yield) #not sure if  this work for panels
adf.test(aggregate((CrMaize13ts$Yield) ~ Year, CrMaize13ts, mean)[[2]])

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!           do pwdtest
adf.test(CrMaize13ts$Yield[CrMaize13ts$ID==1])

adftest<-sapply(unique(CrMaize13ts$ID), function(x) adf.test(CrMaize13ts$Yield[CrMaize13ts$ID==x])  )

purtest(CrMaize13ts$Yield,test = c( "madwu"),data=CrMaize13, index=c("ID","Year"))


CrMaize13tsModern<-subset(CrMaize13ts,Year %in% 1985:2000 )
adftestMod<-sapply(unique(CrMaize13tsModern$ID), function(x) adf.test(CrMaize13tsModern$Yield[CrMaize13tsModern$ID==x])  )
adftestMod

adf.test(aggregate((CrMaize13tsModern$Yield) ~ Year, CrMaize13tsModern, mean)[[2]])


saga<-plm(CrMaize13ts$Yield~1+Year,data=CrMaize13ts)
summary(saga)
purtest(saga,test = c( "ips"))

x <- rnorm(1000)  # no unit-root
adf.test(x)

sum(is.na(CrMaize13$Yield))





purtest(Yield,data=CrMaize13,index=c("ID","Year"))

purtest(object

ur.df(CrMaize13$Yield)  
rowSums(table(CrMaize13ts$ID,CrMaize13ts$Year))


purtest(CrMaize130ts$Yield)  
# I probably need to get it balanced
rowSums(table(CrMaize13ts$ID,CrMaize13ts$Year))
colSums(table(CrMaize13ts$ID,CrMaize13ts$Year))

