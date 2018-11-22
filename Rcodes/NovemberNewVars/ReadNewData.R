rm(list=ls())

library('dplyr')
library('purrr')
load("dataFS/Main/DaTS.RData");testSc<-ScaledTS; testD<-DaTS
library(dplyr); library(tseries); library(plm); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

floods<-read.csv("../FoodSystemGitH/dataFS/climNov2/Flood.csv",na.strings="-999",dec=".")
summary(floods);head(floods)

summary(lm(cum_90_MarMay~cum_95_OctDec,data=floods)) # test regression: are there some missings or not?
nobs(lm(cum_90_MarMay~cum_95_OctDec,data=floods)) 

plot(days_90_MarMay~days_90_MarAug,data=floods)
# seems good

monthlyRa<-read.csv("../FoodSystemGitH/dataFS/climNov2/Monthly_cumul.csv",na.strings="-999",dec=".")
summary(monthlyRa); head(monthlyRa)
nobs(lm(cum_Oct~cum_Jun,data=monthlyRa)) ; summary(lm(cum_Oct~cum_Jun,data=monthlyRa)) #testing regression. Test sems to be sucsessful

names(monthlyRa)
#more checking:
summary(DaTS["SeasRain_MarMay"]); summary(monthlyRa["cum_Mar"])
summary(DaTS["SeasRain_OctDec"]); summary(monthlyRa["cum_Oct"])

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#  now I have to match it with the current main dataset 

names(floods)[1]<-"Year"
names(monthlyRa)[1]<-"Year"

floods2<-floods; floods<-floods2[-c(3,4)]; floods$Year<-as.factor(floods$Year)
monthlyRa2<-monthlyRa; monthlyRa<-monthlyRa2[-c(3,4)]; monthlyRa$Year<-as.factor(monthlyRa$Year)

test1<-DaTS

DaTS<-merge(DaTS,floods, all.x=TRUE)
DaTS<-merge(DaTS,monthlyRa, all.x=TRUE)

testRe<-lm(Yield~.,data=Da[c(1:43,49,79,82)])
summary(testRe)
nobs(testRe)
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# checking: 
sum(DaTS$cum_95_MarMay<DaTS$cum_99_MarMay)
sum(DaTS$cum_95_MarMay>=DaTS$cum_99_MarMay)
sum(DaTS$cum_90_MarMay>=DaTS$cum_95_MarMay)
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#------------      test in a model...............

testRe<-lm(Yield~.,data=DaTS[c(1:43,76,79,82,85:145)])
summary(testRe)
nobs(testRe)
DaTS<-pdata.frame(DaTS,index=c("ID1","Year"))

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#        ok, I still need to get lags of OND seasons...
