rm(list=ls())

library('dplyr')
library('purrr')
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