rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)

library('plm')
library('dplyr')
library('tseries')
library('nlme')

load("Main/CrMaize16.RData")
CrMaize16ts<-pdata.frame(CrMaize16,index=c("ID","Year"))

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

# Plots of maize agains climate/weather


plot(Yield~PreMean, data = CrMaize16ts, main="Maize, county level, pooled", xlab="Precipitation - mean",ylab="Yield MT/hectar")
plot(Yield~PreMed, data = CrMaize16ts, main="Maize, county level, pooled", xlab="Precipitation - median",ylab="Yield MT/hectar")
plot(Yield~PreMeanZ, data = CrMaize16ts) 
plot(Yield~PreMedZ, data = CrMaize16ts, main="Maize, county level, pooled", xlab="Precipitation Z-score, median",ylab="Yield MT/hectar")

# so the correlation seems to be bigger with the raw precipitation. But ML algorithm may have problems converging here..
# maybe the difference in correlation is just scaling??

plot(Yield~TemMean, data = CrMaize16ts)   # VERY INTERESTING. WHY IS IT U-SHAPED??
plot(Yield~TemMed, data = CrMaize16ts, main="Maize, county level, pooled", xlab="Temperature - median",ylab="Yield MT/hectar") 
plot(Yield~TemMeanZ, data = CrMaize16ts) 
plot(Yield~TemMedZ, data = CrMaize16ts, main="Maize, county level, pooled", xlab="Temperature Z-score, median",ylab="Yield MT/hectar")

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  now the coefficients of variation...

plot(Yield~PreMeanCV, data = CrMaize16ts)   # Also VERY INTERESTING, seems to be NEGATIVELY correlated with CV of precipitation
plot(Yield~PreMedCV, data = CrMaize16ts, main="Maize, county level, pooled", xlab="Precipitation - Coefficient of variation, median",ylab="Yield MT/hectar")
plot(Yield~PreMeanCVz, data = CrMaize16ts)
plot(Yield~PreMedCVz, data = CrMaize16ts)


plot(Yield~TemMeanCV, data = CrMaize16ts)   # also here somehow negatively correlated
plot(Yield~TemMedCV, data = CrMaize16ts, main="Maize, county level, pooled", xlab="Temperature - Coefficient of variation,  median",ylab="Yield MT/hectar")
plot(Yield~TemMeanCVz, data = CrMaize16ts, main="Maize, county level, pooled", xlab="Temperature - Coefficient of variation, Z-score, mean",ylab="Yield MT/hectar")
plot(Yield~TemMedCVz, data = CrMaize16ts)