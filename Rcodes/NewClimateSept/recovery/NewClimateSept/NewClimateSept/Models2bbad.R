rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)

library('dplyr')
library('tseries')
library(plm)
library(lme4)
library(lattice)
library(car)
library(lmerTest)
library(optimx)
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# I may have to scale evrything, otherwise it will never converge..


load("Main/MaizeClimate.RData")
MaizeClimateTS<-pdata.frame(MaizeClimate,index=c("ID1","Year"))
MaizeClimateScTS<-MaizeClimateTS
MaizeClimateScTS[,-c(1:5,32:34,38)]<-scale(MaizeClimateTS[,-c(1:5,32:34,38)])

# base<-MaizeClimateScTS[c( "ID1","Year","code","ADM2_NAME","ASAL","MaxTemp_OctMar","AvgTemp_OctMar","CumDD_OctMar","HeatWDays_OctMar","SDtemp_OctMar","MaxTemp_MarSep","AvgTemp_MarSep" ,"CumDD_MarSep","HeatWDays_MarSep","SDtemp_MarSep" ,"PrecCoefVar_OND","DrSpell10_OND","DrSpell20_OND", "DrySpell_OND", "MaxRain_OND","SeasRain_OND", "PrecStDev_OND","Prec2Months_OND","PrecCoefVar_MAM" ,"DrSpell10_MAM","DrSpell20_MAM", "DrySpell_MAM" , "MaxRain_MAM" , "SeasRain_MAM", "PrecStDev_MAM", "Prec2Months_MAM", "Admin1" , "county" ,"Admin2", "Area" , "Yield" ,"MT" , "west1"),]

 base<-MaizeClimateScTS[c( "ID1","Year","ASAL","MaxTemp_OctMar","AvgTemp_OctMar","CumDD_OctMar","HeatWDays_OctMar",
                           "SDtemp_OctMar","MaxTemp_MarSep","AvgTemp_MarSep" ,"CumDD_MarSep","HeatWDays_MarSep","SDtemp_MarSep" ,"PrecCoefVar_OND",
                           "DrSpell10_OND","DrSpell20_OND", "DrySpell_OND", "MaxRain_OND","SeasRain_OND", "Prec2Months_OND","PrecCoefVar_MAM" ,
                           "DrSpell10_MAM","DrSpell20_MAM", "DrySpell_MAM" , "MaxRain_MAM" , "SeasRain_MAM",  "Prec2Months_MAM", "Area" , "Yield" ,"MT" , "west1")]

 base<-base[complete.cases(base),]
 
 
 baseP<-MaizeClimateScTS[c( "ID1","Year","ASAL","HeatWDays_OctMar"
                         ,"HeatWDays_MarSep","PrecCoefVar_OND",
                           "DrSpell10_OND","DrSpell20_OND", "DrySpell_OND", "MaxRain_OND","SeasRain_OND", "Prec2Months_OND","PrecCoefVar_MAM" ,
                           "DrSpell10_MAM","DrSpell20_MAM", "DrySpell_MAM" , "MaxRain_MAM" , "SeasRain_MAM",  "Prec2Months_MAM", "Area" , "Yield" ,"MT" , "west1")]
 
 
 baseP<-base[complete.cases(baseP),]
 apply(as.matrix(baseP),2,function(x) sum(is.na(x)))
 
 baseT<-MaizeClimateScTS[c( "ID1","Year","ASAL","MaxTemp_OctMar","AvgTemp_OctMar","CumDD_OctMar","HeatWDays_OctMar",
                           "SDtemp_OctMar","MaxTemp_MarSep","AvgTemp_MarSep" ,"CumDD_MarSep","HeatWDays_MarSep","SDtemp_MarSep" ,"PrecCoefVar_OND",
                           "DrSpell10_OND","DrSpell20_OND", "DrySpell_OND", "MaxRain_OND","SeasRain_OND", "Prec2Months_OND","PrecCoefVar_MAM" ,
                           "DrSpell10_MAM","DrSpell20_MAM", "DrySpell_MAM" , "MaxRain_MAM" , "SeasRain_MAM",  "Prec2Months_MAM", "Area" , "Yield" ,"MT" , "west1")]
 
 
 
 baseT<-base[complete.cases(baseT),]

 apply(as.matrix(baseT),2,function(x) sum(is.na(x)))
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo



Cassey<-lmer(Yield~HeatWDays_OctMar+HeatWDays_MarSep +PrecCoefVar_OND+DrSpell10_OND +DrSpell20_OND 
             +DrySpell_OND +MaxRain_OND +SeasRain_OND+Prec2Months_OND +PrecCoefVar_MAM+
             DrSpell10_MAM +DrSpell20_MAM+DrySpell_MAM+MaxRain_MAM+SeasRain_MAM+Prec2Months_MAM
             +(HeatWDays_OctMar+HeatWDays_MarSep +PrecCoefVar_OND+DrSpell10_OND +DrSpell20_OND 
               +DrySpell_OND +MaxRain_OND +SeasRain_OND+Prec2Months_OND +PrecCoefVar_MAM+
                 DrSpell10_MAM +DrSpell20_MAM+DrySpell_MAM+MaxRain_MAM+SeasRain_MAM+Prec2Months_MAM|ID1),  data=baseP)
summary(Cassey) 


Setppie<-step(Cassey)
summary(Setppie) 
summary(get_model(Setppie))

save.image("~/foodSystems/Rcodes/NewClimateSept/Models2.RData")
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# first models (a suggestion from email 20.5.2018): lmer(yield~rain+temp+cv_rain+cv_temp+(1+rain+temp|counties))
