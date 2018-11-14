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

load("Main/MaizeClimate.RData")
MaizeClimateTS<-pdata.frame(MaizeClimate,index=c("ID1","Year"))

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

Cassey<-lmer(log(Yield)~SeasRain_MAM +SeasRain_OND  + DrSpell20_MAM+DrSpell20_OND +PrecStDev_MAM+PrecStDev_OND
             +(SeasRain_MAM ||ID1) +(SeasRain_OND ||ID1)+( DrSpell20_MAM||ID1)+(DrSpell20_OND||ID1)   ,data=MaizeClimateTS)
summary(Cassey) 

Cassey<-lmer(log(Yield)~SeasRain_MAM +SeasRain_OND  + DrSpell20_MAM+DrSpell20_OND +PrecStDev_MAM+PrecStDev_OND
             +(SeasRain_MAM +SeasRain_OND  + DrSpell20_MAM+DrSpell20_OND|ID1),data=MaizeClimateTS,
             control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-nlminb-B')))
summary(Cassey) 

Cassey<-lmer(log(Yield)~SeasRain_MAM +SeasRain_OND  + DrSpell20_MAM+DrSpell20_OND +PrecStDev_MAM+PrecStDev_OND
             +(SeasRain_MAM +SeasRain_OND  + DrSpell20_MAM+DrSpell20_OND|ID1),data=MaizeClimateTS)
summary(Cassey) 

Cassey<-lmer(Yield~SeasRain_MAM +SeasRain_OND
             +(SeasRain_MAM +SeasRain_OND|ID1),data=MaizeClimateTS)
summary(Cassey) 

Cassey<-lmer(Yield~scale(SeasRain_MAM) +scale(SeasRain_OND)  
             +(scale(SeasRain_MAM) +scale(SeasRain_OND)|ID1),data=MaizeClimateTS)
summary(Cassey) 

Cassey<-lmer(log(Yield)~SeasRain_MAM +SeasRain_OND 
             +(1|ID1),data=MaizeClimateTS)
summary(Cassey) 

Cassey2<-lmer(Yield~SeasRain_OND +SeasRain_MAM 
             +(1|ID1),data=MaizeClimateTS)
summary(Cassey2) 

Cassey3<-lmer(Yield~.-ID1+(1|ID1),data=MaizeClimateTS[c(1,21,29,36)])
summary(Cassey3) 


lm(y ~ ., data = MaizeClimateTS[c(1,21,29,36])

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# first models (a suggestion from email 20.5.2018): lmer(yield~rain+temp+cv_rain+cv_temp+(1+rain+temp|counties))
