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



load("Main/CrMaize16.RData")
CrMaize16ts<-pdata.frame(CrMaize16,index=c("ID","Year"))


#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

Keith7s<-lmer(Yield~scale(PreMed) + I(scale(PreMed)^2)+ scale(TemMed) +I(scale(TemMed)^2) + scale(PreMedCV)+I(scale(PreMedCV)^2) +
                scale(TemMedCV) +I(scale(TemMedCV)^2) 
              +(scale(PreMed)+I(scale(PreMed)^2)  + scale(TemMed) +I(scale(TemMed)^2) |ID),data=CrMaize16ts)
summary(Keith7s) 

# omg. realized that this is nonsense as scaled precipitation can be negative>>adding squares is a bit nonsense..
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Astrid1<-lmer(log(Yield)~scale(PreMed) + I(scale(PreMed,center=FALSE)^2)+ scale(TemMed) 
              +I(scale(TemMed,center=FALSE)^2) 
              + scale(PreMedCV)+
                scale(TemMedCV)  
              +(scale(PreMed)+I(scale(PreMed,center=FALSE)^2)
                + scale(TemMed) +I(scale(TemMed)^2) |ID),data=CrMaize16ts)
summary(Astrid1) 

#maybe I should actually remove the insignificant...
# prettyy good

Kei7step<-step(Keith7s)
Kei7step1<-get_model(Kei7step) 
summary(Kei7step1)





hist(CrMaize16ts$PreMed)
hist(scale(CrMaize16ts$PreMed))
summary(CrMaize16$PreMed)

hist(CrMaize16ts$PreMedZ)

CrMaize16ts[CrMaize16ts$PreMedZ<(-1),]

CrMaize16ts[CrMaize16ts$PreMedZ<(-1),]