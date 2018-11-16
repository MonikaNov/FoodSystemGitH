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


Rachel3<-lmer(log(Yield)~PreMedZ + I(PreMedZ^2)+ TemMedZ +I(TemMedZ^2) + PreMedCVz+I(PreMedCVz^2) +
                TemMedCVz +I(TemMedCVz^2)  +(PreMedZ+I(PreMedZ^2)  + TemMedZ +I(TemMedZ^2) |ID),data=CrMaize16ts)
summary(Rachel3)

R3step<-step(Rachel3)
R3step1<-get_model(R3step) 
summary(R3step1)
ranef(R3step1)


#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo





hist(CrMaize16ts$PreMedz)

summary(CrMaize16$PreMedz)


CrMaize16ts[CrMaize16ts$PreMedZ<(-1),]

CrMaize16ts[CrMaize16ts$PreMedZ<(-1),]