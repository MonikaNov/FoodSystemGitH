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


Astrid2<-lmer(Yield~scale(PreMed) +  scale(TemMed) 
              +I(scale(TemMed,center=FALSE)^2) 
              + scale(PreMedCV)+
              +(scale(PreMed)
                + scale(TemMed) |ID),data=CrMaize16ts)
summary(Astrid2) 

A2step<-step(Astrid2)
A21<-get_model(A2step) 
summary(A21)


A22<-lmer(Yield~scale(PreMed) +  scale(TemMed) 
              +I(scale(TemMed,center=FALSE)^2) 
              + (scale(TemMed) |ID),data=CrMaize16ts)
summary(A22) 
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo


hist(CrMaize16ts$PreMed)
hist(scale(CrMaize16ts$PreMed))
hist(scale(CrMaize16ts$PreMed,center=FALSE))
summary(CrMaize16$PreMed)

hist(CrMaize16ts$PreMedZ)

CrMaize16ts[CrMaize16ts$PreMedZ<(-1),]

CrMaize16ts[CrMaize16ts$PreMedZ<(-1),]

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
save.image("~/foodSystems/Rcodes/Eq1ProdFun4/forPresentation6.RData")