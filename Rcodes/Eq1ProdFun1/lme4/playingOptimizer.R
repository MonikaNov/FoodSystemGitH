
rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

load("~/foodSystems/dataFS/Main/CrMaize8.RData")
library(plm)
library(lme4)

#ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
# my case

Dale401<-pvcm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"),weights=Area, model='random', data=CrMaize8)
summary(Dale401)

Dale401<-pvcm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"), model='random', data=CrMaize8)
summary(Dale401)
Dale01<-pvcm(Yield~PrecZones+TempZones,index=c("ID","Year"), model='random', data=CrMaize8)
summary(Dale01)


# !!!!!!!!!!!!!!!!!vahy - DOST DIVNY !!!!!!!!!!!!!!!!!!!
Emily20<-lmer(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar+(PrecZones+PrecZonVar+TempZones+TempZonVar|ID),weights=Area,  data=CrMaize8)
summary(Emily20)

Emily21<-lmer(Yield~PrecZones+TempZones+(PrecZones+PrecZonVar+TempZones+TempZonVar|ID),  data=CrMaize8)
summary(Emily21)


Emily22<-lmer(Yield~PrecZones+TempZones+(PrecZones+TempZones|ID),  data=CrMaize8)
summary(Emily22)
#-----------        try different optimizer   ---------------------------------         -----------------
Emily22NM <- update(Emily22,control=lmerControl(optimizer="Nelder_Mead"))
summary(Emily22NM)

# so maybe try the z-scores??

Rory<-lmer(Yield~PrecZscore+TempZscore+(PrecZscore+TempZscore|ID),  data=CrMaize8)
summary(Rory)

RoryNM <- update(Rory,control=lmerControl(optimizer="Nelder_Mead"))
summary(RoryNM)


Rory2<-lmer(Yield~PrecZscore+PrecZscoreVar+TempZscore+TempZscoreVar+(PrecZscore+TempZscore|ID),  data=CrMaize8)
summary(Rory2)

Rory2<-lmer(Yield~PrecZscore+PrecZscoreVar+TempZscore+TempZscoreVar+(TempZscoreVar+TempZscoreVar+PrecZscore+TempZscore|ID),  data=CrMaize8)
summary(Rory2)  # ok, this may be a specification problem


# or scaling?

#--------------

Emily22<-lmer(Yield~PrecZones+TempZones+(PrecZones+TempZones|ID),  data=CrMaize8)
summary(Emily22)
confint(Emily22)

Emily22s<-lmer(Yield~scale(PrecZones)+scale(TempZones)+(scale(PrecZones)+scale(TempZones)|ID),  data=CrMaize8)
summary(Emily22s)


Emily22s<-lmer(Yield~scale(PrecZones/84.36124)+scale(TempZones,center=FALSE)+(scale(PrecZones/84.36124)
                            +scale(TempZones,center=FALSE)|ID),  data=CrMaize8)
summary(Emily22s)
confint(Emily22s)

Emily2ss<-lmer(Yield~I(PrecZones/84.36124)+scale(TempZones,center=FALSE)+(I(PrecZones/84.36124)
                                                                         +scale(TempZones,center=FALSE)|ID),  data=CrMaize8)
summary(Emily2ss)
RX<-getME(Emily2ss, "RX")
sigma2<-sigma(Emily2ss)^2
sigma2*chol2inv(RX)
#--------------
# and actually, what about ML??

Emily22<-lmer(Yield~PrecZones+TempZones+(PrecZones+TempZones|ID), REML=FALSE, data=CrMaize8)
summary(Emily22)


Rory2<-lmer(Yield~PrecZscore+PrecZscoreVar+TempZscore+TempZscoreVar+(PrecZscore+TempZscore|ID),REML=FALSE,  data=CrMaize8)
summary(Rory2)



Emily220<-lmer(Yield~PrecZones+TempZones+(PrecZones+TempZones|ID),weights=Area,  data=CrMaize8)
summary(Emily220)