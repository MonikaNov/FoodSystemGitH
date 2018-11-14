rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

load("Main/CrMaize8.RData")
library(plm)
library(lme4)

Laika1<-plm(Yield~PrecZscore +TempZscore ,index=c("ID","Year"), effect="twoway",model="random",data=CrMaize8)
summary(Laika1)

LaikaW1<-plm(Yield~PrecZscore +PrecZonVar,weights=AreaSc  ,index=c("ID","Year"), effect="twoway",model="random",data=CrMaize8)
summary(LaikaW1)

# --------------------  pvcm ---------------------------------------------------------------------------------------------------------------
Amick1<-pvcm(Yield~PrecZscore+TempZscore,index=c("ID","Year"),model=c("within"),data=CrMaize8)
summary(Amick1)

# now weighted pvcm
Amick2<-pvcm(Yield~PrecZscore+TempZscore+PrecZonVar,index=c("ID","Year"),model=c("within"),data=CrMaize8)
summary(Amick2)

Amick3<-pvcm(Yield~PrecZscore+TempZscore+PrecZonVar,index=c("ID","Year"),model=c("random"),data=CrMaize8)
summary(Amick3)

