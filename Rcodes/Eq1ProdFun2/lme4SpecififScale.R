rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

load("~/foodSystems/dataFS/Main/CrMaize8.RData")
library(plm)
library(lme4)

# just trying to formalize what I have been doing in lme4StillLearning2
#---------------------------------------------------------------------------------------------------------------------------------------------------------------
Emily2s<-lmer(Yield~scale(PrecZones, center=FALSE)+PrecZonVar
              +scale(TempZones, center=FALSE)+TempZonVar+(scale(PrecZones, center=FALSE)+PrecZonVar
              +scale(TempZones, center=FALSE)+TempZonVar|ID),data=CrMaize8)  # now I know that it needs to be scaled to converge ok
summary(Emily2s)

Emily2sw<-lmer(Yield~scale(PrecZones, center=FALSE)+PrecZonVar
              +scale(TempZones, center=FALSE)+TempZonVar+(scale(PrecZones, center=FALSE)+PrecZonVar
           +scale(TempZones, center=FALSE)+TempZonVar|ID),weights=AreaSc/10, data=CrMaize8) # doesn't go very well...
# Area needs to be scaled too. Just got rid of the zero weight (one case) and devided by 10000 

summary(Emily2sw)
#---------------------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------------------------------------

Kyle1s<-lmer(Yield~scale(PrecZones, center=FALSE) +PrecZonVar+scale(TempZones, center=FALSE)
             +TempZonVar+  (scale(PrecZones, center=FALSE)+scale(TempZones, center=FALSE)|ID),data=CrMaize8)
summary(Kyle1s)

Kyle1sw<-lmer(Yield~scale(PrecZones, center=FALSE) +PrecZonVar+scale(TempZones, center=FALSE)
              +TempZonVar+  (scale(PrecZones, center=FALSE)+scale(TempZones, center=FALSE)|ID),
              weights=AreaSc,data=CrMaize8)
summary(Kyle1sw)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------------------------------------
Kyle2s<-lmer(Yield~scale(PrecZones, center=FALSE) +scale(TempZones, center=FALSE)
             +  (scale(PrecZones, center=FALSE)+scale(TempZones, center=FALSE)|ID),data=CrMaize8)
summary(Kyle2s)  # very similar to Kyle1s which is good
anova(Kyle2s)


Kyle2sw<-lmer(Yield~scale(PrecZones, center=FALSE) +scale(TempZones, center=FALSE)
              +  (scale(PrecZones, center=FALSE)+scale(TempZones, center=FALSE)|ID),
              weights=AreaSc,data=CrMaize8)

summary(Kyle2sw)  # very similar to Kyle2s which is good