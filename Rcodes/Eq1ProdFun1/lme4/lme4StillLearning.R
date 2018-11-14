rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

load("~/foodSystems/dataFS/Main/CrMaize8.RData")
library(plm)
#---------------------------------------------------------------------------------------------------------------------------------------------------------------
Emily2<-lmer(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar+(PrecZones+PrecZonVar+TempZones+TempZonVar|ID),data=CrMaize8)
summary(Emily2)

Emily20<-lmer(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar+(PrecZones+PrecZonVar+TempZones+TempZonVar|ID),
              weights=Area,  data=CrMaize8)
summary(Emily20)

Emily200 <- update(Emily20,control=lmerControl(optimizer="Nelder_Mead"))
summary(Emily200)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
Kyle1<-lmer(Yield~PrecZones+TempZones+(PrecZones+TempZones|ID),data=CrMaize8)
summary(Kyle1)

# EVEN THE ORDER HERE GIVES DIFFERENT RESULTS
Kyle1<-lmer(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar+(TempZones+PrecZones|ID),data=CrMaize8)


# not the same!!! Kyle1<-lmer(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar+(PrecZones|ID)+(TempZones|ID),data=CrMaize8)


Kyle1w<-lmer(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar+(PrecZones+TempZones|ID),weights=Area,  data=CrMaize8)
summary(Kyle1w)

# scaling
#---------------------------------------------------------------------------------------------------------------------------------------------------------------
Kyle1s<-lmer(Yield~scale(PrecZones, center=FALSE) +PrecZonVar+scale(TempZones, center=FALSE)
             +TempZonVar+  (scale(PrecZones, center=FALSE)+scale(TempZones, center=FALSE)|ID),data=CrMaize8)
summary(Kyle1s)

Kyle1s0 <- update(Kyle1s,control=lmerControl(optimizer="Nelder_Mead")) # robustness
summary(Kyle1s0) #cool, seems to be exactly the same


Kyle1sw<-lmer(Yield~scale(PrecZones, center=FALSE) +PrecZonVar+scale(TempZones, center=FALSE)
              +TempZonVar+  (scale(PrecZones, center=FALSE)+scale(TempZones, center=FALSE)|ID),
              weights=AreaSc,data=CrMaize8)
summary(Kyle1sw)

Kyle1sw0 <- update(Kyle1sw,control=lmerControl(optimizer="Nelder_Mead")) # robustness
summary(Kyle1sw0) #cool, seems to be exactly the same
#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# now try to remove the var variables? they don't seem to be very significant...

Kyle2s<-lmer(Yield~scale(PrecZones, center=FALSE) +scale(TempZones, center=FALSE)
             +  (scale(PrecZones, center=FALSE)+scale(TempZones, center=FALSE)|ID),data=CrMaize8)
summary(Kyle2s)  # very similar to Kyle1s which is good


Kyle2sw<-lmer(Yield~scale(PrecZones, center=FALSE) +scale(TempZones, center=FALSE)
              +  (scale(PrecZones, center=FALSE)+scale(TempZones, center=FALSE)|ID),
              weights=AreaSc,data=CrMaize8)
summary(Kyle2sw)  # very similar to Kyle2s which is good
