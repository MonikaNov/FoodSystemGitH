rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

load("~/foodSystems/dataFS/Main/CrMaize8.RData")
library(plm)
#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# CONCLUSION (spoiler alert):
# SO THERE ARE COUPLE OF COUNTIES WHERE PREC. SINIFICANT EVEN IF SMALL SAMPLE REGRESSIONS FOR INDIVIDUAL COUNTIES..
#---------------------------------------------------------------------------------------------------------------------------------------------------------------

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
 
# Probably better no weights (ANnemie strobgly not in favor of them) so Dale32 the best for now

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

Dale30<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"), data=CrMaize8)
summary(Dale30)

Dale32<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"), effect="twoway",model="random",data=CrMaize8)
summary(Dale32)

plot(Dale32,type=c("p","smooth"))

Dale321<-plm(Yield~PrecZones+TempZones,index=c("ID","Year"), effect="twoway",model="random",data=CrMaize8)
summary(Dale321)

Kimmy1<-pggls(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"),effect="individual", data=CrMaize8)
summary(Kimmy1)
plot(Kimmy1,type=c("p","smooth"))

Kimmy1<-pggls(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"),effect="individual",model="random", data=CrMaize8)
summary(Kimmy1)

Kimmy1<-pggls(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"),effect="time",model="within", data=CrMaize8)
summary(Kimmy1)

Kimmy1<-pggls(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"),effect="time",model="random", data=CrMaize8)
summary(Kimmy1)


#---------------------------------------------------------------------------------------------------------------------------------------------------------------
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo


# now variable coefficient random = all regressors in fixed effects and in random effects. Pe liked it

Kimmy2<-pvcm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"),model=c("within"), data=CrMaize8)
summary(Kimmy2)
