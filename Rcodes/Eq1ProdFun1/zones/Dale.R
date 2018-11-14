rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

load("~/foodSystems/dataFS/Main/CrMaize8.RData")
library(plm)

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

# 

Dale1<-lm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar, data=CrMaize8)
summary(Dale1)

Dale2<-lm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar, data=CrMaize8,weights = Area)
summary(Dale2)

Dale3<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"), data=CrMaize8)
summary(Dale3)

Dale4<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"),weights = Area, data=CrMaize8)
summary(Dale4)

pFtest(Dale4,Dale2)  # null >>OLS is better. so, in this case, panel with individual effects is better...
pFtest(Dale3,Dale1)

Dale31<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"),effect='time', data=CrMaize8)
summary(Dale31)

Dale32<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"),effect='twoways', data=CrMaize8)
summary(Dale32)

pFtest(Dale32,Dale1) 
pFtest(Dale31,Dale1)  # null: OLS better than fixed effects

Dale41<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"),effect='time',weights = Area, data=CrMaize8)
summary(Dale41)

Dale42<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"),effect='twoways',weights = Area, data=CrMaize8)
summary(Dale42)


pFtest(Dale42,Dale2) 
pFtest(Dale41,Dale2) 

Dale2<-lm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar+as.factor(ID), data=CrMaize8)
summary(Dale2)


Dale2<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"), data=CrMaize8,weights = Area)
summary(Dale2)
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#pooltests:
Dale4<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"), model='random', data=CrMaize8,weights = Area)
summary(Dale4)

Dale6<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"), model='random',random.method="amemiya",
           data=CrMaize8,weights = Area)
summary(Dale6)

Dale7<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"), model='random',
           data=CrMaize8,weights = Area)
summary(Dale7)

Dale7<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"), model='random',random.models="within",
           data=CrMaize8,weights = Area)
summary(Dale7)






Dale4<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"), model='random', data=CrMaize8,weights = Area)
summary(Dale4)
ercomp(Dale4)

Dale2<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"), model='within', data=CrMaize8,weights = Area)
summary(Dale2)
fixef(Dale2)
DaleVC<-pvcm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"), model='within', data=CrMaize8,weights = Area)
summary(DaleVC)

pooltest(Dale2,DaleVC)

Dale2<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"), model='pooling', data=CrMaize8,weights = Area)
summary(Dale2)


pooltest(Dale2,DaleVC)

#----- pooltests no weights:

Dale2<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"), model='within', data=CrMaize8)
summary(Dale2)
DaleVC<-pvcm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"), model='within', data=CrMaize8)
summary(DaleVC)

pooltest(Dale2,DaleVC)

#----- pooltests two ways :
Dale8<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"), model='within',effect="twoways", data=CrMaize8)
summary(Dale8)

fixef(Dale8, effect ="time")
summary(fixef(Dale8, effect ="time"))
summary(fixef(Dale8, effect ="individual"))
fixef(Dale8, effect ="twoways")

pooltest(Dale8,DaleVC)



# ËHM, NOW WITH WEIGHTS..

Dale9<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"), model='within',effect="twoways",weights=Area, data=CrMaize8)
summary(Dale9)

Dale9VC<-pvcm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"), model='within',weights=Area, data=CrMaize8)
summary(Dale9VC)
fixef(Dale9VC)

pooltest(Dale9,Dale9VC)


#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# variable coefficients..Fixed or random and so...


Dale11<-pvcm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"), model='within',weights=Area, data=CrMaize8)
summary(Dale11)

Dale12<-pvcm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"), model='random',weights=Area, data=CrMaize8)
summary(Dale12)

phtest(Dale11,Dale12)

Dale42<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"), model='within', data=CrMaize8,weights = Area)
summary(Dale42)
Dale4<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"), model='random', data=CrMaize8,weights = Area)
summary(Dale4)

phtest(Dale42,Dale4)

library(apsrtable)
apsrtable(Dale1,Dale2, model.names= c("OLS unweighted", "OLS weighted"))

Dale432<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"), model='within', data=CrMaize8)
summary(Dale432)
Dale43<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"), model='random', data=CrMaize8)
summary(Dale43)

phtest(Dale432,Dale43)  # so the unique errors are correlated with regressors, I NEED FIXED EFFECTS


#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# playing 16.5.2018

Dale89<-pvcm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"), model='random',weights=Area, data=CrMaize8)
summary(Dale89)

Dale88<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"),weights=Area, data=CrMaize8)
summary(Dale88)