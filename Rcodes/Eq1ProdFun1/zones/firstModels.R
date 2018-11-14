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

Andy1<-lm(Yield~PrecZones+PrecZonVar, data=CrMaize8,weights = Area)
summary(Andy1)

Andy2<-plm(Yield~PrecZones+PrecZonVar,index=c("ID","Year"), data=CrMaize8,weights = Area)
summary(Andy2)


Andy3<-plm(Yield~i_avTemp+i_avPrec+PrecZones+PrecZonVar,index=c("ID","Year"),weights = Area, data=CrMaize8)
summary(Andy3)

AndyVC<-pvcm(Yield~i_avTemp+i_avPrec+PrecZones+PrecZonVar,index=c("ID","Year"),weights = Area,model='within', data=CrMaize8)
summary(AndyVC)

pooltest(Andy3,AndyVC)  # cool. Seems like I can pool. Nope. vypada to, ze kdyz pouziju vahy area, tak ne pooling. Jinak jo


Andy2<-plm(Yield~PrecZones+PrecZonVar,index=c("ID","Year"), data=CrMaize8)
summary(Andy2)

AndyVC<-pvcm(Yield~PrecZones+PrecZonVar,index=c("ID","Year"),model='within', data=CrMaize8)
summary(AndyVC)

pooltest(Andy2,AndyVC) 

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Andy3<-plm(Yield~i_avPrec+i_LavPrec+i_avTemp+i_LavTemp+PrecZones+PrecZonVar,index=c("ID","Year"),weights = Area, data=CrMaize8)
summary(Andy3)

AndyVC<-pvcm(Yield~i_avPrec+i_LavPrec+i_avTemp+i_LavTemp+PrecZones+PrecZonVar,index=c("ID","Year"),weights = Area,model='within', data=CrMaize8)
summary(AndyVC)

pooltest(Andy3,AndyVC) 


Andy3<-plm(Yield~i_avPrec+i_LavPrec+i_avTemp+i_LavTemp+PrecZones+PrecZonVar,index=c("ID","Year"), data=CrMaize8)
summary(Andy3)

AndyVC<-pvcm(Yield~i_avPrec+i_LavPrec+i_avTemp+i_LavTemp+PrecZones+PrecZonVar,index=c("ID","Year"),model='within', data=CrMaize8)
summary(AndyVC)

pooltest(Andy3,AndyVC) 

#---------------------------
#here correct signs:-)

Andy3<-plm(Yield~i_avPrec+i_LavPrec+i_avTemp+i_LavTemp+PrecZones+PrecZonVar,model="random",index=c("ID","Year"),weights = Area, data=CrMaize8)
summary(Andy3)

Andy3<-plm(Yield~i_avPrec+i_LavPrec+i_avTemp+i_LavTemp,model="random",index=c("ID","Year"),weights = Area, data=CrMaize8)
summary(Andy3)

Andy3<-plm(Yield~i_avPrec+i_LavPrec+i_avTemp+i_LavTemp,model="pooling",index=c("ID","Year"),weights = Area, data=CrMaize8)
summary(Andy3)

Andy3<-plm(Yield~PrecZones+PrecZonVar,model="random",index=c("ID","Year"),weights = Area, data=CrMaize8)
summary(Andy3)