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

Dale1<-lm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar, data=CrMaize8,weights = Area)
summary(Dale1)


Dale2<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"), data=CrMaize8)
summary(Dale2)


Dale2<-lm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar+as.factor(ID), data=CrMaize8)
summary(Dale2)


Dale2<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"), data=CrMaize8,weights = Area)
summary(Dale2)
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#pooltests:
Dale4<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"), model='random', data=CrMaize8,weights = Area)
summary(Dale4)


Dale2<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"), model='within', data=CrMaize8,weights = Area)
summary(Dale2)
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


#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
# now z scores...

Denise1<-lm(Yield~PrecZscore+PrecZonVar+TempZscore+TempZonVar, data=CrMaize8)
summary(Denise1)


Denise2<-lm(Yield~PrecZscore+PrecZonVar+TempZscore+TempZonVar,weights = Area, data=CrMaize8)
summary(Denise2)

Dale1<-lm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar, data=CrMaize8,weights = Area)
summary(Dale1)

Denise3<-plm(Yield~PrecZscore+PrecZonVar+TempZscore+TempZonVar,index=c("ID","Year"),
             model='within',weights = Area,  data=CrMaize8)
summary(Denise3)

Denise3b<-plm(Yield~PrecZscore+PrecZonVar+TempZscore+TempZonVar,index=c("ID","Year"),
             model='within', data=CrMaize8)
summary(Denise3b)


Denise4<-plm(Yield~PrecZscore+PrecZonVar+TempZscore+TempZonVar,index=c("ID","Year"), model='random',weights = Area,  data=CrMaize8)
summary(Denise4)


DeniseVC<-pvcm(Yield~PrecZscore+PrecZonVar+TempZscore+TempZonVar,
               index=c("ID","Year"), model='within', data=CrMaize8,weights = Area)
summary(DeniseVC)

pooltest(Denise3,DeniseVC)


Denise3<-plm(Yield~PrecZscore+PrecZonVar+TempZscore+TempZonVar,index=c("ID","Year"), model='pooling',weights = Area,  data=CrMaize8)
summary(Denise3)
pooltest(Denise3,DeniseVC)


#--------------

#pooling test no weights


Denise3<-plm(Yield~PrecZscore+PrecZonVar+TempZscore+TempZonVar,index=c("ID","Year"), model='within',  data=CrMaize8)
summary(Denise3)

DeniseVC<-pvcm(Yield~PrecZscore+PrecZonVar+TempZscore+TempZonVar,index=c("ID","Year"), model='within', data=CrMaize8)
summary(DeniseVC)

pooltest(Denise3,DeniseVC)


Denise3<-plm(Yield~PrecZscore+PrecZonVar+TempZscore+TempZonVar,index=c("ID","Year"), model='pooling',  data=CrMaize8)
summary(Denise3)


pooltest(Denise3,DeniseVC)

#-----------------
Denise3<-plm(Yield~PrecZscore+PrecZonVar+TempZscore+TempZonVar,index=c("ID","Year"), model='within',  data=CrMaize8)
Denise6<-plm(Yield~PrecZscore+PrecZonVar+TempZscore+TempZonVar,index=c("ID","Year"), model='random',  data=CrMaize8)
phtest(Denise3,Denise6)


DeniseVC<-pvcm(Yield~PrecZscore+PrecZonVar+TempZscore+TempZonVar,index=c("ID","Year"), model='within', data=CrMaize8)
DeniseVC6<-pvcm(Yield~PrecZscore+PrecZonVar+TempZscore+TempZonVar,index=c("ID","Year"), model='random', data=CrMaize8)
phtest(DeniseVC, DeniseVC6)

DeniseVC<-pvcm(Yield~PrecZscore+PrecZonVar+TempZscore+TempZonVar,index=c("ID","Year"), model='random', data=CrMaize8)
summary(DeniseVC)

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
# now z scores and also weights z-scores. which is quite strange..

Eileen1<-lm(Yield~PrecZscore+PrecZscoreVar+TempZscore+TempZscoreVar, data=CrMaize8)
summary(Eileen1)

Eileen2<-lm(Yield~PrecZscore+PrecZscoreVar+TempZscore+TempZscoreVar,weights = Area, data=CrMaize8)
summary(Eileen2)

Eileen3b<-plm(Yield~PrecZscore+PrecZscoreVar+TempZscore+TempZscoreVar,index=c("ID","Year"),
             model='within',  data=CrMaize8)
summary(Eileen3b)

Eileen3<-plm(Yield~PrecZscore+PrecZscoreVar+TempZscore+TempZscoreVar,index=c("ID","Year"),
             model='within',weights = Area,  data=CrMaize8)
summary(Eileen3)

Eileen4<-plm(Yield~PrecZscore+PrecZscoreVar+TempZscore+TempZscoreVar,index=c("ID","Year"), model='random',weights = Area,  data=CrMaize8)
summary(Eileen4)



EileenVC<-pvcm(Yield~PrecZscore+PrecZscoreVar+TempZscore+TempZscoreVar,index=c("ID","Year"), model='within', data=CrMaize8,weights = Area)
summary(EileenVC)

pooltest(Eileen3,EileenVC)

Eileen3<-plm(Yield~PrecZscore+PrecZscoreVar+TempZscore+TempZscoreVar,index=c("ID","Year"), model='pooling',weights = Area,  data=CrMaize8)
summary(Eileen3)


pooltest(Eileen3,EileenVC)

# SO ITS NOT POOLABLE...................#
# poolability with weights now
Eileen4<-plm(Yield~PrecZscore+PrecZscoreVar+TempZscore+TempZscoreVar,index=c("ID","Year"),  data=CrMaize8)
summary(Eileen4)

EileenVC<-pvcm(Yield~PrecZscore+PrecZscoreVar+TempZscore+TempZscoreVar,index=c("ID","Year"), model='within', data=CrMaize8)
summary(EileenVC)

pooltest(Eileen4,EileenVC)

# DALES ARE PROBABLY THE BEST !!!!!!!!!!