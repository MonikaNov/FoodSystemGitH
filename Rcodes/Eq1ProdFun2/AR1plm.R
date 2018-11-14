rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

load("~/foodSystems/dataFS/Main/CrMaize8.RData")
library(plm)
#---------------------------------------------------------------------------------------------------------------------------------------------------------------
Dale32<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"), effect="twoway",model="random",data=CrMaize8)
summary(Dale32)

Dale42<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"), weights=Area, effect="twoway",model="random",data=CrMaize8)
summary(Dale42)


#---------------------------------------------------------------------------------------------------------------------------------------------------------------
CrMaize9<-plm.data(CrMaize8, index=c("ID","Year"))
Sookie1<-pgmm(Yield~lag(Yield,c(1))+PrecZones +TempZones   | lag(Yield, 2:99),data=CrMaize9)
summary(Sookie1)

CrMaize9<-plm.data(CrMaize8, index=c("ID","Year"))
Sookie1<-pgmm(Yield~lag(Yield,c(1))+PrecZones +TempZones+PrecZonVar +TempZonVar  | lag(Yield, 2:99),data=CrMaize9)
summary(Sookie1)

Sookie1<-pgmm(Yield~lag(Yield,c(1))+TempZones | lag(Yield, 2:99),data=CrMaize9)
summary(Sookie1)

Sookie1<-pgmm(Yield~+TempZscore+PrecZscore | lag(Yield, 2:10),data=CrMaize9)
summary(Sookie1)

Sookie1<-pgmm(Yield~+TempZscore+PrecZscore | lag(Yield, 2:70),data=CrMaize9)
summary(Sookie1)

#SEEMS THAT NOT LAGGED DEPENDENT BUT AR(1) ERRORS !!!!!!!!!!!!!