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

# Dale30 and Dale 40 are the models which I choose as a base for now and I will use them to go throw the specification testss...


Dale30<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"), data=CrMaize8)
summary(Dale30)

Dale40<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"),
            weights = Area, data=CrMaize8)
summary(Dale40)

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 1. Tests of poolability

Dale3vc<-pvcm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"),model="within", data=CrMaize8)
summary(Dale3vc)

pooltest(Dale30,Dale3vc)

Dale31<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"),model="pooling", data=CrMaize8)
summary(Dale31)

pooltest(Dale31,Dale3vc)

#--- -  ---- -   --------    ---------   -------    - - - - - - - --- -  ----      ------- -----      -----------        ---- ---     -----------
Dale4vc<-pvcm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"),model="within",
            weights = Area, data=CrMaize8)
summary(Dale4vc)

pooltest(Dale40,Dale4vc)

Dale41<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"),model="pooling",
            weights = Area, data=CrMaize8)
summary(Dale41)

pooltest(Dale41,Dale4vc)

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------
# now lets go...
Dale30<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"), data=CrMaize8)
summary(Dale30)

Dale99<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"),model="random", effect="individual",
            random.method="amemiya",data=CrMaize8)
summary(Dale99)


Dale99<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"),model="random", effect="twoway",
            random.method="amemiya",data=CrMaize8)
summary(Dale99)

fixef(Dale99)



Dale99<-pvcm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"),model="within",
          data=CrMaize8)
summary(Dale99)

Dale999<-pvcm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"),model="random",
             data=CrMaize8)
summary(Dale999)

Dale98<-pvcm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar+as.factor(ID)-1,index=c("ID","Year"),model="within",
             random.method="amemiya",data=CrMaize8)
summary(Dale98)

Dale97<-pggls(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"),model="within",
             data=CrMaize8)
summary(Dale97)

Dale99<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"),model="within",
              data=CrMaize8)
summary(Dale99)

Dale96<-pggls(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"),model="within",effect="time",
              data=CrMaize8)
summary(Dale96)

Dale96<-pggls(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"),model="random",effect="time",
              data=CrMaize8)
summary(Dale96)


Dale99<-pvcm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"),model="random"
            ,data=CrMaize8)
summary(Dale99)
Dale99$coefficients