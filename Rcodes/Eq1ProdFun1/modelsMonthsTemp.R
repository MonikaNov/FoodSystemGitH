rm(list=ls())

WDuni<-c("/home/m/mn/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

load("~/foodSystems/dataFS/Main/CrMaize5i_months2.RData")
load("~/foodSystems/dataFS/Main/climate4.RData")
library(plm)
#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
# I need to create indicators of drought

Agnes1<-lm(Yield~i_JanPrecZ+i_JanPrecZ+i_FebPrecZ+i_MarPrecZ+i_AprPrecZ+i_MayPrecZ+i_JunPrecZ+i_JulPrecZ+
              i_AugPrecZ+i_SepPrecZ+i_OctPrecZ+i_NovPrecZ+i_DecPrecZ
            +i_JanPrecZl+i_JanPrecZl+i_FebPrecZl+i_MarPrecZl+i_AprPrecZl+i_MayPrecZl+i_JunPrecZl+i_JulPrecZl+
              i_AugPrecZl+i_SepPrecZl+i_OctPrecZl+i_NovPrecZl+i_DecPrecZl
           +i_JanTempZ+i_FebTempZ+i_MarTempZ+i_AprTempZ+i_MayTempZ+i_JunTempZ+i_JulTempZ+
             i_AugTempZ+i_SepTempZ+i_OctTempZ+i_NovTempZ+i_DecTempZ
           +i_JanTempZl+i_JanTempZl+i_FebTempZl+i_MarTempZl+i_AprTempZl+i_MayTempZl+i_JunTempZl+i_JulTempZl+
             i_AugTempZl+i_SepTempZl+i_OctTempZl+i_NovTempZl+i_DecTempZl,
            data=CrMaize5)

summary(Agnes1)



Agnes2<-lm(Yield~i_JanPrecZ+i_JanPrecZ+i_FebPrecZ+i_MarPrecZ+i_AprPrecZ+i_MayPrecZ+i_JunPrecZ+i_JulPrecZ+
             i_AugPrecZ+i_SepPrecZ+i_OctPrecZ+i_NovPrecZ+i_DecPrecZ
           +i_JanPrecZl+i_JanPrecZl+i_FebPrecZl+i_MarPrecZl+i_AprPrecZl+i_MayPrecZl+i_JunPrecZl+i_JulPrecZl+
             i_AugPrecZl+i_SepPrecZl+i_OctPrecZl+i_NovPrecZl+i_DecPrecZl
          +i_JanTempZ+i_FebTempZ+i_MarTempZ+i_AprTempZ+i_MayTempZ+i_JunTempZ+i_JulTempZ+
             i_AugTempZ+i_SepTempZ+i_OctTempZ+i_NovTempZ+i_DecTempZ
           +i_JanTempZl+i_JanTempZl+i_FebTempZl+i_MarTempZl+i_AprTempZl+i_MayTempZl+i_JunTempZl+i_JulTempZl+
             i_AugTempZl+i_SepTempZl+i_OctTempZl+i_NovTempZl+i_DecTempZl,weight=Area,
           data=CrMaize5)

summary(Agnes2)


library(car)

vif(Agnes1)

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#panels 

Agnes3<-plm(Yield~i_JanPrecZ+i_JanPrecZ+i_FebPrecZ+i_MarPrecZ+i_AprPrecZ+i_MayPrecZ+i_JunPrecZ+i_JulPrecZ+
             i_AugPrecZ+i_SepPrecZ+i_OctPrecZ+i_NovPrecZ+i_DecPrecZ
           +i_JanPrecZl+i_JanPrecZl+i_FebPrecZl+i_MarPrecZl+i_AprPrecZl+i_MayPrecZl+i_JunPrecZl+i_JulPrecZl+
             i_AugPrecZl+i_SepPrecZl+i_OctPrecZl+i_NovPrecZl+i_DecPrecZl
         +i_JanTempZ+i_FebTempZ+i_MarTempZ+i_AprTempZ+i_MayTempZ+i_JunTempZ+i_JulTempZ+
             i_AugTempZ+i_SepTempZ+i_OctTempZ+i_NovTempZ+i_DecTempZ
           +i_JanTempZl+i_JanTempZl+i_FebTempZl+i_MarTempZl+i_AprTempZl+i_MayTempZl+i_JunTempZl+i_JulTempZl+
             i_AugTempZl+i_SepTempZl+i_OctTempZl+i_NovTempZl+i_DecTempZl,index=c("ID","Year"),
           data=CrMaize5)

summary(Agnes3)


Agnes4<-plm(Yield~i_JanPrecZ+i_JanPrecZ+i_FebPrecZ+i_MarPrecZ+i_AprPrecZ+i_MayPrecZ+i_JunPrecZ+i_JulPrecZ+
              i_AugPrecZ+i_SepPrecZ+i_OctPrecZ+i_NovPrecZ+i_DecPrecZ
            +i_JanPrecZl+i_JanPrecZl+i_FebPrecZl+i_MarPrecZl+i_AprPrecZl+i_MayPrecZl+i_JunPrecZl+i_JulPrecZl+
              i_AugPrecZl+i_SepPrecZl+i_OctPrecZl+i_NovPrecZl+i_DecPrecZl
           +i_JanTempZ+i_FebTempZ+i_MarTempZ+i_AprTempZ+i_MayTempZ+i_JunTempZ+i_JulTempZ+
              i_AugTempZ+i_SepTempZ+i_OctTempZ+i_NovTempZ+i_DecTempZ
            +i_JanTempZl+i_JanTempZl+i_FebTempZl+i_MarTempZl+i_AprTempZl+i_MayTempZl+i_JunTempZl+i_JulTempZl+
              i_AugTempZl+i_SepTempZl+i_OctTempZl+i_NovTempZl+i_DecTempZl,index=c("ID","Year"),weights=Area,
            data=CrMaize5)

summary(Agnes4)



#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Agnes5<-plm(Yield~ i_MarPrecZl+i_AugPrecZ+i_NovPrecZ+i_FebPrecZl+i_MarPrecZl+
              i_JulPrecZl+i_JunTempZ+ i_AugTempZ+i_OctTempZ    +i_OctTempZl+    
                 i_DecTempZl ,index=c("ID","Year"),
            data=CrMaize5)

summary(Agnes5)

Agnes5VC<-pvcm(Yield~ i_MarPrecZl+i_AugPrecZ+i_NovPrecZ+i_FebPrecZl+i_MarPrecZl+
              i_JulPrecZl+i_JunTempZ+ i_AugTempZ+i_OctTempZ    +i_OctTempZl+    
              i_DecTempZl ,index=c("ID","Year"),model="within",
            data=CrMaize5)

summary(Agnes5VC)
pooltest(Agnes5,Agnes5VC)

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Agnes5<-plm(Yield~ i_MarPrecZl+i_AugPrecZ+i_NovPrecZ+i_FebPrecZl+i_MarPrecZl+
              i_JulPrecZl+i_JunTempZ+ i_AugTempZ+i_OctTempZ    +i_OctTempZl+    
              i_DecTempZl ,index=c("ID","Year"),weights=Area,
            data=CrMaize5)

summary(Agnes5)

Agnes5VC<-pvcm(Yield~ i_MarPrecZl+i_AugPrecZ+i_NovPrecZ+i_FebPrecZl+i_MarPrecZl+
                 i_JulPrecZl+i_JunTempZ+ i_AugTempZ+i_OctTempZ    +i_OctTempZl+    
                 i_DecTempZl ,index=c("ID","Year"),model="within",weights=Area,
               data=CrMaize5)

summary(Agnes5VC)
pooltest(Agnes5,Agnes5VC)

library(car)

AgnesFoo<-lm(Yield~ i_MarPrecZl+i_AugPrecZ+i_NovPrecZ+i_FebPrecZl+i_MarPrecZl+
                 i_JulPrecZl+i_JunTempZ+ i_AugTempZ+i_OctTempZ    +i_OctTempZl+    
                 i_DecTempZl ,weight,
               data=CrMaize5)
vif(AgnesFoo)

AgnesFoo<-lm(Yield~ i_MarPrecZl+i_AugPrecZ+i_NovPrecZ+i_FebPrecZl+i_MarPrecZl+
               i_JulPrecZl+i_JunTempZ+ i_AugTempZ+i_OctTempZ    +i_OctTempZl+    
               i_DecTempZl ,
             data=CrMaize5)
vif(AgnesFoo)


# interesting. A bit different VIFs with and without weights..
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Agnes6<-plm(Yield~i_JanPrecZ+
              i_AugPrecZ+i_OctPrecZ+i_NovPrecZ+i_FebPrecZl+i_MarPrecZl
            +i_AprTempZ+i_MayTempZ+
              i_AugTempZ+i_DecTempZ
            +i_AprTempZl+i_MayTempZl+
              i_AugTempZl+i_SepTempZl+i_NovTempZl,index=c("ID","Year"),weights=Area,
            data=CrMaize5)

summary(Agnes6)

Agnes6VC<-pvcm(Yield~i_JanPrecZ+
              i_AugPrecZ+i_OctPrecZ+i_NovPrecZ+i_FebPrecZl+i_MarPrecZl
            +i_AprTempZ+i_MayTempZ+
              i_AugTempZ+i_DecTempZ
            +i_AprTempZl+i_MayTempZl+
              i_AugTempZl+i_SepTempZl+i_NovTempZl,index=c("ID","Year"),weights=Area,model="within",
            data=CrMaize5)
summary(Agnes6VC)

pooltest(Agnes6,Agnes6VC)