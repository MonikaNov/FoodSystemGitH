rm(list=ls())

WDuni<-c("/home/m/mn/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

load("~/foodSystems/dataFS/Main/CrMaize5i_months.RData")
load("~/foodSystems/dataFS/Main/CrMaize5i_months2.RData")
load("~/foodSystems/dataFS/Main/climate4.RData")
library(plm)
#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
# I need to create indicators of drought

Dianne1<-lm(Yield~i_JanPrecZ+i_JanPrecZ+i_FebPrecZ+i_MarPrecZ+i_AprPrecZ+i_MayPrecZ+i_JunPrecZ+i_JulPrecZ+
              i_AugPrecZ+i_SepPrecZ+i_OctPrecZ+i_NovPrecZ+i_DecPrecZ
            +i_JanPrecZl+i_JanPrecZl+i_FebPrecZl+i_MarPrecZl+i_AprPrecZl+i_MayPrecZl+i_JunPrecZl+i_JulPrecZl+
              i_AugPrecZl+i_SepPrecZl+i_OctPrecZl+i_NovPrecZl+i_DecPrecZl,
            data=CrMaize5)

summary(Dianne1)


Dianne2<-lm(Yield~i_JanPrecZ+i_JanPrecZ+i_FebPrecZ+i_MarPrecZ+i_AprPrecZ+i_MayPrecZ+i_JunPrecZ+i_JulPrecZ+
              i_AugPrecZ+i_SepPrecZ+i_OctPrecZ+i_NovPrecZ+i_DecPrecZ
            +i_JanPrecZl+i_JanPrecZl+i_FebPrecZl+i_MarPrecZl+i_AprPrecZl+i_MayPrecZl+i_JunPrecZl+i_JulPrecZl+
              i_AugPrecZl+i_SepPrecZl+i_OctPrecZl+i_NovPrecZl+i_DecPrecZl,weights=Area,
            data=CrMaize5)

summary(Dianne2)

library(car)

vif(Dianne1)

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#panels 
Dianne3<-plm(Yield~i_JanPrecZ+i_JanPrecZ+i_FebPrecZ+i_MarPrecZ+i_AprPrecZ+i_MayPrecZ+i_JunPrecZ+i_JulPrecZ+
              i_AugPrecZ+i_SepPrecZ+i_OctPrecZ+i_NovPrecZ+i_DecPrecZ
            +i_JanPrecZl+i_JanPrecZl+i_FebPrecZl+i_MarPrecZl+i_AprPrecZl+i_MayPrecZl+i_JunPrecZl+i_JulPrecZl+
              i_AugPrecZl+i_SepPrecZl+i_OctPrecZl+i_NovPrecZl+i_DecPrecZl,weights=Area,
            data=CrMaize5,index=c("ID","Year"))

summary(Dianne3)

Dianne3<-plm(Yield~i_JanPrecZ+i_JanPrecZ+i_FebPrecZ+i_MarPrecZ+i_AprPrecZ+i_MayPrecZ+i_JunPrecZ+i_JulPrecZ+
               i_AugPrecZ+i_SepPrecZ+i_OctPrecZ+i_NovPrecZ+i_DecPrecZ
             +i_JanPrecZl+i_JanPrecZl+i_FebPrecZl+i_MarPrecZl+i_AprPrecZl+i_MayPrecZl+i_JunPrecZl+i_JulPrecZl+
               i_AugPrecZl+i_SepPrecZl+i_OctPrecZl+i_NovPrecZl+i_DecPrecZl,weights=Area,
             data=CrMaize5,index=c("ID","Year"),model="pooling")

summary(Dianne3)



Dianne4<-plm(Yield~i_JanPrecZ+i_JanPrecZ+i_FebPrecZ+i_MarPrecZ+i_AprPrecZ
             +i_MayPrecZ+i_JunPrecZ+i_JulPrecZ+
               i_AugPrecZ+i_SepPrecZ+i_OctPrecZ+i_NovPrecZ+i_DecPrecZ
             +i_JanPrecZl+i_JanPrecZl+i_FebPrecZl+i_MarPrecZl+i_AprPrecZl
             +i_MayPrecZl+i_JunPrecZl+i_JulPrecZl+
               i_AugPrecZl+i_SepPrecZl+i_OctPrecZl+i_NovPrecZl+i_DecPrecZl,
             data=CrMaize5,index=c("ID","Year"))

summary(Dianne4)

Dianne3<-pggls(Yield~i_JanPrecZ+i_JanPrecZ+i_FebPrecZ+i_MarPrecZ+i_AprPrecZ
               +i_MayPrecZ+i_JunPrecZ+i_JulPrecZ+
               i_AugPrecZ+i_SepPrecZ+i_OctPrecZ+i_NovPrecZ+i_DecPrecZ
             +i_JanPrecZl+i_JanPrecZl+i_FebPrecZl+i_MarPrecZl+i_AprPrecZl+i_MayPrecZl
             +i_JunPrecZl+i_JulPrecZl+
               i_AugPrecZl+i_SepPrecZl+i_OctPrecZl+i_NovPrecZl+i_DecPrecZl,weights=Area,
             data=CrMaize5,index=c("ID","Year"))

summary(Dianne3)



#-------------------------------------------------------------------------------------------------------------------------------------------



Shelly1<-plm(Yield~i_FebPrecZ+i_DecPrecZ+i_MarPrecZl+i_AprPrecZl+i_JulPrecZl+i_DecPrecZl,weights=Area,
             data=CrMaize5,index=c("ID","Year"))

summary(Shelly1)



ShellyVCM<-pvcm(Yield~i_FebPrecZ+i_DecPrecZ+i_MarPrecZl+i_AprPrecZl+i_JulPrecZl+i_DecPrecZl,weights=Area,
             data=CrMaize5,index=c("ID","Year"),model="within")

summary(ShellyVCM)

pooltest(Shelly1,ShellyVCM)

Shelly2<-lm(Yield~i_FebPrecZ+i_DecPrecZ+i_MarPrecZl+i_AprPrecZl+i_JulPrecZl+i_DecPrecZl,weights=Area,
             data=CrMaize5)

vif(Shelly2)

# -------no weights pooltest --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Shelly4<-plm(Yield~i_FebPrecZ+i_DecPrecZ+i_MarPrecZl+i_AprPrecZl+i_JulPrecZl+i_DecPrecZl,
             data=CrMaize5,index=c("ID","Year"))

summary(Shelly4)


ShellyVCM<-pvcm(Yield~i_FebPrecZ+i_DecPrecZ+i_MarPrecZl+i_AprPrecZl+i_JulPrecZl+i_DecPrecZl,
                data=CrMaize5,index=c("ID","Year"),model="within")

summary(ShellyVCM)

pooltest(Shelly4,ShellyVCM)
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


Audry1<-plm(Yield~i_JanPrecZ+i_JanPrecZ+i_FebPrecZ+i_MarPrecZ+i_AprPrecZ+i_MayPrecZ+i_JunPrecZ+i_JulPrecZ+
               i_AugPrecZ+i_SepPrecZ+i_OctPrecZ+i_NovPrecZ+i_DecPrecZ,weights=Area,
             data=CrMaize5,index=c("ID","Year"))

summary(Audry1)

Audry2<-pvcm(Yield~i_JanPrecZ+i_JanPrecZ+i_FebPrecZ+i_MarPrecZ+i_AprPrecZ+i_MayPrecZ+i_JunPrecZ+i_JulPrecZ+
              i_AugPrecZ+i_SepPrecZ+i_OctPrecZ+i_NovPrecZ+i_DecPrecZ,weights=Area,
            data=CrMaize5,index=c("ID","Year"),model="within")

summary(Audry2)

pooltest(Audry1,Audry2)


# try Lasso if they too correlated
# now I am doing correlation tests - I want to corelate each month with ech other:

cor(CrMaize5[11:33])
apply(CrMaize5[11:33],2, function (x) cor.test(unlist(CrMaize5[11]),x))

   apply(CrMaize5[11:33],2, function(x) apply(CrMaize5[11:33],2, function (y) cor.test(y,x)))

   # COOL
   
   MyCorelations<-apply(CrMaize5[11:34],2, function(x) apply(CrMaize5[11:34],2, function (y) cor.test(y,x)))

lapply(MyCorelations[[2]], print)

lapply(MyCorelations, length)
MyCorelations[[2]][1]
unlist(MyCorelations[[2]][3])[3]


Mycors3<-simplify2array(MyCorelations, higher = TRUE)
   unlist(Mycors3[1,3])[3]

Mycors4<-simplify2array(Mycors3,1:2, function(x) as.numeric(unlist(x)[3]))
Mycors4

# checking:
cor.test(unlist(CrMaize5[13]),unlist(CrMaize5[33]))

# now statistics of the p-values---------------------------------------------------------------------------------------------------------------------

mean(Mycors4)

mean(c(Mycors4))

median(c(Mycors4))

hist(c(Mycors4),100)

hist(c(Mycors4),20,plot=FALSE)

boxplot(c(Mycors4))
dim(Mycors4)

summary(Mycors4)
apply(as.matrix(Mycors4),1:2,mean)