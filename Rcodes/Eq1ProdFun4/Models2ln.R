rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)

library('dplyr')
library('tseries')
library(plm)
library(lme4)
library(lattice)
library(car)
library(lmerTest)



load("Main/CrMaize16.RData")
CrMaize16ts<-pdata.frame(CrMaize16,index=c("ID","Year"))


#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# first models (a suggestion from email 20.5.2018): lmer(yield~rain+temp+cv_rain+cv_temp+(1+rain+temp|counties))

Keith1<-lmer(Yield~PreMedZ +TemMedZ  + PreMedCVz+TemMedCVz +(PreMedZ+TemMedZ|ID),data=CrMaize16ts)
summary(Keith1) # to compare

Rachel1<-lmer(log(Yield)~PreMedZ +TemMedZ  + PreMedCVz+TemMedCVz +(PreMedZ+TemMedZ|ID),data=CrMaize16ts)
summary(Rachel1) # smaller criterion than Keith 1-> better fit

# squares

Keith3<-lmer(Yield~PreMedZ + I(PreMedZ^2)+ TemMedZ +I(TemMedZ^2) + PreMedCVz+I(PreMedCVz^2) +
               TemMedCVz +I(TemMedCVz^2)  +(PreMedZ+I(PreMedZ^2)  + TemMedZ +I(TemMedZ^2) |ID),data=CrMaize16ts)
summary(Keith3)

Rachel3<-lmer(log(Yield)~PreMedZ + I(PreMedZ^2)+ TemMedZ +I(TemMedZ^2) + PreMedCVz+I(PreMedCVz^2) +
               TemMedCVz +I(TemMedCVz^2)  +(PreMedZ+I(PreMedZ^2)  + TemMedZ +I(TemMedZ^2) |ID),data=CrMaize16ts)
summary(Rachel3)

Rachel7<-lmer(log(Yield)~PreMed + I(PreMed^2)+ TemMed +I(TemMed^2) + PreMedCV+I(PreMedCV^2) +
               TemMedCV +I(TemMedCV^2)  +(PreMedZ+I(PreMed^2)  + TemMedZ+I(TemMed^2) |ID),data=CrMaize16ts)
summary(Rachel7) # to compare

Keith7<-lmer(Yield~PreMed + I(PreMed^2)+ TemMed +I(TemMed^2) + PreMedCV+I(PreMedCV^2) +
               TemMedCV +I(TemMedCV^2)  +(PreMed+I(PreMed^2)  + TemMed +I(TemMed^2) |ID),data=CrMaize16ts)
summary(Keith7) 

# squares and scaling  #------------------------------------------------------------------------------------------------------------------------------------------------------

Rachel7s<-lmer(log(Yield)~scale(PreMed) + I(scale(PreMed)^2)+ scale(TemMed) +I(scale(TemMed)^2) + scale(PreMedCV)+I(scale(PreMedCV)^2) +
                scale(TemMedCV) +I(scale(TemMedCV)^2)  +(scale(PreMed)+I(scale(PreMed)^2)  + scale(TemMed)
                                                          +I(scale(TemMed)^2) |ID),data=CrMaize16ts)
summary(Rachel7s) # nice Rachel7s and Keith7s. according to REML, Rachel better

Keith7s<-lmer(Yield~scale(PreMed) + I(scale(PreMed)^2)+ scale(TemMed) +I(scale(TemMed)^2) + scale(PreMedCV)+I(scale(PreMedCV)^2) +
                scale(TemMedCV) +I(scale(TemMedCV)^2)  +(scale(PreMed)+I(scale(PreMed)^2) 
                                                         + scale(TemMed) +I(scale(TemMed)^2) |ID),data=CrMaize16ts)
summary(Keith7s) 

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

# ---------- ----------- now the means  #------------------------------------------------------------------------------------------------------------------------------------------------------

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo



Keith2<-lmer(Yield~PreMeanZ +TemMeanZ  + PreMeanCVz+TemMeanCVz +(PreMeanZ+TemMeanZ|ID),data=CrMaize16ts)
summary(Keith2) # to compare

Rachel2<-lmer(log(Yield)~PreMeanZ +TemMeanZ  + PreMeanCVz+TemMeanCVz +(PreMeanZ+TemMeanZ|ID),data=CrMaize16ts)
summary(Rachel2) # smaller criterion than Keith 1-> better fit

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# squares and raw

Rachel8<-lmer(log(Yield)~PreMean + I(PreMean^2)+ TemMean +I(TemMean^2) + PreMeanCV+I(PreMeanCV^2) +
                TemMeanCV +I(TemMeanCV^2)  +(PreMean+I(PreMean^2)  + TemMean +I(TemMean^2) |ID),data=CrMaize16ts)
summary(Rachel8) # to compare

Keith8<-lmer(Yield~PreMean + I(PreMean^2)+ TemMean +I(TemMean^2) + PreMeanCV+I(PreMeanCV^2) +
               TemMeanCV +I(TemMeanCV^2)  +(PreMean+I(PreMean^2)  + TemMean +I(TemMean^2) |ID),data=CrMaize16ts)
summary(Keith8) 
# neither on econverges

# squares and scaling  #------------------------------------------------------------------------------------------------------------------------------------------------------

Rachel8s<-lmer(log(Yield)~scale(PreMean) + I(scale(PreMean)^2)+ scale(TemMean) +I(scale(TemMean)^2) + scale(PreMeanCV)+I(scale(PreMeanCV)^2) +
                 scale(TemMeanCV) +I(scale(TemMeanCV)^2)  +(scale(PreMean)+I(scale(PreMean)^2)  + scale(TemMean) +I(scale(TemMean)^2) |ID),data=CrMaize16ts)
summary(Rachel8s) # this looks nice

Keith8s<-lmer(Yield~scale(PreMean) + I(scale(PreMean)^2)+ scale(TemMean) +I(scale(TemMean)^2)
              + scale(PreMeanCV)+I(scale(PreMeanCV)^2) +
                scale(TemMeanCV) +I(scale(TemMeanCV)^2)  +(scale(PreMean)
                                                           +I(scale(PreMean)^2)  + scale(TemMean) +I(scale(TemMean)^2) |ID),data=CrMaize16ts)
summary(Keith8s) # so here the linear converges and non-linear does not


Keith4<-lmer(Yield~PreMeanZ + I(PreMeanZ^2)+ TemMeanZ +I(TemMeanZ^2) + PreMeanCVz+I(PreMeanCVz^2) +
               TemMeanCVz +I(TemMeanCVz^2)  +(PreMeanZ+I(PreMeanZ^2)  + TemMeanZ +I(TemMeanZ^2) |ID),data=CrMaize16ts)
summary(Keith4)

Rachel4<-lmer(log(Yield)~PreMeanZ + I(PreMeanZ^2)+ TemMeanZ +I(TemMeanZ^2) + PreMeanCVz+I(PreMeanCVz^2) +
               TemMeanCVz +I(TemMeanCVz^2)  +(PreMeanZ+I(PreMeanZ^2)  + TemMeanZ +I(TemMeanZ^2) |ID),data=CrMaize16ts)
summary(Rachel4)
