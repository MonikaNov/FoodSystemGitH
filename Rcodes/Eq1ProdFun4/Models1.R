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
summary(Keith1) 

Keith1w<-lmer(Yield~PreMedZ +TemMedZ  + PreMedCVz+TemMedCVz +(PreMedZ+TemMedZ|ID),weights=Area/1000,data=CrMaize16ts)
summary(Keith1w) # Area divided by 1000, otherwise problems with convergence

Keith2<-lmer(Yield~PreMeanZ +TemMeanZ  + PreMeanCVz+TemMeanCVz +(PreMeanZ+TemMeanZ|ID),data=CrMaize16ts)
summary(Keith2) 

Keith2w<-lmer(Yield~PreMeanZ +TemMeanZ  + PreMeanCVz+TemMeanCVz +(PreMeanZ+TemMeanZ|ID),weights=Area/1000,data=CrMaize16ts)
summary(Keith2w) # Area divided by 1000, otherwise problems with convergence




Keith3<-lmer(Yield~PreMedZ + I(PreMedZ^2)+ TemMedZ +I(TemMedZ^2) + PreMedCVz+I(PreMedCVz^2) +
               TemMedCVz +I(TemMedCVz^2)  +(PreMedZ+I(PreMedZ^2)  + TemMedZ +I(TemMedZ^2) |ID),data=CrMaize16ts)
summary(Keith3) 
Keith3w<-lmer(Yield~PreMedZ + I(PreMedZ^2)+ TemMedZ +I(TemMedZ^2) + PreMedCVz+I(PreMedCVz^2) +
               TemMedCVz +I(TemMedCVz^2)  +(PreMedZ+I(PreMedZ^2)  + TemMedZ +I(TemMedZ^2) |ID),weights=Area/1000,data=CrMaize16ts)
summary(Keith3w) 

Keith4<-lmer(Yield~PreMeanZ + I(PreMeanZ^2)+ TemMeanZ +I(TemMeanZ^2) + PreMeanCVz+I(PreMeanCVz^2) +
               TemMeanCVz +I(TemMeanCVz^2)  +(PreMeanZ+I(PreMeanZ^2)  + TemMeanZ +I(TemMeanZ^2) |ID),data=CrMaize16ts)
summary(Keith4) # based on the REML criterion at convergence, unweighted model seems to be much better
                # Based on the REML criterion at convergence, Keith 1 (that is the model without squares) seems to be a better fit 
                # seems that medians are a slightly better fit than means


Keith4w<-lmer(Yield~PreMeanZ + I(PreMeanZ^2)+ TemMeanZ +I(TemMeanZ^2) + PreMeanCVz+I(PreMeanCVz^2) +
                TemMeanCVz +I(TemMeanCVz^2)  +(PreMeanZ+I(PreMeanZ^2)  + TemMeanZ +I(TemMeanZ^2) |ID),weights=Area/1000,data=CrMaize16ts)
summary(Keith4w) 

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#                               RAW DATA (INSTEAD OF THE Z-SCORE)
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# medians
Keith5<-lmer(Yield~PreMed +TemMed  + PreMedCV+TemMedCV +(PreMed+TemMed|ID),data=CrMaize16ts)
summary(Keith5) 
Keith5w<-lmer(Yield~PreMed +TemMed  + PreMedCV+TemMedCV +(PreMed+TemMed|ID),weights=Area/1000,data=CrMaize16ts)
summary(Keith5w) # Area divided by 1000, otherwise problems with convergence

# means
Keith6<-lmer(Yield~PreMean +TemMean  + PreMeanCV+TemMeanCV +(PreMean+TemMean|ID),data=CrMaize16ts)
summary(Keith6) 
Keith6w<-lmer(Yield~PreMean +TemMean  + PreMeanCV+TemMeanCV +(PreMean+TemMean|ID),weights=Area/1000,data=CrMaize16ts)
summary(Keith6w)       # Area divided by 1000, otherwise problems with convergence

# ------------              squares     ----------------------------------------        ----------------              -

Keith7<-lmer(Yield~PreMed + I(PreMed^2)+ TemMed +I(TemMed^2) + PreMedCV+I(PreMedCV^2) +
               TemMedCV +I(TemMedCV^2)  +(PreMedZ+I(PreMed^2)  + TemMed +I(TemMed^2) |ID),data=CrMaize16ts)
summary(Keith7) 
Keith7w<-lmer(Yield~PreMed + I(PreMed^2)+ TemMed +I(TemMed^2) + PreMedCV+I(PreMedCV^2) +
                TemMedCV +I(TemMedCV^2)  +(PreMed+I(PreMed^2)  + TemMed +I(TemMed^2) |ID),weights=Area/1000,data=CrMaize16ts)
summary(Keith7w) 

Keith8<-lmer(Yield~PreMean + I(PreMean^2)+ TemMean +I(TemMean^2) + PreMeanCV+I(PreMeanCV^2) +
               TemMeanCV +I(TemMeanCV^2)  +(PreMean+I(PreMean^2)  + TemMean +I(TemMean^2) |ID),data=CrMaize16ts)
summary(Keith8) 
Keith8w<-lmer(Yield~PreMean + I(PreMean^2)+ TemMean +I(TemMean^2) + PreMeanCV+I(PreMeanCV^2) +
                TemMeanCV +I(TemMeanCV^2)  +(PreMean+I(PreMean^2)  + TemMean +I(TemMean^2) |ID),weights=Area/1000,data=CrMaize16ts)
summary(Keith8w) 

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#                              NOW RAW DATA WITH SCALING/STANDARDIZATION
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# means
Keith5s<-lmer(Yield~scale(PreMed) +scale(TemMed) 
+ scale(PreMedCV)+scale(TemMedCV) +(scale(PreMed)+scale(TemMed)|ID),data=CrMaize16ts)
summary(Keith5s) 
# medians
# oh, much nicer..

# ------------              squares     ----------------------------------------        ----------------              -

Keith7s<-lmer(Yield~scale(PreMed) + I(scale(PreMed)^2)+ scale(TemMed) +I(scale(TemMed)^2) + scale(PreMedCV)+I(scale(PreMedCV)^2) +
    scale(TemMedCV) +I(scale(TemMedCV)^2)  +(scale(PreMed)+I(scale(PreMed)^2)  + scale(TemMed) +I(scale(TemMed)^2) |ID),data=CrMaize16ts)
summary(Keith7s) 

Keith8s<-lmer(Yield~scale(PreMean) + I(scale(PreMean)^2)+ scale(TemMean) +I(scale(TemMean)^2) + scale(PreMeanCV)+I(scale(PreMeanCV)^2) +
                scale(TemMeanCV) +I(scale(TemMeanCV)^2)  +(scale(PreMean)+I(scale(PreMean)^2) 
                        + scale(TemMean) +I(scale(TemMean)^2) |ID),data=CrMaize16ts)
summary(Keith8s) 

Keith7sw<-lmer(Yield~scale(PreMed) + I(scale(PreMed)^2)+ scale(TemMed) +I(scale(TemMed)^2) + scale(PreMedCV)+I(scale(PreMedCV)^2) +
                scale(TemMedCV) +I(scale(TemMedCV)^2)  +(scale(PreMed)+I(scale(PreMed)^2) 
                                                          + scale(TemMed) +I(scale(TemMed)^2) |ID),data=CrMaize16ts, weights=Area/1000)
summary(Keith7sw) 
# and so on...

Keith8sw<-lmer(Yield~scale(PreMean) + I(scale(PreMean)^2)+ scale(TemMean) +I(scale(TemMean)^2) + scale(PreMeanCV)+I(scale(PreMeanCV)^2) +
                 scale(TemMeanCV) +I(scale(TemMeanCV)^2)  +(scale(PreMean)+I(scale(PreMean)^2) 
                                                             + scale(TemMean) +I(scale(TemMean)^2) |ID),data=CrMaize16ts, weights=Area/1000)
summary(Keith8sw)  # This one fails to converge...