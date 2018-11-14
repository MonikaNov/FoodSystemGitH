
rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo


load("Main/CrMaize16.RData")
CrMaize16ts<-pdata.frame(CrMaize16,index=c("ID","Year"))



#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo


# ------------              medians     ----------------------------------------        ----------------              -



basic1<-lm(Yield~PreMed + I(PreMed^2)+ TemMed +I(TemMed^2) + PreMedCV+I(PreMedCV^2) +
            TemMedCV,data=CrMaize16ts)
summary(basic1) 


basic2<-lm(log(Yield)~PreMed + I(PreMed^2)+ TemMed +I(TemMed^2) + PreMedCV+I(PreMedCV^2) +
            TemMedCV,data=CrMaize16ts)
summary(basic2) 

basic3<-lm(Yield~PreMedZ + I(PreMedZ^2)+ TemMedZ +I(TemMedZ^2) + PreMedCVz+I(PreMedCVz^2) +
            TemMedCVz,data=CrMaize16ts)
summary(basic3) 

basic4<-lm(log(Yield)~PreMedZ + I(PreMedZ^2)+ TemMedZ +I(TemMedZ^2) + PreMedCVz+I(PreMedCVz^2) +
            TemMedCVz,data=CrMaize16ts)
summary(basic4) 

# ------------              means     ----------------------------------------        ----------------              -

basic1A<-lm(Yield~PreMean + I(PreMean^2)+ TemMean +I(TemMean^2) + PreMeanCV+I(PreMeanCV^2) +
            TemMeanCV,data=CrMaize16ts)
summary(basic1A) 


basic2A<-lm(log(Yield)~PreMean + I(PreMean^2)+ TemMean +I(TemMean^2) + PreMeanCV+I(PreMeanCV^2) +
            TemMeanCV,data=CrMaize16ts)
summary(basic2A) 

basic3A<-lm(Yield~PreMeanZ + I(PreMeanZ^2)+ TemMeanZ +I(TemMeanZ^2) + PreMeanCVz+I(PreMeanCVz^2) +
            TemMeanCVz,data=CrMaize16ts)
summary(basic3A) 

basic4A<-lm(log(Yield)~PreMeanZ + I(PreMeanZ^2)+ TemMeanZ +I(TemMeanZ^2) + PreMeanCVz+I(PreMeanCVz^2) +
            TemMeanCVz,data=CrMaize16ts)
summary(basic4A) 

