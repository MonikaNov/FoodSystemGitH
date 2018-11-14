rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(plm)

load("~/foodSystems/dataFS/Main/VCIphase.RData")
VCIphaseTS<-pdata.frame(VCIphase,index=c("CountyID","T"))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

VCIphaseAMJ<-subset(VCIphase, Month %in% c(4,5,6))
VCIphaseNDJ<-subset(VCIphase, Month %in% c(11,12,1))
VCIphaseNDJAMJ<-subset(VCIphase, Month %in% c(4,5,6,11,12,1))

VCItsAMJ<-subset(VCIphaseTS, Month %in% c(4,5,6))
VCItsNDJ<-subset(VCIphaseTS, Month %in% c(11,12,1))
VCItsNDJAMJ<-subset(VCIphaseTS, Month %in% c(4,5,6,11,12,1))


#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 1.AMJ

boxplot((VCIphaseAMJ$VCImean)~PhaseInt,data=VCIphaseAMJ,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("VCI from the BOKU website"), main ="April, May, June")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))


boxplot((VCIphase$VCImean)~PhaseInt,data=VCIphase,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("VCI from the BOKU website"), main ="All months")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

# now lags..

boxplot(lag(VCItsAMJ$VCImean)~PhaseInt,data=VCItsAMJ,xaxt = "n", xlab='NDMA Early Warning Phase', main ="April, May, June",
        ylab=c("VCI from the BOKU website - first lag"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))


boxplot(lag(VCIphaseTS$VCImean)~PhaseInt,data=VCIphaseTS,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("VCI from the BOKU website - first lag"), main ="All months")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#2. NDJ

boxplot((VCIphaseNDJ$VCImean)~PhaseInt,data=VCIphaseNDJ,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("VCI from the BOKU website"), main ="November, December, January")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))


boxplot((VCIphase$VCImean)~PhaseInt,data=VCIphase,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("VCI from the BOKU website"), main ="All months")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

# now lags..

boxplot(lag(VCItsNDJ$VCImean)~PhaseInt,data=VCItsNDJ,xaxt = "n", xlab='NDMA Early Warning Phase',main ="November, December, January",
        ylab=c("VCI from the BOKU website - first lag"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))


boxplot(lag(VCIphaseTS$VCImean)~PhaseInt,data=VCIphaseTS,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("VCI from the BOKU website - first lag"), main ="All months")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# 3. NDJAMJ



boxplot((VCIphaseNDJAMJ$VCImean)~PhaseInt,data=VCIphaseNDJAMJ,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("VCI from the BOKU website"), main ="November, December, January & April, May, June")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))


boxplot((VCIphase$VCImean)~PhaseInt,data=VCIphase,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("VCI from the BOKU website"), main ="All months")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

# now lags..

boxplot(lag(VCItsNDJAMJ$VCImean)~PhaseInt,data=VCItsNDJAMJ,xaxt = "n", xlab='NDMA Early Warning Phase',main ="November, December, January & April, May, June",
        ylab=c("VCI from the BOKU website - first lag"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))


boxplot(lag(VCIphaseTS$VCImean)~PhaseInt,data=VCIphaseTS,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("VCI from the BOKU website - first lag"), main ="All months")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))
