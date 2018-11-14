rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(plm)

load("~/foodSystems/dataFS/Main/VCI_NDMAphase.RData")
VCI2phaseTS<-pdata.frame(VCI2phase,index=c("CountyID","T"))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

VCI2phaseAMJ<-subset(VCI2phase, Month %in% c(4,5,6))
VCI2phaseNDJ<-subset(VCI2phase, Month %in% c(11,12,1))
VCI2phaseNDJAMJ<-subset(VCI2phase, Month %in% c(4,5,6,11,12,1))

VCIts2AMJ<-subset(VCI2phaseTS, Month %in% c(4,5,6))
VCIts2NDJ<-subset(VCI2phaseTS, Month %in% c(11,12,1))
VCIts2NDJAMJ<-subset(VCI2phaseTS, Month %in% c(4,5,6,11,12,1))


#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 1.AMJ

boxplot((VCI2phaseAMJ$VCI_ndma)~PhaseInt,data=VCI2phaseAMJ,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("VCI from NDMA"), main ="April, May, June")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))


boxplot((VCI2phase$VCI_ndma)~PhaseInt,data=VCI2phase,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("VCI from NDMA"), main ="All months")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

# now lags..

boxplot(lag(VCIts2AMJ$VCI_ndma)~PhaseInt,data=VCIts2AMJ,xaxt = "n", xlab='NDMA Early Warning Phase', main ="April, May, June",
        ylab=c("VCI from NDMA- first lag"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))


boxplot(lag(VCI2phaseTS$VCI_ndma)~PhaseInt,data=VCI2phaseTS,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("VCI from NDMA - first lag"), main ="All months")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#2. NDJ

boxplot((VCI2phaseNDJ$VCI_ndma)~PhaseInt,data=VCI2phaseNDJ,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("VCI from NDMA"), main ="November, December, January")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))


boxplot((VCI2phase$VCI_ndma)~PhaseInt,data=VCI2phase,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("VCI from NDMA"), main ="All months")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))


# now lags..

boxplot(lag(VCIts2NDJ$VCI_ndma)~PhaseInt,data=VCIts2NDJ,xaxt = "n", xlab='NDMA Early Warning Phase',main ="November, December, January",
        ylab=c("VCI from NDMA - first lag"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))



boxplot(lag(VCI2phaseTS$VCI_ndma)~PhaseInt,data=VCI2phaseTS,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("VCI from NDMA - first lag"), main ="All months")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# 3. NDJAMJ



boxplot((VCI2phaseNDJAMJ$VCI_ndma)~PhaseInt,data=VCI2phaseNDJAMJ,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("VCI from NDMA"), main ="November, December, January & April, May, June")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))


boxplot((VCI2phase$VCI_ndma)~PhaseInt,data=VCI2phase,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("VCI from NDMA"), main ="All months")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

# now lags..

boxplot(lag(VCIts2NDJAMJ$VCI_ndma)~PhaseInt,data=VCIts2NDJAMJ,xaxt = "n", xlab='NDMA Early Warning Phase',main ="November, December, January & April, May, June",
        ylab=c("VCI from NDMA - first lag"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))



boxplot(lag(VCI2phaseTS$VCI_ndma)~PhaseInt,data=VCI2phaseTS,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("VCI from NDMA - first lag"), main ="All months")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))