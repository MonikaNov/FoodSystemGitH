rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(plm)

load("~/foodSystems/dataFS/Main/VCIphase.RData")
VCIphaseTS<-pdata.frame(VCIphase,index=c("CountyID","T"))

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# 1. Boxplots

boxplot(VCImean~PhaseInt,data=VCIphase,xaxt = "n", xlab='NDMA Early Warning Phase',ylab=c("VCI mean"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

boxplot(VCImean~PhaseNum,data=VCIphase,xaxt = "n", xlab='NDMA Early Warning Phase',ylab=c("VCI mean"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

boxplot(VCIp10~PhaseInt,data=VCIphase,xaxt = "n", xlab='NDMA Early Warning Phase',ylab=c("VCI p10"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

boxplot(VCIp30~PhaseInt,data=VCIphase,xaxt = "n", xlab='NDMA Early Warning Phase',ylab=c("VCI p30"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

boxplot(VCIp50~PhaseInt,data=VCIphase,xaxt = "n", xlab='NDMA Early Warning Phase',ylab=c("VCI p50"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

boxplot(VCIp70~PhaseInt,data=VCIphase,xaxt = "n", xlab='NDMA Early Warning Phase',ylab=c("VCI p70"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# other indicators- jen tak pro zajimavost

boxplot(as.numeric(MUAC)~PhaseInt,data=VCIphase,xaxt = "n", xlab='NDMA Early Warning Phase',ylab=c("MUAC"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

boxplot(as.numeric(CSI)~PhaseInt,data=VCIphase,xaxt = "n", xlab='NDMA Early Warning Phase',ylab=c("Coping Strategy Index"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# lags

VCIphaseTS<-pdata.frame(VCIphase,index=c("CountyID","T"))

boxplot(lag(VCIphaseTS$VCImean)~VCIphaseTS$PhaseInt,xaxt = "n", xlab='NDMA Early Warning Phase',ylab=c("VCI mean lag 1"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

boxplot(lag(VCIphaseTS$VCImean,2)~VCIphaseTS$PhaseInt,xaxt = "n", xlab='NDMA Early Warning Phase',ylab=c("VCI mean lag 2"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

boxplot(lag(VCIphaseTS$VCImean,3)~VCIphaseTS$PhaseInt,xaxt = "n", xlab='NDMA Early Warning Phase',ylab=c("VCI mean lag 3"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

boxplot(VCImean~PhaseInt,data=VCIphaseTS,xaxt = "n", xlab='NDMA Early Warning Phase',ylab=c("VCI mean"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

