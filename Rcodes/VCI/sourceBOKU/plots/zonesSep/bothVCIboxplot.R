rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(plm)
library(pglm)
load("Main/VCI_NDMAphase.RData")
rm(zoneID2)
zoneID2<-read.csv( "~/foodSystems/Rcodes/Climate/plots/ZonesSep/zoneID2.csv",header=TRUE)
rm(VCI_NDMA)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
rm(VCI3phase)
VCI3phase<-merge(VCI2phase, zoneID2[c(1,3,4)], by="CountyID",all.x=TRUE)
VCI3ts<-pdata.frame(VCI3phase,index=c("CountyID","T"))

#            GROOT

# subsets

VCI3arid<-subset(VCI3ts, Arid==1)
VCI3semiarid<-subset(VCI3ts, SemiArid==1)
#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
# 
#1. VCI NDMA

boxplot((VCI3arid$VCI_ndma)~PhaseInt,data=VCI3arid,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("VCI from NDMA"), main ="Arid counties")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

boxplot((VCI3semiarid$VCI_ndma)~PhaseInt,data=VCI3semiarid,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("VCI from NDMA"), main ="Semiarid counties")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))


boxplot((VCI3ts$VCI_ndma)~PhaseInt,data=VCI3ts,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("VCI from NDMA"), main ="All counties")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

#----------------------------------------------------------------------------------------------------------

# lag 
boxplot(lag(VCI3arid$VCI_ndma)~PhaseInt,data=VCI3arid,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("VCI from NDMA - first lag"), main ="Arid counties")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

boxplot(lag(VCI3semiarid$VCI_ndma)~PhaseInt,data=VCI3semiarid,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("VCI from NDMA - first lag"), main ="Semiarid counties")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

boxplot(lag(VCI3ts$VCI_ndma)~PhaseInt,data=VCI3ts,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("VCI from NDMA - first lag"), main ="All counties")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
# 
#1. VCI from BOKU

boxplot((VCI3arid$VCImean)~PhaseInt,data=VCI3arid,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("VCI from BOKU"), main ="Arid counties")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

boxplot((VCI3semiarid$VCImean)~PhaseInt,data=VCI3semiarid,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("VCI from BOKU"), main ="Semiarid counties")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))


boxplot((VCI3ts$VCImean)~PhaseInt,data=VCI3ts,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("VCI from BOKU"), main ="All counties")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

#----------------------------------------------------------------------------------------------------------

# lag 
boxplot(lag(VCI3arid$VCImean)~PhaseInt,data=VCI3arid,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("VCI from BOKU - first lag"), main ="Arid counties")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

boxplot(lag(VCI3semiarid$VCImean)~PhaseInt,data=VCI3semiarid,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("VCI from BOKU - first lag"), main ="Semiarid counties")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

boxplot(lag(VCI3ts$VCImean)~PhaseInt,data=VCI3ts,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("VCI from BOKU - first lag"), main ="All counties")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))