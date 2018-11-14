rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(plm)
library(pglm)
load("~/foodSystems/dataFS/Phase06.RData")
Phase07<-pdata.frame(Phase06,index=c("CountyID","T"))
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# raw temperature

boxplot((Phase07$Tmx)~PhaseInt,data=Phase07,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("Maximum temperature"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))


boxplot(lag(Phase07$Tmx)~PhaseInt,data=Phase07,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("Lagged max. temperature (previous month)"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

boxplot(lag(Phase07$Tmx,2)~PhaseInt,data=Phase07,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("Lagged max. temperature (2 months before)"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

boxplot(lag(Phase07$Tmx,3)~PhaseInt,data=Phase07,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("Lagged max. temperature (3 months before)"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

boxplot(lag(Phase07$Tmx,6)~PhaseInt,data=Phase07,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("Lagged max. temperature (6 months before)"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))


boxplot(lag(Phase07$Tmx,8)~PhaseInt,data=Phase07,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("Lagged max. temperature (8 months before)"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))



boxplot(lag(Phase07$Tmx,10)~PhaseInt,data=Phase07,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("Lagged max. temperature (10 months before)"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))


boxplot(lag(Phase07$Tmx,12)~PhaseInt,data=Phase07,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("Lagged max. temperature (12 months before)"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Z-score temperature

boxplot((Phase07$Tmxz)~PhaseInt,data=Phase07,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("Maximum temperature - Z-score"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))


boxplot(lag(Phase07$Tmxz)~PhaseInt,data=Phase07,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("Lagged max. temperature (previous month) - Z-score"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

boxplot(lag(Phase07$Tmxz,2)~PhaseInt,data=Phase07,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("Lagged max. temperature (2 months before) - Z-score"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

boxplot(lag(Phase07$Tmxz,3)~PhaseInt,data=Phase07,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("Lagged max. temperature (3 months before) - Z-score"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))


boxplot(lag(Phase07$Tmxz,6)~PhaseInt,data=Phase07,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("Lagged max. temperature (6 months before) - Z-score"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

boxplot(lag(Phase07$Tmxz,8)~PhaseInt,data=Phase07,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("Lagged max. temperature (8 months before) - Z-score"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))


boxplot(lag(Phase07$Tmxz,10)~PhaseInt,data=Phase07,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("Lagged max. temperature (10 months before) - Z-score"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))


boxplot(lag(Phase07$Tmxz,12)~PhaseInt,data=Phase07,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("Lagged max. temperature (12 months before) - Z-score"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))




