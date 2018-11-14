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

Phase06Jan<-subset(Phase06, Month == 1)
Phase06Jun<-subset(Phase06, Month ==6)
Phase06JanJun<-subset(Phase06, Month %in% c(1,6))

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# SPEI 3

boxplot((Phase06Jan$SPEI3)~PhaseInt,data=Phase06Jan,xaxt = "n", xlab='NDMA Early Warning Phase',
      ylab=c("SPEI 3"),main ="January")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

boxplot((Phase06Jun$SPEI3)~PhaseInt,data=Phase06Jun,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("SPEI 3"),main ="June")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

boxplot((Phase06JanJun$SPEI3)~PhaseInt,data=Phase06JanJun,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("SPEI 3"),main ="January & June")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

boxplot((Phase06$SPEI3)~PhaseInt,data=Phase06,xaxt = "n", xlab='NDMA Early Warning Phase',
        main ="All months",   ylab=c("SPEI 3"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))


# COOL :-)
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# SPEI 10

boxplot((Phase06Jan$SPEI10)~PhaseInt,data=Phase06Jan,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("SPEI 10"),main ="January")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

boxplot((Phase06Jun$SPEI10)~PhaseInt,data=Phase06Jun,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("SPEI 10"),main ="June")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

boxplot((Phase06JanJun$SPEI10)~PhaseInt,data=Phase06JanJun,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("SPEI 10"),main ="January & June")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

boxplot((Phase06$SPEI10)~PhaseInt,data=Phase06,xaxt = "n", xlab='NDMA Early Warning Phase',
        main ="All months",   ylab=c("SPEI 10"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

