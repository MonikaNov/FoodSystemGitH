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

Phase06AMJ<-subset(Phase06, Month %in% c(4,5,6))
Phase06NDJ<-subset(Phase06, Month %in% c(11,12,1))
Phase06NDJAMJ<-subset(Phase06, Month %in% c(4,5,6,11,12,1))

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

hist(Phase06NDJ$SPEI3,100)
boxplot(Phase06NDJ$SPEI3)

boxplot((Phase06NDJ$SPEI3)~PhaseInt,data=Phase06NDJ,xaxt = "n", xlab='NDMA Early Warning Phase',
        main ="November, December, January",       ylab=c("SPEI 3"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))


boxplot((Phase06$SPEI3)~PhaseInt,data=Phase06,xaxt = "n", xlab='NDMA Early Warning Phase',
        main ="All months",       ylab=c("SPEI 3"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))


# COOL :-)
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
boxplot(Phase06NDJ$SPEI10)
hist(Phase06NDJ$SPEI10,50)


boxplot((Phase06NDJ$SPEI10)~PhaseInt,data=Phase06NDJ,xaxt = "n", xlab='NDMA Early Warning Phase',
        main ="November, December, January",      ylab=c("SPEI 10"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))


boxplot((Phase06$SPEI10)~PhaseInt,data=Phase06,xaxt = "n", xlab='NDMA Early Warning Phase',
   main ="All months",     ylab=c("SPEI 10"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

# COOL :-)
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
boxplot(Phase06NDJ$SPEI12)
hist(Phase06NDJ$SPEI12,50)


boxplot((Phase06NDJ$SPEI12)~PhaseInt,data=Phase06NDJ,xaxt = "n", xlab='NDMA Early Warning Phase',
        main ="November, December, January",    ylab=c("SPEI 12"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))


boxplot((Phase06$SPEI12)~PhaseInt,data=Phase06,xaxt = "n", xlab='NDMA Early Warning Phase',
        main ="All months",    ylab=c("SPEI 12"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))




#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


boxplot((Phase06NDJ$SPEI18)~PhaseInt,data=Phase06NDJ,xaxt = "n", xlab='NDMA Early Warning Phase',
        main ="November, December, January",   ylab=c("SPEI 18"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))


boxplot((Phase06$SPEI18)~PhaseInt,data=Phase06,xaxt = "n", xlab='NDMA Early Warning Phase',
        main ="All months",        ylab=c("SPEI 18"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))