rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(plm)
library(pglm)
load("~/foodSystems/dataFS/Phase06.RData")
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# histogram

barplot(table(Phase06$PhaseOrd),xaxt = "n", main='NDMA Early Warning Phase',xlab=" ")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# boxplots



boxplot(SPEI3~PhaseInt,data=Phase06,xaxt = "n", xlab='NDMA Early Warning Phase',ylab=c("SPEI 3"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

boxplot(SPEI10~PhaseInt,data=Phase06,xaxt = "n", xlab='NDMA Early Warning Phase',ylab=c("SPEI 10"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

boxplot(SPEI12~PhaseInt,data=Phase06,xaxt = "n", xlab='NDMA Early Warning Phase',ylab=c("SPEI 12"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

boxplot(SPEI18~PhaseInt,data=Phase06,xaxt = "n", xlab='NDMA Early Warning Phase',ylab=c("SPEI 18"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

boxplot(Prec~PhaseInt,data=Phase06,xaxt = "n", xlab='NDMA Early Warning Phase',ylab=c("Precipitation (10th percentile)"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

boxplot(PrecZ~PhaseInt,data=Phase06,xaxt = "n", xlab='NDMA Early Warning Phase',ylab=c("Precipitation (10th percentile) - Z-score"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

boxplot(Tmx~PhaseInt,data=Phase06,xaxt = "n", xlab='NDMA Early Warning Phase',ylab=c("Maximum temperature (90th percentile)"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

boxplot(Tmxz~PhaseInt,data=Phase06, xaxt = "n",xlab='NDMA Early Warning Phase',ylab=c("Maximum temperature (90th percentile) - Z-score"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# lags


Phase07<-pdata.frame(Phase06,index=c("CountyID","T"))

#SPEI

boxplot(lag(Phase07$SPEI3)~PhaseInt,data=Phase07,xaxt = "n", xlab='NDMA Early Warning Phase',ylab=c("Lagged SPEI 3 (previous month)"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

boxplot(lag(Phase07$SPEI10)~PhaseInt,data=Phase07,xaxt = "n", xlab='NDMA Early Warning Phase',ylab=c("Lagged SPEI 10 (previous month)"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

boxplot(lag(Phase07$SPEI12)~PhaseInt,data=Phase07,xaxt = "n", xlab='NDMA Early Warning Phase',ylab=c("Lagged SPEI 12 (previous month)"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

boxplot(lag(Phase07$SPEI18)~PhaseInt,data=Phase07,xaxt = "n", xlab='NDMA Early Warning Phase',ylab=c("Lagged SPEI 18 (previous month)"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))


#--------------------------------------------------------------------------------------------------------------------------------------------------------
# Precipitations

#  boxplot((Phase07$Prec)~PhaseInt,data=Phase07,xaxt = "n", xlab='NDMA Early Warning Phase',ylab=c("Lagged precipitation (previous month)"))
#  axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

boxplot(lag(Phase07$Prec)~PhaseInt,data=Phase07,xaxt = "n", xlab='NDMA Early Warning Phase',ylab=c("Lagged precipitation (previous month)"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

boxplot(lag(Phase07$Prec,2)~PhaseInt,data=Phase07,xaxt = "n", 
        xlab='NDMA Early Warning Phase',ylab=c("Lagged precipitation (2 months before)"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))


boxplot(lag(Phase07$Prec,3)~PhaseInt,data=Phase07,xaxt = "n", xlab='NDMA Early Warning Phase',ylab=c("Lagged precipitation (3 months before)"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

boxplot(lag(Phase07$Prec,6)~PhaseInt,data=Phase07,xaxt = "n", xlab='NDMA Early Warning Phase',ylab=c("Lagged precipitation (6 months before)"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

boxplot(lag(Phase07$Prec,8)~PhaseInt,data=Phase07,xaxt = "n", xlab='NDMA Early Warning Phase',ylab=c("Lagged precipitation (8 months before)"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

boxplot(lag(Phase07$Prec,10)~PhaseInt,data=Phase07,xaxt = "n", xlab='NDMA Early Warning Phase',ylab=c("Lagged precipitation (10 months before)"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

boxplot(lag(Phase07$Prec,12)~PhaseInt,data=Phase07,xaxt = "n", xlab='NDMA Early Warning Phase',ylab=c("Lagged precipitation (12 months before)"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

#--------------------------------------------------------------------------------------------------------------------------------------------------------
# Precipitations Z-score


boxplot((Phase07$PrecZ)~PhaseInt,data=Phase07,xaxt = "n", xlab='NDMA Early Warning Phase',ylab=c("Precipitation- Z-score"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))


boxplot(lag(Phase07$PrecZ)~PhaseInt,data=Phase07,xaxt = "n", xlab='NDMA Early Warning Phase',ylab=c("Lagged precipitation (previous month) - Z-score"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))



boxplot(lag(Phase07$PrecZ,2)~PhaseInt,data=Phase07,xaxt = "n", xlab='NDMA Early Warning Phase',ylab=c("Lagged precipitation (2 months before) - Z-score"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))


boxplot(lag(Phase07$PrecZ,3)~PhaseInt,data=Phase07,xaxt = "n", xlab='NDMA Early Warning Phase',ylab=c("Lagged precipitation (3 months before) - Z-score"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

boxplot(lag(Phase07$PrecZ,6)~PhaseInt,data=Phase07,xaxt = "n", xlab='NDMA Early Warning Phase',ylab=c("Lagged precipitation (6 months before) - Z-score"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

boxplot(lag(Phase07$PrecZ,8)~PhaseInt,data=Phase07,xaxt = "n", xlab='NDMA Early Warning Phase',ylab=c("Lagged precipitation (8 months before) - Z-score"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

boxplot(lag(Phase07$PrecZ,10)~PhaseInt,data=Phase07,xaxt = "n", xlab='NDMA Early Warning Phase',ylab=c("Lagged precipitation (10 months before) - Z-score"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

boxplot(lag(Phase07$PrecZ,12)~PhaseInt,data=Phase07,xaxt = "n", xlab='NDMA Early Warning Phase',ylab=c("Lagged precipitation (12 months before) - Z-score"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))





