rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(plm)
library(pglm)
load("Main/Phase10zones.RData")
Phase11<-pdata.frame(Phase10,index=c("CountyID","T"))
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Phase11Arid<-subset(Phase11, Arid==1)
Phase11SemiArid<-subset(Phase11, SemiArid==1)
#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

# SPEI 3

boxplot((Phase11Arid$SPEI3)~PhaseInt,data=Phase11Arid,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("SPEI 3"),main ="Arid counties")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

boxplot((Phase11SemiArid$SPEI3)~PhaseInt,data=Phase11SemiArid,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("SPEI 3"),main ="Semiarid counties")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

boxplot((Phase11$SPEI3)~PhaseInt,data=Phase11,xaxt = "n", xlab='NDMA Early Warning Phase',
      ylab=c("SPEI 3"),main ="All counties")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# lag

boxplot(lag(Phase11Arid$SPEI3)~PhaseInt,data=Phase11Arid,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("SPEI 3 - first lag"),main ="Arid counties")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

boxplot(lag(Phase11SemiArid$SPEI3)~PhaseInt,data=Phase11SemiArid,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("SPEI 3 - first lag"),main ="Semiarid counties")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

boxplot(lag(Phase11$SPEI3)~PhaseInt,data=Phase11,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("SPEI 3 - first lag"),main ="All counties")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))
#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
# SPEI 10

boxplot((Phase11Arid$SPEI10)~PhaseInt,data=Phase11Arid,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("SPEI 10"),main ="Arid counties")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

boxplot((Phase11SemiArid$SPEI10)~PhaseInt,data=Phase11SemiArid,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("SPEI 10"),main ="Semiarid counties")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

boxplot((Phase11$SPEI10)~PhaseInt,data=Phase11,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("SPEI 10"),main ="All counties")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# lag

boxplot(lag(Phase11Arid$SPEI10)~PhaseInt,data=Phase11Arid,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("SPEI 10 - first lag"),main ="Arid counties")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

boxplot(lag(Phase11SemiArid$SPEI10)~PhaseInt,data=Phase11SemiArid,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("SPEI 10 - first lag"),main ="Semiarid counties")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

boxplot(lag(Phase11$SPEI10)~PhaseInt,data=Phase11,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("SPEI 10 - first lag"),main ="All counties")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))


#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
# SPEI 12

boxplot((Phase11Arid$SPEI12)~PhaseInt,data=Phase11Arid,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("SPEI 12"),main ="Arid counties")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

boxplot((Phase11SemiArid$SPEI12)~PhaseInt,data=Phase11SemiArid,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("SPEI 12"),main ="Semiarid counties")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

boxplot((Phase11$SPEI12)~PhaseInt,data=Phase11,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("SPEI 12"),main ="All counties")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# lag

#  ARID   ARID       ARID
boxplot(lag(Phase11Arid$SPEI12)~PhaseInt,data=Phase11Arid,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("SPEI 12"),main ="Arid counties")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))


#  SAMIARID   SAMIARID       SAMIARID
boxplot(lag(Phase11SemiArid$SPEI12)~PhaseInt,data=Phase11SemiArid,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("SPEI 12"),main ="Semiarid counties")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))


#  ALL   ALL       ALL
boxplot(lag(Phase11$SPEI12)~PhaseInt,data=Phase11,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("SPEI 12"),main ="All counties")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))



#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
# SPEI 18

boxplot((Phase11Arid$SPEI18)~PhaseInt,data=Phase11Arid,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("SPEI 18"),main ="Arid counties")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

boxplot((Phase11SemiArid$SPEI18)~PhaseInt,data=Phase11SemiArid,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("SPEI 18"),main ="Semiarid counties")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

boxplot((Phase11$SPEI18)~PhaseInt,data=Phase11,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("SPEI 18"),main ="All counties")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# lag

#  ARID   ARID       ARID
boxplot(lag(Phase11Arid$SPEI18)~PhaseInt,data=Phase11Arid,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("SPEI 18"),main ="Arid counties")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))


#  SAMIARID   SAMIARID       SAMIARID
boxplot(lag(Phase11SemiArid$SPEI18)~PhaseInt,data=Phase11SemiArid,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("SPEI 18"),main ="Semiarid counties")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))

#  ALL   ALL       ALL
boxplot(lag(Phase11$SPEI18)~PhaseInt,data=Phase11,xaxt = "n", xlab='NDMA Early Warning Phase',
        ylab=c("SPEI 18"),main ="All counties")
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))