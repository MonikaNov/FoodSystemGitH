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
table(Phase06$PhaseOrd,Phase06$Month)

hist(Phase06$PhaseOrd)
table(Phase06$PhaseOrd)

barplot(table(Phase06$PhaseOrd,Phase06$Month))
barplot(table(Phase06$PhaseOrd,Phase06$Month), beside=TRUE)

barplot(table(Phase06$PhaseOrd,Phase06$Month)[2,])

barplot(table(Phase06$PhaseOrd,Phase06$Month)[3,])

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


barplot(table(Phase06$PhaseOrd,Phase06$Month)[2:3,], beside=TRUE,ylab="Frequency",xlab="Month"
        ,col=c(4,2),border=NA)

legend(1, y = 38,
       legend=c('Alert','Alarm'),fill=c(4,2),border=NA,
       bty = "n",lwd=NA, cex=1, col=c(2,"orange",4,3), lty=NA)




#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

barplot(table((Phase06$PhaseOrd),lag(Phase07$PhaseOrd))[2,],xaxt = "n",ylab="Frequency"
        ,main='Previous phase, given that current phase is Alert (frequencies)')
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))


barplot(table((Phase06$PhaseOrd),lag(Phase07$PhaseOrd))[3,],xaxt = "n",ylab="Frequency"
        ,main='Previous phase, given that current phase is Alarm (frequencies)')
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))



barplot(table((Phase06$PhaseOrd),lag(Phase07$PhaseOrd))[1,],xaxt = "n",ylab="Frequency"
        ,main='Previous phase, given that current phase is Normal (frequencies)')
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))