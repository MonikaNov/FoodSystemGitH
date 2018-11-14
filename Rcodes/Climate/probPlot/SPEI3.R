rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)

load("Main/Phase06.RData")

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(plm)
library(pglm)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
hist(Phase06$SPEI3)
summary(Phase06$SPEI3)

summary(Phase06$SPEI)

summary(Phase06$SPEI10)
summary(Phase06$SPEI12)
summary(Phase06$SPEI18)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Phase08<-Phase06
Phase08$SPEI3c<-NA
Phase08$SPEI3c[Phase08$SPEI3<= -1.5]<-1
Phase08$SPEI3c[Phase08$SPEI3<= -0.5  &Phase08$SPEI3> -1.5  ]<-2
Phase08$SPEI3c[Phase08$SPEI3<= 0.5  &Phase08$SPEI3> -0.5  ]<-3
Phase08$SPEI3c[Phase08$SPEI3<= 1.5  &Phase08$SPEI3> 0.5  ]<-4
Phase08$SPEI3c[Phase08$SPEI3> 1.5  ]<-5

table(Phase08$SPEI3c)
boxplot(Phase08$SPEI3~Phase08$SPEI3c)
table(Phase08$PhaseInt,Phase08$SPEI3c)
prop.table(table(Phase08$PhaseInt,Phase08$SPEI3c),2)
frequencies1<-prop.table(table(Phase08$PhaseInt,Phase08$SPEI3c),2)


plot(frequencies1[,5],type='l',col=3,lwd=1.5,xaxt = "n",main='SPEI 3', xlab='NDMA Early Warning Phase',
     ylab=c("Probabpility / Frequencies"))
lines(frequencies1[,1],type='l',col=2,lwd=1.5)
lines(frequencies1[,2],type='l',col='orange',lwd=1.5)
lines(frequencies1[,3],type='l',col=1,lwd=1.5)
lines(frequencies1[,4],type='l',col=4,lwd=1.5)
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))
legend(2.2, y = 0.75,
       legend=c('SPEI \u2264 -1.5',' -1.5 < SPEI \u2264 -0.5',' -0.5 < SPEI \u2264 0.5',' 0.5  < SPEI \u2264 1.5',' 1.5 < SPEI'),
       bty = "n",lwd=2, cex=1, col=c(2,"orange",1,4,3), lty=c(1,1))

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# now trying a bit different categories

Phase08$SPEI3c4<-NA
Phase08$SPEI3c4[Phase08$SPEI3<= -1]<-1
Phase08$SPEI3c4[Phase08$SPEI3<= 0  &Phase08$SPEI3> -1 ]<-2
Phase08$SPEI3c4[Phase08$SPEI3<= 1  &Phase08$SPEI3> 0  ]<-3
Phase08$SPEI3c4[Phase08$SPEI3> 1  ]<-4

table(Phase08$SPEI3c4)
boxplot(Phase08$SPEI3~Phase08$SPEI3c4)
table(Phase08$PhaseInt,Phase08$SPEI3c4)
prop.table(table(Phase08$PhaseInt,Phase08$SPEI3c4),2)
frequencies2<-prop.table(table(Phase08$PhaseInt,Phase08$SPEI3c4),2)

plot(frequencies2[,2],type='l',lwd=1.5,col='orange',xaxt = "n",main='SPEI 3', xlab='NDMA Early Warning Phase',
     ylab=c("Probabpility / Frequencies"))
lines(frequencies2[,1],type='l',col=2,lwd=1.5)
lines(frequencies2[,3],type='l',col=4,lwd=1.5)
lines(frequencies2[,4],type='l',col=3,lwd=1.5)
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))
legend(2.31, y = 0.75,
       legend=c('SPEI \u2264 -1',' -1 < SPEI \u2264 0',' 0 < SPEI \u2264 1',' 1 < SPEI'),
       bty = "n",lwd=2, cex=1, col=c(2,"orange",4,3), lty=c(1,1))

