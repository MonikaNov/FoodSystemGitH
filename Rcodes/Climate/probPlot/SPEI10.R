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
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Phase08<-Phase06
Phase08$SPEI10c<-NA
Phase08$SPEI10c[Phase08$SPEI10<= -0.5  &Phase08$SPEI10> -1.5  ]<-1
Phase08$SPEI10c[Phase08$SPEI10<= 0.5  &Phase08$SPEI10> -0.5  ]<-2
Phase08$SPEI10c[Phase08$SPEI10<= 1.5  &Phase08$SPEI10> 0.5  ]<-3
Phase08$SPEI10c[Phase08$SPEI10> 1.5  ]<-4

table(Phase08$SPEI10c,Phase08$SPEI3c)
plot(Phase08$SPEI10c,Phase08$SPEI3c)
boxplot(Phase08$SPEI10~Phase08$SPEI10c)
table(Phase08$PhaseInt,Phase08$SPEI10c)
prop.table(table(Phase08$PhaseInt,Phase08$SPEI10c),2)
frequencies3<-prop.table(table(Phase08$PhaseInt,Phase08$SPEI10c),2)


plot(frequencies3[,1],type='l',col=2,lwd=1.5,xaxt = "n", main='SPEI 10',xlab='NDMA Early Warning Phase',
     ylab=c("Probabpility / Frequencies"))
lines(frequencies3[,2],type='l',col='orange',lwd=1.5)
lines(frequencies3[,3],type='l',col=4,lwd=1.5)
lines(frequencies3[,4],type='l',col=3,lwd=1.5)
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))
legend(2.2, y = 0.775,
       legend=c('SPEI \u2264 -0.5',' -0.5 < SPEI \u2264 0.5',' 0.5  < SPEI \u2264 1.5',' 1.5 < SPEI'),
       bty = "n",lwd=2, cex=1, col=c(2,"orange",4,3), lty=c(1,1))

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# now trying a bit different categories

Phase08$SPEI10c4<-NA
Phase08$SPEI10c4[Phase08$SPEI10<= -1]<-1
Phase08$SPEI10c4[Phase08$SPEI10<= 0  &Phase08$SPEI10> -1 ]<-2
Phase08$SPEI10c4[Phase08$SPEI10<= 1  &Phase08$SPEI10> 0  ]<-3
Phase08$SPEI10c4[Phase08$SPEI10> 1  ]<-4

table(Phase08$SPEI10c4)
boxplot(Phase08$SPEI10~Phase08$SPEI10c4)
table(Phase08$PhaseInt,Phase08$SPEI10c4)
prop.table(table(Phase08$PhaseInt,Phase08$SPEI10c4),2)
frequencies4<-prop.table(table(Phase08$PhaseInt,Phase08$SPEI10c4),2)

plot(frequencies4[,1],type='l',lwd=1.5,col=2,xaxt = "n",main='SPEI 10', xlab='NDMA Early Warning Phase',
     ylab=c("Probabpility / Frequencies"))
lines(frequencies4[,2],type='l',col='orange',lwd=1.5)
lines(frequencies4[,3],type='l',col=4,lwd=1.5)
lines(frequencies4[,4],type='l',col=3,lwd=1.5)
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))
legend(2.31, y = 0.92,
       legend=c('SPEI \u2264 -1',' -1 < SPEI \u2264 0',' 0 < SPEI \u2264 1',' 1 < SPEI'),
       bty = "n",lwd=2, cex=1, col=c(2,"orange",4,3), lty=c(1,1))

