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
Phase08$SPEI18c<-NA
Phase08$SPEI18c[Phase08$SPEI18<= -0.5  &Phase08$SPEI18> -1.5  ]<-1
Phase08$SPEI18c[Phase08$SPEI18<= 0.5  &Phase08$SPEI18> -0.5  ]<-2
Phase08$SPEI18c[Phase08$SPEI18<= 1.5  &Phase08$SPEI18> 0.5  ]<-3
Phase08$SPEI18c[Phase08$SPEI18> 1.5  ]<-4

table(Phase08$SPEI18c,Phase08$SPEI3c)
plot(Phase08$SPEI18c,Phase08$SPEI3c)
boxplot(Phase08$SPEI18~Phase08$SPEI18c)
table(Phase08$PhaseInt,Phase08$SPEI18c)
prop.table(table(Phase08$PhaseInt,Phase08$SPEI18c),2)
frequencies7<-prop.table(table(Phase08$PhaseInt,Phase08$SPEI18c),2)


plot(frequencies7[,1],type='l',col=2,lwd=1.5,xaxt = "n", main='SPEI 18',xlab='NDMA Early Warning Phase',
     ylab=c("Probabpility / Frequencies"))
lines(frequencies7[,2],type='l',col='orange',lwd=1.5)
lines(frequencies7[,3],type='l',col=4,lwd=1.5)
lines(frequencies7[,4],type='l',col=3,lwd=1.5)
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))
legend(2.2, y = 0.78,
       legend=c('SPEI \u2264 -0.5',' -0.5 < SPEI \u2264 0.5',' 0.5  < SPEI \u2264 1.5',' 1.5 < SPEI'),
       bty = "n",lwd=2, cex=1, col=c(2,"orange",4,3), lty=c(1,1))

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# now trying a bit different categories

Phase08$SPEI18c4<-NA
Phase08$SPEI18c4[Phase08$SPEI18<= -1]<-1
Phase08$SPEI18c4[Phase08$SPEI18<= 0  &Phase08$SPEI18> -1 ]<-2
Phase08$SPEI18c4[Phase08$SPEI18<= 1  &Phase08$SPEI18> 0  ]<-3
Phase08$SPEI18c4[Phase08$SPEI18> 1  ]<-4

table(Phase08$SPEI18c4)
boxplot(Phase08$SPEI18~Phase08$SPEI18c4)
table(Phase08$PhaseInt,Phase08$SPEI18c4)
prop.table(table(Phase08$PhaseInt,Phase08$SPEI18c4),2)
frequencies8<-prop.table(table(Phase08$PhaseInt,Phase08$SPEI18c4),2)

plot(frequencies8[,1],type='l',lwd=1.5,col=2,xaxt = "n",main='SPEI 18', xlab='NDMA Early Warning Phase',
     ylab=c("Probabpility / Frequencies"))
lines(frequencies8[,2],type='l',col='orange',lwd=1.5)
lines(frequencies8[,3],type='l',col=4,lwd=1.5)
lines(frequencies8[,4],type='l',col=3,lwd=1.5)
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))
legend(2.31, y = 0.76,
       legend=c('SPEI \u2264 -1',' -1 < SPEI \u2264 0',' 0 < SPEI \u2264 1',' 1 < SPEI'),
       bty = "n",lwd=2, cex=1, col=c(2,"orange",4,3), lty=c(1,1))

