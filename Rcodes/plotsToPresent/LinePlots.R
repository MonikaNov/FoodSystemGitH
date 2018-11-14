rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(plm)
load("Main/VCIphaseClim.RData")

# time series Kitui, Isiolo

VCI3<-pdata.frame(VCIphaseClim,index=c("CountyID","T"))
IsioloVCI<-subset(VCI3, County.x %in% c("Isiolo"))
KituiVCI<-subset(VCI3, County.x %in% c("Kitui"))

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plot(IsioloVCI$VCImean, type='l',ylim=c(-5,45),xaxt = "n",ylab=c("VCI mean, SPEI 3"),xlab=NA)
lines(IsioloVCI$SPEI3, type='l',col=3)
lines(10*IsioloVCI$PhaseInt, type='l',col=4)

atx<-c(8,20,32,44)
# axis(1, at=atx, labels=VCIphaseClim$Year[atx])
axis(1, at=atx, labels=c('Jan 2014','Jan 2015','Jan 2016', 'Jan 2017'))

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# scaling SPEI

plot(IsioloVCI$VCImean, type='l',ylim=c(0,45),xaxt = "n",ylab=c("VCI mean, SPEI 3"),xlab=NA)
lines(10*IsioloVCI$SPEI3+30, type='l',col=3)
lines(10*IsioloVCI$PhaseInt, type='l',col=4)

atx<-c(8,20,32,44)
# axis(1, at=atx, labels=VCIphaseClim$Year[atx])
axis(1, at=atx, labels=c('Jan 2014','Jan 2015','Jan 2016', 'Jan 2017'))
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Just overlapping period
scope<-16:32

plot(IsioloVCI$VCImean[scope], type='l',ylim=c(-5,45),xaxt = "n",ylab=c("VCI mean, SPEI 3"),xlab=NA)
lines(IsioloVCI$SPEI3[scope], type='l',col=3)
lines(10*IsioloVCI$PhaseInt[scope], type='l',col=4)

atx<-c(5,11,17)
# axis(1, at=atx, labels=VCIphaseClim$Year[atx])
axis(1, at=atx, labels=c('Jan 2015','Jul 2015', 'Jan 2016'))


#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Kitui

plot(KituiVCI$VCImean, type='l',ylim=c(-5,60),xaxt = "n",ylab=c("VCI mean, SPEI 3"),xlab=NA)
lines(KituiVCI$SPEI3, type='l',col=3)
lines(10*KituiVCI$PhaseInt, type='l',col=4)

atx<-c(8,20,32,44)
# axis(1, at=atx, labels=VCIphaseClim$Year[atx])
axis(1, at=atx, labels=c('Jan 2014','Jan 2015','Jan 2016', 'Jan 2017'))

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Scaling SPEI 

plot(KituiVCI$VCImean, type='l',ylim=c(-30,60),xaxt = "n",ylab=c("VCI mean, SPEI 3"),xlab=NA)
lines(10*KituiVCI$SPEI3, type='l',col=3)
lines(10*KituiVCI$PhaseInt, type='l',col=4)

atx<-c(8,20,32,44)
# axis(1, at=atx, labels=VCIphaseClim$Year[atx])
axis(1, at=atx, labels=c('Jan 2014','Jan 2015','Jan 2016', 'Jan 2017'))