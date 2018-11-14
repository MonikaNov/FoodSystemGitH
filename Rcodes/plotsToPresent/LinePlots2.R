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
plot(IsioloVCI$VCImean, type='l',ylim=c(-5,45),xaxt = "n",ylab=c("VCI mean, SPEI 3"),main="Isiolo",xlab=NA)
lines(IsioloVCI$SPEI3, type='l',col=3)
lines(10*IsioloVCI$PhaseInt, type='l',col=4)

atx<-c(8,20,32,44)
# axis(1, at=atx, labels=VCIphaseClim$Year[atx])
axis(1, at=atx, labels=c('Jan 2014','Jan 2015','Jan 2016', 'Jan 2017'))
legend(x=-4, y = 13, legend=c('Normal'),fill=NA,border=NA,lty=1,text.col = 4, bty = "n",lwd=3)
legend(x=-4, y = 23, legend=c('Alert'),fill=NA,border=NA,text.col = 4,bty = "n",lwd=3,  lty=1)
legend(x=-4, y = 33, legend=c('Alarm'),fill=NA,border=NA,text.col = 4, bty = "n",lwd=3, lty=1)

legend(x=36,y= 8,legend=c('VCI','NDMA EW phase','SPEI 3'), col=c(1,4,3), text.col = c(1,4,3),
       lty=1,pch=NA,bty='n',  border = NULL)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# scaling SPEI

plot(IsioloVCI$VCImean, type='l',ylim=c(-30,45),xaxt = "n",ylab=c("10 x SPEI 3,  VCI mean"),xlab=NA,main="Isiolo")
lines(10*IsioloVCI$SPEI3, type='l',col=3)
lines(10*IsioloVCI$PhaseInt, type='l',col=4)

atx<-c(8,20,32,44)
# axis(1, at=atx, labels=VCIphaseClim$Year[atx])
axis(1, at=atx, labels=c('Jan 2014','Jan 2015','Jan 2016', 'Jan 2017'))

legend(x=-4, y = 14, legend=c('Normal'),fill=NA,border=NA,lty=1,text.col = 4, bty = "n",lwd=3)
legend(x=-4, y = 24, legend=c('Alert'),fill=NA,border=NA,text.col = 4,bty = "n",lwd=3,  lty=1)
legend(x=-4, y = 34, legend=c('Alarm'),fill=NA,border=NA,text.col = 4, bty = "n",lwd=3, lty=1)

legend(x=36,y= -0.5,legend=c('VCI','NDMA EW phase','SPEI 3 - scaled x 10'), col=c(1,4,3), text.col = c(1,4,3),
       lty=1,pch=NA,bty='n',  border = NULL)

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Just overlapping period
scope<-16:32
plot(IsioloVCI$VCImean[scope], type='l',xlim=c(-2,17),ylim=c(-5,45),xaxt = "n",ylab=c("VCI mean, SPEI 3"),
     main="Isiolo",xlab=NA)
lines(IsioloVCI$SPEI3[scope], type='l',col=3)
lines(10*IsioloVCI$PhaseInt[scope], type='l',col=4)

atx<-c(5,11,17) 
axis(1, at=atx, labels=c('Jan 2015','Jul 2015', 'Jan 2016'))

legend(x=-3.5, y = 13, legend=c('Normal'),fill=NA,border=NA,lty=1,text.col = 4, bty = "n",lwd=3)
legend(x=-3.5, y = 23, legend=c('Alert'),fill=NA,border=NA,text.col = 4,bty = "n",lwd=3,  lty=1)
legend(x=-3.5, y = 33, legend=c('Alarm'),fill=NA,border=NA,text.col = 4, bty = "n",lwd=3, lty=1)

legend(x=1,y= 46,legend=c('VCI','NDMA EW phase','SPEI 3'), col=c(1,4,3), text.col = c(1,4,3),
       lty=1,pch=NA,bty='n',  border = NULL)
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#Kitui

plot(KituiVCI$VCImean, type='l',xlim=c(-5,55),ylim=c(-5,60),xaxt = "n",ylab=c(" SPEI 3, VCI mean"),main="Kitui",xlab=NA)
lines(KituiVCI$SPEI3, type='l',col=3)
lines(10*KituiVCI$PhaseInt, type='l',col=4)
atx<-c(8,20,32,44)
axis(1, at=atx, labels=c('Jan 2014','Jan 2015','Jan 2016', 'Jan 2017'))

legend(x=-10.5, y = 13, legend=c('Normal'),fill=NA,border=NA,lty=1,text.col = 4, bty = "n",lwd=3)
legend(x=-10.5, y = 23, legend=c('Alert'),fill=NA,border=NA,text.col = 4,bty = "n",lwd=3,  lty=1)
legend(x=-10.5, y = 33, legend=c('Alarm'),fill=NA,border=NA,text.col = 4, bty = "n",lwd=3, lty=1)

legend(x=40,y= 60,legend=c('VCI','NDMA EW phase','SPEI 3'), col=c(1,4,3), text.col = c(1,4,3),
       lty=1,pch=NA,bty='n',  border = NULL)
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Scaling SPEI 

plot(KituiVCI$VCImean, type='l',ylim=c(-20,60),xlim=c(-5,55),xaxt = "n",ylab=c("SPEI 3, VCI mean"),
     main="Kitui",xlab=NA)
lines(10*KituiVCI$SPEI3, type='l',col=3)
lines(10*KituiVCI$PhaseInt, type='l',col=4)

atx<-c(8,20,32,44)
axis(1, at=atx, labels=c('Jan 2014','Jan 2015','Jan 2016', 'Jan 2017'))

legend(x=-10.5, y = 14, legend=c('Normal'),fill=NA,border=NA,lty=1,text.col = 4, bty = "n",lwd=3)
legend(x=-10.5, y = 24, legend=c('Alert'),fill=NA,border=NA,text.col = 4,bty = "n",lwd=3,  lty=1)
legend(x=-10.5, y = 34, legend=c('Alarm'),fill=NA,border=NA,text.col = 4, bty = "n",lwd=3, lty=1)

legend(x=37.5,y= 62.5,legend=c('VCI','NDMA EW phase','SPEI 3 - scaled x 10'), col=c(1,4,3), text.col = c(1,4,3),
       lty=1,pch=NA,bty='n',  border = NULL)