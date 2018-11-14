rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)
library('plm')
library('dplyr')
library('tseries')
library('urca')
options(max.print=10000)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
load("Main/CrMaize14.RData")
load("Main/CrMaize13.RData")
load("Main/CrMaize11.RData")

rm(CrMaize13ts)
CrMaize13ts<-pdata.frame(CrMaize13,index=c("ID","Year"))
rm(CrMaize14ts)
CrMaize14ts<-pdata.frame(CrMaize14,index=c("ID","Year"))
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# downwards trend, MT, and yields mean and median

plot(aggregate((CrMaize13ts$MT/100000) ~ Year, CrMaize13ts, mean),ylim=c(0,2.5),ylab=c("Maize"))
lines(aggregate(CrMaize13ts$Yield ~ Year, CrMaize13ts, mean),type="p",col='blue')
lines(aggregate(CrMaize13ts$Yield ~ Year, CrMaize13ts, median),type="p",col='red')
legend(x=18, y = 2.6, legend=c("MT/hectare - mean over counties","MT/hectare - median over counties","MT scaled by 100,000")
       ,bty="o",col=c('blue','red','black'),pch=c(1,1,15))

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# downward trend,yields mean IQR

plot(aggregate(CrMaize13ts$Yield ~ Year, CrMaize13ts, mean),ylim=c(0.5,2.4),col='blue',ylab=c("Yield maize MT/hectar, average over counties"))
lines(aggregate(CrMaize13ts$Yield ~ Year, CrMaize13ts, mean),col='blue',type="p",pch=15)
lines(aggregate(CrMaize13ts$Yield ~ Year, CrMaize13ts, IQR),col='red',type="p",pch=19)
legend(x=27, y = 2.4, legend=c("Mean","Inter quartile range"),bty="o",col=c('blue','red'),pch=c(15,19))
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# bpnus: some nice histograms

hist(CrMaize13ts$Yield,100)
hist(CrMaize13ts$Yield,100,xlim=c(0,5))
hist(CrMaize14ts$Yield,50,xlim=c(0,5))
# loggss

hist(log(CrMaize13ts$Yield),100)
hist(log(CrMaize14ts$Yield),100)
hist(log(CrMaize14ts$Yield),100,xlim=c(-3.5,2))
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# now plot of those that have trend -> there is an interesting hub..


Countiess<-unique(CrMaize14ts$ID)
#FOLLOWING DON'T seem to have the downward trend:
rm(NoDown)
NoDown<-Countiess[c(4,6,7,10,12,17,19,21,29,31,32,34,35,36,37,39,40,43,46,47)]
plot(CrMaize14ts$Yield[! CrMaize14ts$ID %in% NoDown]~CrMaize14ts$Year[! CrMaize14ts$ID %in% NoDown],
     col='red',type="p",pch=15,ylab=c("Yield maize MT/hectar"),xlab="Year",main="Counties with a more pronounced downward trend")