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

boxplot(Yield~Year,data=CrMaize13ts)
boxplot(lag(Yield)~Year,data=CrMaize13ts)


plot(aggregate(Yield ~ Year, CrMaize13ts, sd))
plot(aggregate(Yield ~ Year, CrMaize13ts, mean))
plot(aggregate(Yield ~ Year, CrMaize13ts, median))

FD<-diff(CrMaize13ts$Yield)

FD[index(FD)==80]

unique(CrMaize13ts$ID)
par(mfrow=c(3,1))
plot(FD[index(FD)==80],type="l")
plot(FD[index(FD)==65],type="l")
plot(FD[index(FD)==64],type="l")


plot(diff(CrMaize13ts$Yield)~CrMaize13ts$Year)

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

t.test(CrMaize13ts$Yield[CrMaize13ts$Year%in%1971:1990],CrMaize13ts$Yield[CrMaize13ts$Year%in%1991:2015])
t.test(CrMaize13ts$Yield[CrMaize13ts$Year%in%1971:1996],CrMaize13ts$Yield[CrMaize13ts$Year%in%1996:2015])

# t-test signifikantni vypada to, ze tam bude trend...


# now plot of MT and yields - DOWNWARDS TREND
plot(aggregate((CrMaize13ts$MT/20000) ~ Year, CrMaize13ts, mean),ylim=c(1,4.5))
lines(aggregate(CrMaize13ts$Yield ~ Year, CrMaize13ts, mean),type="p",col='blue')

# now without The outliers

plot(aggregate((CrMaize13ts$MT/100000) ~ Year, CrMaize13ts, mean),ylim=c(0,2.5))
lines(aggregate(CrMaize13ts$Yield ~ Year, CrMaize13ts, mean),type="p",col='blue')
lines(aggregate(CrMaize13ts$Yield ~ Year, CrMaize13ts, median),type="p",col='red')


# and scaled

plot(aggregate((CrMaize13ts$MT/100000) ~ Year, CrMaize13ts, mean))
lines(aggregate(CrMaize13ts$Yield ~ Year, CrMaize13ts, mean),type="p",col='blue')
lines(aggregate(CrMaize13ts$Yield ~ Year, CrMaize13ts, median),type="p",col='red')

# also other measures of variability may be interesting

plot(aggregate(CrMaize13ts$Yield ~ Year, CrMaize13ts, mean),ylim=c(0.5,2.4),col='blue',type="h",ylab=c("Yield maize MT/hectar"))
lines(aggregate(CrMaize13ts$Yield ~ Year, CrMaize13ts, mean),col='blue',type="p",pch=15)
lines(aggregate(CrMaize13ts$Yield ~ Year, CrMaize13ts, IQR),col='red',type="p",pch=19)
legend(x=27, y = 2.4, legend=c("Mean","Inter quartile range"),bty="o",col=c('blue','red'),pch=c(15,19))

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
 # now if I plot each county individually, is there also a downwards trends?
#what about west counties separately?

# getting zones:
CrMaize14ts<-CrMaize14ts[with(CrMaize14ts,order(ID,Year)),]
aa<-CrMaize14ts
zones<-read.csv(file='/its/home/mn301/foodSystems/Rcodes/Eq1ProdFun1/zones/WestEast.csv')
CrMaize14ts<-merge(CrMaize14ts,zones,all.x=TRUE,sort=FALSE)
CrMaize14ts<-CrMaize14ts[with(CrMaize14ts,order(ID,Year)),]
all.equal(aa,CrMaize14ts[c(2:5,1,6:13)],check.attributes=FALSE)

plot(aggregate(Yield ~ Year, CrMaize14ts, mean),ylim=c(0.5,2.7),col='blue',type="h",ylab=c("Yield maize MT/hectar"))
lines(aggregate(Yield ~ Year, CrMaize14ts, mean),col='blue',type="p",pch=15)

# now plotting the zones seperately:

lines(aggregate(Yield ~ Year, subset(CrMaize14ts,west1==1), mean),col='red',type="p",pch=15,ylab=c("Yield maize MT/hectar"))
lines(aggregate(Yield ~ Year, subset(CrMaize14ts,west1==0), mean),col='green',type="p",pch=15,ylab=c("Yield maize MT/hectar"))

# now one county by one

Countiess<-unique(CrMaize14ts$ID)
i<-0

i<-i+1
plot(CrMaize14ts$Yield[CrMaize14ts$ID==Countiess[i]]~CrMaize14ts$Year[CrMaize14ts$ID==Countiess[i]],col='blue',type="p",pch=15,ylab=c("Yield maize MT/hectar")); i

#FOLLOWING DON'T seem to have the downward trend:
rm(NoDown)
NoDown<-Countiess[c(4,6,7,10,12,17,19,21,29,31,32,34,35,36,37,39,40,43,46,47)]
unique(CrMaize14ts$county[which(CrMaize14ts$ID %in% NoDown)])
CrMaize14ts$west1[which(CrMaize14ts$ID %in% NoDown)]

unique(CrMaize14ts$county[which(! CrMaize14ts$ID %in% NoDown)])
CrMaize14ts$west1[which(! CrMaize14ts$ID %in% NoDown)]

plot(aggregate(Yield ~ Year, subset(CrMaize14ts, ID %in% NoDown), mean),ylim=c(0.5,2.7),col='blue',type="h",ylab=c("Yield maize MT/hectar"))
lines(aggregate(Yield ~ Year, subset(CrMaize14ts,ID %in% NoDown), mean),col='blue',type="p",pch=15)

lines(aggregate(Yield ~ Year, subset(CrMaize14ts,! ID %in% NoDown), mean),col='green',type="p",pch=15,ylab=c("Yield maize MT/hectar"))


plot(CrMaize14ts$Yield[CrMaize14ts$ID %in% NoDown]~CrMaize14ts$Year[CrMaize14ts$ID %in% NoDown],col='blue',type="p",pch=15,ylab=c("Yield maize MT/hectar"))

plot(CrMaize14ts$Yield[! CrMaize14ts$ID %in% NoDown]~CrMaize14ts$Year[! CrMaize14ts$ID %in% NoDown],col='red',type="p",pch=15,ylab=c("Yield maize MT/hectar"))

i<-0

i<-i+1
plot(CrMaize14ts$Yield[CrMaize14ts$ID==NoDown[i]]~CrMaize14ts$Year[CrMaize14ts$ID==NoDown[i]],col='blue',type="p",pch=15,ylab=c("Yield maize MT/hectar")); i

