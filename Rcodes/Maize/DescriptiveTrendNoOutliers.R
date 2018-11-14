rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)
library('plm')
library('dplyr')
library('tseries')
library('lme4')
library('lmerTest')
options(max.print=10000)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

load("Main/CrMaize14.RData")
load("Main/CrMaize13.RData")
load("Main/CrMaize11.RData")

rm(CrMaize14ts)
CrMaize14ts<-pdata.frame(CrMaize14,index=c("ID","Year"))

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

boxplot(Yield~Year,data=CrMaize14ts)
boxplot(lag(Yield)~Year,data=CrMaize14ts)


plot(aggregate(Yield ~ Year, CrMaize14ts, sd))
plot(aggregate(Yield ~ Year, CrMaize14ts, mean))
plot(aggregate(Yield ~ Year, CrMaize14ts, median))

FD14<-diff(CrMaize14ts$Yield)
plot(aggregate(FD14 ~ index(FD14)$Year, FD14, mean))
plot(aggregate(FD ~ index(FD)$Year, FD, mean),ylim=c(-0.6,0.6))
lines(aggregate(FD14 ~ index(FD14)$Year, FD14, mean),col=3,type="p")
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

t.test(CrMaize14ts$Yield[CrMaize14ts$Year%in%1971:1990],CrMaize14ts$Yield[CrMaize14ts$Year%in%1991:2015])
t.test(CrMaize14ts$Yield[CrMaize14ts$Year%in%1971:1996],CrMaize14ts$Yield[CrMaize14ts$Year%in%1997:2015])
t.test(CrMaize14ts$MT[CrMaize14ts$Year%in%1971:1996],CrMaize14ts$MT[CrMaize14ts$Year%in%1997:2015])
t.test(CrMaize14ts$MT[CrMaize14ts$Year%in%1989:1996],CrMaize14ts$MT[CrMaize14ts$Year%in%1990:2015])
# t-test signifikantni vypada to, ze tam bude trend...


plot(aggregate((CrMaize14ts$MT/20000) ~ Year, CrMaize14ts, mean),ylim=c(1,4.5))
lines(aggregate(CrMaize14ts$Yield ~ Year, CrMaize14ts, mean),type="p",col='blue')
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# various adftests

adf.test(seq(1:1000), k=2) # to be sure how it works
adf.test(rnorm(1000), k=2) 


adftestss<-sapply(unique(CrMaize14ts$ID), function(x) adf.test(CrMaize14ts$Yield[CrMaize14ts$ID==x])  )
adftestss

ts14Part<-subset(CrMaize14ts,Year %in% 2001:2014 )
adftestPart<-sapply(unique(ts14Part$ID), function(x) adf.test(ts14Part$Yield[ts14Part$ID==x])  )
adftestPart

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# test of trends

CrMaize14ts$t<-as.numeric(as.character(CrMaize14ts$Year))-1969
Tariq<-lmer(Yield~t +  (t|ID),data=CrMaize14ts)
summary(Tariq) 

Tariq2<-plm(Yield~t ,data=CrMaize14ts)
summary(Tariq2)

Tariq2<-plm(Yield~t*ID ,data=CrMaize14ts)
summary(Tariq2)


Tariq3<-pvcm(Yield~t ,data=CrMaize14ts)
summary(Tariq3)


Tariq3<-pvcm(Yield~t ,model=c("random"),data=CrMaize14ts)
summary(Tariq3)


#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ts14Part<-subset(CrMaize14ts,Year %in% 1989:2014 ) 
Tariq5<-lmer(Yield~t +  (t|ID),data=ts14Part)
summary(Tariq5) 

Tariq6<-plm(Yield~t ,data=ts14Part)
summary(Tariq6)

Tariq7<-plm(Yield~t*ID ,data=ts14Part)
summary(Tariq7)


Tariq8<-pvcm(Yield~t ,data=ts14Part)
summary(Tariq8)
Tariq9<-pvcm(Yield~t ,model=c("random"),data=ts14Part)
summary(Tariq9)



#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# now do the same but without the outliers...