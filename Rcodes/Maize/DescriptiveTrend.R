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

rm(CrMaize13ts)
CrMaize13$Year<-as.numeric(as.character(CrMaize13$Year))


CrMaize13ts<-pdata.frame(CrMaize13,index=c("ID","Year"))

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

boxplot(Yield~Year,data=CrMaize13ts)
boxplot(lag(Yield)~Year,data=CrMaize13ts)


plot(aggregate(Yield ~ Year, CrMaize13ts, sd))
plot(aggregate(Yield ~ Year, CrMaize13ts, mean))
plot(aggregate(Yield ~ Year, CrMaize13ts, median))

FD<-diff(CrMaize13ts$Yield)
plot(aggregate(FD ~ index(FD)$Year, FD, mean))


plot(diff(CrMaize13ts$Yield)~CrMaize13ts$Year)

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

t.test(CrMaize13ts$Yield[CrMaize13ts$Year%in%1971:1990],CrMaize13ts$Yield[CrMaize13ts$Year%in%1991:2015])

# t-test signifikantni vypada to, ze tam bude trend...


plot(aggregate((CrMaize13ts$MT/20000) ~ Year, CrMaize13ts, mean),ylim=c(1,4.5))
lines(aggregate(CrMaize13ts$Yield ~ Year, CrMaize13ts, mean),type="p",col='blue')
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# various adftests

adf.test(seq(1:1000), k=2) # to be sure how it works
adf.test(rnorm(1000), k=2) 


adftestss<-sapply(unique(CrMaize13ts$ID), function(x) adf.test(CrMaize13ts$Yield[CrMaize13ts$ID==x])  )
adftestss

ts13Part<-subset(CrMaize13ts,Year %in% 2001:2014 )
adftestPart<-sapply(unique(ts13Part$ID), function(x) adf.test(ts13Part$Yield[ts13Part$ID==x])  )
adftestPart

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# test of trends

CrMaize13ts$t<-as.numeric(as.character(CrMaize13ts$Year))-1969
Saga<-lmer(Yield~t +  (t|ID),data=CrMaize13ts)
summary(Saga) 

Saga2<-plm(Yield~t ,data=CrMaize13ts)
summary(Saga2)

Saga2<-plm(Yield~t*ID ,data=CrMaize13ts)
summary(Saga2)


Saga3<-pvcm(Yield~t ,data=CrMaize13ts)
summary(Saga3)


Saga3<-pvcm(Yield~t ,model=c("random"),data=CrMaize13ts)
summary(Saga3)


#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ts13Part<-subset(CrMaize13ts,Year %in% 1985:2014 ) # from about 1990,1991,1992 trend switch from negative to positive!!!then around 1996 it gets significant
Saga5<-lmer(Yield~t +  (t|ID),data=ts13Part)
summary(Saga5) 

Saga6<-plm(Yield~t ,data=ts13Part)
summary(Saga6)

Saga7<-plm(Yield~t*ID ,data=ts13Part)
summary(Saga7)


Saga8<-pvcm(Yield~t ,data=ts13Part)
summary(Saga8)
Saga9<-pvcm(Yield~t ,model=c("random"),data=ts13Part)
summary(Saga9)



#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# now do the same but without the outliers...