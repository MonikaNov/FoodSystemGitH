rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)
library('plm')
library('dplyr')
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

load("Main/CrMaize13.RData")
load("Main/CrMaize11.RData")

rm(CrMaize13ts)
CrMaize13ts<-pdata.frame(CrMaize13,index=c("ID","Year"))

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

boxplot(Yield~Year,data=CrMaize13ts)
boxplot(lag(Yield)~Year,data=CrMaize13ts)

plot.ts(CrMaize13ts$Yield,lag(CrMaize13ts$Yield))
plot.ts(CrMaize13ts$Yield,diff(CrMaize13ts$Yield))
plot(diff(CrMaize13ts$Yield)~CrMaize13ts$Year)

plot(diff(CrMaize13ts$Yield)~CrMaize13ts$Year,ylim=c(-0.5,0.5))

diff(CrMaize13ts$Yield)

dif1<-diff(CrMaize13ts$Yield)


summary(diff(CrMaize13ts$Yield))

#check what does aggregate do and dply

index(diff(CrMaize13ts$Yield))

aggregate(Yield ~ Year, CrMaize13ts, mean)
aggregate(Yield ~ Year, CrMaize13ts, sd)
plot(aggregate(Yield ~ Year, CrMaize13ts, sd))

sd(subset(CrMaize13$Yield, CrMaize13$Year==2005),na.rm = TRUE)

aggregate(diff(CrMaize13ts$Yield) ~ Year, CrMaize13ts, mean)
aggregate(diff(CrMaize13ts$Yield) ~ Year, CrMaize13ts, median)
median(subset(diff(CrMaize13ts$Yield), CrMaize13ts$Year %in% 2009),na.rm = TRUE)
mean(subset(diff(CrMaize13ts$Yield), CrMaize13ts$Year %in% 1980),na.rm = TRUE)
aggregate(diff(CrMaize13ts$Yield) ~ Year, CrMaize13ts, summary)

plot(aggregate(diff(CrMaize13ts$Yield) ~ Year, CrMaize13ts, mean),ylim=c(-15,15))
lines(aggregate(diff(CrMaize13ts$Yield) ~ Year, CrMaize13ts, max),type="p")
lines(aggregate(diff(CrMaize13ts$Yield) ~ Year, CrMaize13ts, min),type="p")

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# now dplyr, what does it do
aggregate((CrMaize13ts$Yield) ~ Year, CrMaize13ts, mean)
summarise(group_by(CrMaize13ts,   Year), mean(Yield))

print.data.frame(summarise(group_by(CrMaize13ts,   Year), mean=mean(Yield),sd=sd(Yield)))


(print.data.frame(summarise(group_by(CrMaize13ts,   Year), mean=mean(Yield),sd=sd(Yield))))

plot(print.data.frame(summarise(group_by(CrMaize13ts,   Year), sd=sd(Yield))))
lines(print.data.frame(summarise(group_by(CrMaize13ts,   Year), mean=mean(Yield))),type="p")

plot(print.data.frame(summarise(group_by(CrMaize13ts,   Year), IQR=IQR(Yield))))
lines(print.data.frame(summarise(group_by(CrMaize13ts,   Year), mean=mean(Yield))),type="p")

plot(aggregate(diff(CrMaize13ts$Yield) ~ Year, CrMaize13ts, mean))
plot(aggregate((CrMaize13ts$Yield) ~ Year, CrMaize13ts, mean))
plot(aggregate((CrMaize13ts$Yield) ~ Year, CrMaize13ts, median))
plot(aggregate((CrMaize13ts$Yield) ~ Year, CrMaize13ts, median),ylim=c(-1,15))
plot(CrMaize13ts$Yield ~ Year, data=CrMaize13ts)
plot(CrMaize13ts$Yield ~ Year, data=CrMaize13ts,ylim=c(0,5))
plot(aggregate((CrMaize13ts$Yield) ~ Year, CrMaize13ts, median),ylim=c(0,3))


# MEAN AND MEDIAN KLESA!!! test stacionarity? Trend???

# testy taky od r. 1990??

t.test(CrMaize13ts$Yield[CrMaize13ts$Year%in%1971:1990],CrMaize13ts$Yield[CrMaize13ts$Year%in%1991:2015])

# t-test signifikantni vypada to, ze tam bude trend...


plot(aggregate((CrMaize13ts$MT/20000) ~ Year, CrMaize13ts, mean),ylim=c(1,4.5))

lines(aggregate(CrMaize13ts$Yield ~ Year, CrMaize13ts, mean),type="p",col='blue')

summarise(group_by(CrMaize13ts,   Year), length(Yield))
print(data.frame(summarise(group_by(CrMaize13ts,   Year), length(Yield))))