rm(list=ls())

WDuni<-c("/home/m/mn/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/sussex/US/paper3") # doma

setwd(WDuni)
setwd(WDhome)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(plm)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Crops<-read.csv( "New31Jan2018/KE_Ag_Stats_2014.csv",header=TRUE, na.strings = c("NA","NC","NC.","-","#DIV/0!"),colClasses=c("factor","factor","factor","factor","factor","factor","factor","factor","numeric","numeric","numeric"))  #numeric,numeric,numeric))
CrMaize2<-subset(Crops, Crop=="Maize" & !Admin2=="" & Season=='')

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
table(CrMaize2$Year)

hist(CrMaize2$MT,100)
hist(CrMaize2$Area,100)
hist(CrMaize2$Yield,100)


boxplot(CrMaize2$MT)
boxplot(CrMaize2$Area)
boxplot(CrMaize2$Yield)

boxplot(CrMaize2$MT~CrMaize2$Year)
boxplot(CrMaize2$Yield~CrMaize2$Year)
boxplot(CrMaize2$Area~CrMaize2$Year)

tapply(CrMaize2$Area,CrMaize2$Year,median)
lines(tapply(CrMaize2$Area,CrMaize2$Year,median))


boxplot(CrMaize2$Area[CrMaize2$Year==1992])