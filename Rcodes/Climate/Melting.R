rm(list=ls())

WDuni<-c("/home/m/mn/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/sussex/US/paper3") # doma

setwd(WDuni)
setwd(WDhome)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(reshape)
library(dplyr)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Climate0<-read.csv( "CountyClimateM/pre_kenya_adm2_1999-2015.10thperc.csv",header=TRUE)
summary(Climate0)


Climate1<-melt(Climate0, id.vars=c(1,2,3))
names(Climate1)[4]<-'MonthYear'
Climate1$Month<-substr(Climate1$MonthYear,2,3)
Climate1$Year<-substr(Climate1$MonthYear,5,8)

jupi<-summarise(group_by(Climate1, Month, ID1),
          mean=mean(value), sd=sd(value))

print(as.data.frame(jupi))