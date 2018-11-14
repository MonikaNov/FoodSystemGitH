rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)
library('plm')
library('reshape2')
library('tseries')

options(max.print=10000)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

load("Main/CrMaize14.RData")
load("Main/CrMaize13.RData")

rm(CrMaize13ts)
CrMaize13ts<-pdata.frame(CrMaize13,index=c("ID","Year"))
rm(CrMaize14ts)
CrMaize14ts<-pdata.frame(CrMaize14,index=c("ID","Year"))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

FAOSTATmz<-read.csv( "cropsMe/FAOSTAT_data_6-22-2018.csv",header=TRUE)
rm(FAOSTATmz2)
FAOSTATmz2<-cast(FAOSTATmz, Year~Element,value=c("Value"))

rm(FAOSTATmzFlag)
FAOSTATmzFlag<-cast(FAOSTATmz, Year~Element,value=c("Flag.Description"))


rm(FAOSTATmz3)

FAOSTATmz3<-merge(FAOSTATmz2,FAOSTATmzFlag,by=("Year"))
names(FAOSTATmz3)
names(FAOSTATmz3)<-c("Year","Area","Production","Yield","AreaFlag","ProductionFlag","YieldFlag")

FAOSTATmz4<-subset(FAOSTATmz3, Year %in% c(1970:2014))

# rm(list=c('FAOSTATmz','FAOSTATmz2','FAOSTATmz3'))
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

MTtotal<-aggregate(CrMaize14$MT, by=list(CrMaize14$Year), FUN=sum)
names(MTtotal)[1]<-"Year"
names(MTtotal)[2]<-"MT"
AreaTotal<-aggregate(CrMaize14ts$Area, by=list(CrMaize14ts$Year), FUN=sum)
names(AreaTotal)[1]<-"Year"
names(AreaTotal)[2]<-"Area"
# to check in

sum(CrMaize14ts$MT[which(CrMaize14ts$Year==2011)])

sum(CrMaize14ts$Area[which(CrMaize14ts$Year==1979)])
sum(CrMaize14ts$Area[which(CrMaize14ts$Year==1988)])
sum(CrMaize14ts$Area[which(CrMaize14ts$Year==2013)])             # GROOT !!!!!!!!!!!!!!!!!!!      

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ok, now the plots:

plot(MTtotal$MT,FAOSTATmz4$Production)
plot(AreaTotal$Area,FAOSTATmz4$Area)

#--------------------------------

plot(MTtotal$x,FAOSTATmz4$Area,xlim=c(374000,3700000),ylim=c(171300,3750000))
lines(AreaTotal$x,FAOSTATmz4$Production, col="blue", type='p')

#--------------------------------

AreaDiff<-FAOSTATmz4$Area-AreaTotal$x
MTDiff<-FAOSTATmz4$Production-MTtotal$x

AreaRelD<-100*(FAOSTATmz4$Area-AreaTotal$x)/FAOSTATmz4$Area
MTRelD<-100*(FAOSTATmz4$Production-MTtotal$x)/MTtotal$x

#--------------------------------
summary(AreaRelD)
summary(MTRelD)

hist(AreaRelD)
hist(MTRelD,15)  # look awfull - too big differences

FAOSTATmz4$Year[which(abs(AreaRelD)>15)]
AreaTotal$Year[which(abs(AreaRelD)>15)]

FAOSTATmz4$Year[which(abs(MTRelD)>15)]
MTtotal$Year[which(abs(MTRelD)>15)]

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

sum(CrMaize14ts$MT[CrMaize14ts$Year==1970])
FAOSTATmz4$Production[FAOSTATmz4$Year==1970]
MTRelD[FAOSTATmz4$Year==1970]

sum(CrMaize14ts$MT[CrMaize14ts$Year==1971])
FAOSTATmz4$Production[FAOSTATmz4$Year==1971]
MTRelD[FAOSTATmz4$Year==1971]
(1090110-1400000)/1090110