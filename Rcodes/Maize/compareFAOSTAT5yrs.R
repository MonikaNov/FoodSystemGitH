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
rm(YieldTotal)
YieldTotal<-MTtotal
YieldTotal$MT<-MTtotal$MT/AreaTotal$Area
names(YieldTotal)[1]<-"Year"
names(YieldTotal)[2]<-"Yield"

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ok, now the plots:

plot(MTtotal$MT,FAOSTATmz4$Production,xlab="FEWSNET",ylab="FAOSTAT",main="Production (MT)")
plot(AreaTotal$Area,FAOSTATmz4$Area,xlab="FEWSNET",ylab="FAOSTAT",main="Area (Hectares)")
plot(YieldTotal$Yield,FAOSTATmz4$Yield/10000,xlab="FEWSNET",ylab="FAOSTAT",main="Yield (MT/Hectares)")
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# now I need to get the 5 years averages

# 1. FAOSTAT

FAOSTAT5yr<-aggregate(FAOSTATmz4[2:4], by =list(year5=rep(seq(1970, 2014,5), times=rep(5,9))), FUN=mean)

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 2 FEWSNET

rm(Yield5yrs)
Yield5yrs<-aggregate(YieldTotal[2], by =list(year5=rep(seq(1970, 2014,5), times=rep(5,9))), FUN = mean)
Yield5yrs

rm(MT5yrs)
MT5yrs<-aggregate(MTtotal$MT, by =list(year5=rep(seq(1970, 2014,5), times=rep(5,9))), FUN = mean)
names(MT5yrs)[2]<-"MT"

rm(Area5yrs)
Area5yrs<-aggregate(AreaTotal[2], by =list(year5=rep(seq(1970, 2014,5), times=rep(5,9))), FUN = mean)
Area5yrs

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# now the plots
mylabels<-c("1970-1975","1976-1980","1981-1985","1986-1990","1991-1995","1996-2000","2001-2005","2006-2010","2010-2014")

plot(Yield5yrs$Yield,FAOSTAT5yr$Yield/10000,xlab="FEWSNET",ylab="FAOSTAT",xlim=c(1.3,2.2),ylim=c(1.15,1.9),
     main="Yield (MT/Hectares), 5-year average")
text(Yield5yrs$Yield,FAOSTAT5yr$Yield/10000, labels=mylabels, cex= 0.65, pos=3)

plot(Yield~year5, data = Yield5yrs, xlab = "Year", ylab = "Yield (MT/Hectares), FEWSNET",     main="Yield (MT/Hectares), 5-year average")

# now more zoom, to see if I can see more of correlation

plot(Yield5yrs$Yield,FAOSTAT5yr$Yield/10000,xlab="FEWSNET",ylab="FAOSTAT",xlim=c(1,3),ylim=c(1,3),
     main="Yield (MT/Hectares), 5-year average",sub = "Zoom out (the scale is relatively small)")
text(Yield5yrs$Yield,FAOSTAT5yr$Yield/10000, labels=mylabels, cex= 0.65, pos=3)



plot(MT5yrs$MT,FAOSTAT5yr$Production,xlab="FEWSNET",ylab="FAOSTAT",main="Production (MT), 5-year average",xlim=c(270000,3950000),ylim=c(1400000,3950000))
text(MT5yrs$MT,FAOSTAT5yr$Production, labels=mylabels, cex= 0.7, pos=3)

plot(Area5yrs$Area,FAOSTAT5yr$Area,xlab="FEWSNET",ylab="FAOSTAT",main="Area (Hectares), 5-year average",xlim=c(550000,2100000),ylim=c(1200000,2200000))
text(Area5yrs$Area,FAOSTAT5yr$Area, labels=mylabels, cex= 0.7, pos=3)
