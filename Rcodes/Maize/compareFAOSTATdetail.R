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


plot(FAOSTATmz4$Production,FAOSTATmz4$Area,xlim=c(0,3800000),ylim=c(0,3800000),xlab=c('Production'),ylab=c('Area'))
lines(AreaTotal$Area,MTtotal$MT, col="blue", type='p')

plot(YieldTotal$Yield,FAOSTATmz4$Yield/10000,xlab="FEWSNET",ylab="FAOSTAT",main="Yield (MT/Hectares)")

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# different colours for different decades

plot(YieldTotal$Yield[YieldTotal$Year %in% c(1970:1979)],FAOSTATmz4$Yield[FAOSTATmz4$Year %in% c(1970:1979)]/10000,
     xlab="FEWSNET",ylab="FAOSTAT",main="Yield (MT/Hectares)",xlim=c(1.3,2.4),ylim=c(1.15,2.2),col="Yellow2",pch=16)
lines(YieldTotal$Yield[YieldTotal$Year %in% c(1980:1989)],FAOSTATmz4$Yield[FAOSTATmz4$Year %in% c(1980:1989)]/10000,
     type="p",main="Yield (MT/Hectares)",col="darkorange",pch=16)
lines(YieldTotal$Yield[YieldTotal$Year %in% c(1990:1999)],FAOSTATmz4$Yield[FAOSTATmz4$Year %in% c(1990:1999)]/10000,
      type="p",main="Yield (MT/Hectares)",col="maroon2",pch=16)
lines(YieldTotal$Yield[YieldTotal$Year %in% c(2000:2009)],FAOSTATmz4$Yield[FAOSTATmz4$Year %in% c(1990:1999)]/10000,
      type="p",main="Yield (MT/Hectares)",col="darkred",pch=16)
lines(YieldTotal$Yield[YieldTotal$Year %in% c(2010:2019)],FAOSTATmz4$Yield[FAOSTATmz4$Year %in% c(2010:2019)]/10000,
      type="p",main="Yield (MT/Hectares)",col="black",pch=16)
legend(x=1.25, y = 2.25, legend=c("1970-1979","1980-1989","1990-1999","2000-2009","2010-2014")
       ,bty="o",col=c('Yellow2','darkorange','maroon2','darkred','black'),pch=c(16),ncol=5,cex=0.78)


# same plot as above, just different color scheme, maybe different scale

plot(YieldTotal$Yield[YieldTotal$Year %in% c(1970:1979)],FAOSTATmz4$Yield[FAOSTATmz4$Year %in% c(1970:1979)]/10000,
     xlab="FEWSNET",ylab="FAOSTAT",main="Yield (MT/Hectares)",xlim=c(1.3,2.4),ylim=c(1.15,2.2),col="darkolivegreen1")
lines(YieldTotal$Yield[YieldTotal$Year %in% c(1980:1989)],FAOSTATmz4$Yield[FAOSTATmz4$Year %in% c(1980:1989)]/10000,
      type="p",main="Yield (MT/Hectares)",col="chartreuse1")
lines(YieldTotal$Yield[YieldTotal$Year %in% c(1990:1999)],FAOSTATmz4$Yield[FAOSTATmz4$Year %in% c(1990:1999)]/10000,
      type="p",main="Yield (MT/Hectares)",col="darkolivegreen4")
lines(YieldTotal$Yield[YieldTotal$Year %in% c(2000:2009)],FAOSTATmz4$Yield[FAOSTATmz4$Year %in% c(1990:1999)]/10000,
      type="p",main="Yield (MT/Hectares)",col="darkgreen")
lines(YieldTotal$Yield[YieldTotal$Year %in% c(2010:2019)],FAOSTATmz4$Yield[FAOSTATmz4$Year %in% c(2010:2019)]/10000,
      type="p",main="Yield (MT/Hectares)",col="black")

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# now production


plot(MTtotal$MT[MTtotal$Year %in% c(1970:1979)],FAOSTATmz4$Production[FAOSTATmz4$Year %in% c(1970:1979)],
     xlab="FEWSNET",ylab="FAOSTAT",main="Production (MT)",xlim=c(270000,3950000),ylim=c(1400000,3950000),col="Yellow2",pch=16)
lines(MTtotal$MT[MTtotal$Year  %in% c(1980:1989)],FAOSTATmz4$Production[FAOSTATmz4$Year %in% c(1980:1989)],
      type="p",main="Yield (MT/Hectares)",col="darkorange",pch=16)
lines(MTtotal$MT[MTtotal$Year  %in% c(1990:1999)],FAOSTATmz4$Production[FAOSTATmz4$Year %in% c(1990:1999)],
type="p",col="maroon2",pch=16)
lines(MTtotal$MT[MTtotal$Year  %in% c(2000:2009)],FAOSTATmz4$Production[FAOSTATmz4$Year %in% c(1990:1999)],
      type="p",col="darkred",pch=16)
lines(MTtotal$MT[MTtotal$Year  %in% c(2010:2019)],FAOSTATmz4$Production[FAOSTATmz4$Year %in% c(2010:2019)],
      type="p",main="Yield (MT/Hectares)",col="black",pch=16)
legend(x=115000, y = 4050000, legend=c("1970-1979","1980-1989","1990-1999","2000-2009","2010-2014")
       ,bty="o",col=c('Yellow2','darkorange','maroon2','darkred','black'),pch=c(16),ncol=5,cex=0.78)



#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# and area


plot(AreaTotal$Area[AreaTotal$Year %in% c(1970:1979)],FAOSTATmz4$Area[FAOSTATmz4$Year %in% c(1970:1979)],
     xlab="FEWSNET", ylab="FAOSTAT",main="Area (Hectares)",xlim=c(200000,2100000),ylim=c(950000,2280000),col="Yellow2",pch=16)
lines(AreaTotal$Area[AreaTotal$Year %in% c(1980:1989)],FAOSTATmz4$Area[FAOSTATmz4$Year%in% c(1980:1989)],
      type="p",main="Yield (MT/Hectares)",col="darkorange",pch=16)
lines(AreaTotal$Area[AreaTotal$Year  %in% c(1990:1999)],FAOSTATmz4$Area[FAOSTATmz4$Year %in% c(1990:1999)],
      type="p",col="maroon2",pch=16)
lines(AreaTotal$Area[AreaTotal$Year  %in% c(2000:2009)],FAOSTATmz4$Area[FAOSTATmz4$Year %in% c(1990:1999)],
      type="p",col="darkred",pch=16)
lines(AreaTotal$Area[AreaTotal$Year %in% c(2010:2019)],FAOSTATmz4$Area[FAOSTATmz4$Year %in% c(2010:2019)],
      type="p",main="Yield (MT/Hectares)",col="black",pch=16)
legend(x=120000, y = 2330000, legend=c("1970-1979","1980-1989","1990-1999","2000-2009","2010-2014")
       ,bty="o",col=c('Yellow2','darkorange','maroon2','darkred','black'),pch=c(16),ncol=5,cex=0.78)
