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

plot(MTtotal$MT,FAOSTATmz4$Production,xlab="FEWSNET",ylab="FAOSTAT",main="Production (MT)")
plot(AreaTotal$Area,FAOSTATmz4$Area,xlab="FEWSNET",ylab="FAOSTAT",main="Area (Hectares)")

cor.test(MTtotal$MT,FAOSTATmz4$Production)
cor.test(AreaTotal$Area,FAOSTATmz4$Area)
#--------------------------------


plot(FAOSTATmz4$Production,FAOSTATmz4$Area,xlim=c(0,3800000),ylim=c(0,3800000),xlab=c('Production'),ylab=c('Area'))
lines(AreaTotal$Area,MTtotal$MT, col="blue", type='p')
#--------------------------------

MTDiff<-FAOSTATmz4$Production-MTtotal$MT
AreaDiff<-FAOSTATmz4$Area-AreaTotal$Area

MTRelD<-100*(FAOSTATmz4$Production-MTtotal$MT)/MTtotal$MT
AreaRelD<-100*(FAOSTATmz4$Area-AreaTotal$Area)/AreaTotal$Area
#--------------------------------
summary(MTRelD)
summary(AreaRelD)

summary(abs(MTRelD))
summary(abs(AreaRelD))


hist(MTRelD,15)  # look awfull - too big differences
hist(MTRelD,30)  
hist(MTRelD,20,xlab=c("Difference between FAOSTAT and FEWSNET data in %"),main=c("Histogram - production"))  
hist(AreaRelD)
hist(AreaRelD,30,xlab=c("Difference between FAOSTAT and FEWSNET data in %"),main=c("Histogram - harvested area"))  

FAOSTATmz4$Year[which(abs(MTRelD)>15)]
MTtotal$Year[which(abs(MTRelD)>15)]

FAOSTATmz4$Year[which(abs(MTRelD)>30)]
MTtotal$Year[which(abs(MTRelD)>30)]

FAOSTATmz4$Year[which(abs(MTRelD)>50)]
MTtotal$Year[which(abs(MTRelD)>50)]
MTRelD[which(abs(MTRelD)>50)]

MTtotal$Year[which(abs(MTRelD)>100)]
MTtotal$Year[which(abs(MTRelD)>200)]

#-----------

FAOSTATmz4$Year[which(abs(AreaRelD)>15)]
AreaTotal$Year[which(abs(AreaRelD)>15)]

FAOSTATmz4$Year[which(abs(AreaRelD)>50)]
AreaTotal$Year[which(abs(AreaRelD)>50)]

FAOSTATmz4$Year[which(abs(AreaRelD)>100)]
AreaTotal$Year[which(abs(AreaRelD)>100)]
AreaRelD[which(abs(AreaRelD)>100)]


FAOSTATmz4$Year[which(abs(AreaRelD)>200)]
AreaTotal$Year[which(abs(AreaRelD)>200)]

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# now some checking
FAOSTATmz4$Area[FAOSTATmz4$Year==1996] # FAOSTAT
sum(CrMaize14ts$Area[CrMaize14ts$Year==1996])  # FEWSNET
# (FAOSTAT-FEWSNET)/FEWSNET
(1489000- 171335)/ 171335
AreaRelD[FAOSTATmz4$Year==1996]

# GOOD. I HAVE ALSO DONE CHECKING OF MT (PRODUCTION) BUT I HAVE DELETED IT...
#-----------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                  now YIELDS

rm(YieldTotal)
YieldTotal<-MTtotal
YieldTotal$MT<-MTtotal$MT/AreaTotal$Area

names(YieldTotal)[1]<-"Year"
names(YieldTotal)[2]<-"Yield"

# checking
YieldTotal[YieldTotal$Year==1973,]

1353170/666341

#----------------------------------------------------------------------------------------------------
plot(YieldTotal$Yield,FAOSTATmz4$Yield/10000,xlab="FEWSNET",ylab="FAOSTAT",main="Yield (MT/Hectares)")

cor.test(MTtotal$MT,FAOSTATmz4$Production)
cor.test(AreaTotal$Area,FAOSTATmz4$Area)
cor.test(YieldTotal$Yield,FAOSTATmz4$Yield/10000)


YieldRelD<-100*(FAOSTATmz4$Yield/10000-YieldTotal$Yield)/YieldTotal$Yield

summary(YieldRelD)
summary(abs(YieldRelD))


hist(YieldRelD)
hist(YieldRelD,20,xlab=c("Difference between FAOSTAT and FEWSNET data in %"),main=c("Histogram - yield"))  

# pokus, jestli kdyz to spoctu opacne, budou rozdily vetsi??  -->> ANO

YieldRelD2<-100*(YieldTotal$Yield-FAOSTATmz4$Yield/10000)/(FAOSTATmz4$Yield/10000)
summary(YieldRelD2)
hist(YieldRelD2)
hist(YieldRelD2,35)  #-->> ANO
#-----------------------------------------------------------


FAOSTATmz4$Year[which(abs(YieldRelD)>15)]
YieldTotal$Year[which(abs(YieldRelD)>15)]

FAOSTATmz4$Year[which(abs(YieldRelD)>20)]
YieldTotal$Year[which(abs(YieldRelD)>20)]

FAOSTATmz4$Year[which(abs(YieldRelD)>30)]
YieldTotal$Year[which(abs(YieldRelD)>30)]

FAOSTATmz4$Year[which(abs(YieldRelD)>35)]
YieldTotal$Year[which(abs(YieldRelD)>35)]
YieldRelD[which(abs(YieldRelD)>35)]

YieldTotal$Year[which(abs(YieldRelD)>33.5)]
YieldRelD[which(abs(YieldRelD)>33.5)]

FAOSTATmz4$Year[which(abs(YieldRelD2)>50)]
YieldTotal$Year[which(abs(YieldRelD2)>50)]



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# now some checking
FAOSTATmz4$Yield[FAOSTATmz4$Year==2010] # FAOSTAT
sum(CrMaize14ts$Yield[CrMaize14ts$Year==2010])  # FEWSNET
# (FAOSTAT-FEWSNET)/FEWSNET
(17.251- 69.17)/69.17
YieldRelD[FAOSTATmz4$Year==2010]   #YEPPP