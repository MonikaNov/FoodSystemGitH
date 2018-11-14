rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo


load("Main/climate13.RData")
load("Main/CrMaize16.RData")
library(dplyr)

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo


# 1.  Why is there much more locations in the climate dataset than in the maize dataset ??

names(climate13[[1]])
unique(climate13[[1]][c("ID1", "ADM2_NAME")])



lapply(climate13[2:8], function(x) all.equal(     unique(    climate13[[1]][c("ADM2_NAME","ID1")]) , unique(x[c("ADM2_NAME","ID1")]) ))

head(CrMaize16)
unique(CrMaize16[c("ID","county")])


setdiff(unique(climate13[[1]]$ID1),unique(CrMaize16$ID))  # no difference the other way around
setdiff(unique(climate13[[1]]$ADM2_NAME),unique(CrMaize16$county))
            setdiff(unique(CrMaize16$county),unique(climate13[[1]]$ADM2_NAME))
            sort(unique(climate13[[1]]$ADM2_NAME))  # so theses counties basically are in the climate as well, just some differences in spelling
            

            
DifID<- setdiff(unique(climate13[[1]]$ID1),unique(CrMaize16$ID))         

NotInMaize<-unique(climate13[[1]][climate13[[1]]$ID1 %in%DifID,  c("ID1","ADM2_NAME")])

    MaizeCounties2<-  unique(CrMaize16[c("ID","county")])  # so what IS in Maize
    MaizeCounties<- MaizeCounties[order(MaizeCounties$county  ),]
    
t(MaizeCounties)
NotInMaize

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

# Now look at the plots of Maize. Would it be helpful to remove some outliers?



library('plm')
library('dplyr')
library('tseries')
library('nlme')

rm(list=ls())

load("Main/CrMaize16.RData")

CrMaize16ts<-pdata.frame(CrMaize16,index=c("ID","Year"))

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

plot(Yield ~ Year, CrMaize16ts, mean)

#--------------------------------------------

means<-aggregate(CrMaize16ts$Yield ~ Year, CrMaize16ts, mean)

plot(means[,2],type="p",ylim=c(0,4),xaxt="n" ,xlab=NA)
axis(side = 1, at =seq(1, 15, by = 1), labels= seq(2000, 2014, by = 1),las=2)
sapply(   unique(   CrMaize16ts$ID ),      function (x) (points(Yield ~ Year, subset(CrMaize16ts, ID==x),pch="*")
                                                       ))

# nice
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

# now plots against FAOSTAT:

# first I need to put the faostat data and maize average together:


load("Main/CrMaize14.RData")
CrMaize14ts<-pdata.frame(CrMaize14,index=c("ID","Year"))

rm(list=c('FAOSTATmz','FAOSTATmz2','FAOSTATmz3'))
FAOSTATmz<-read.csv( "cropsMe/FAOSTAT_data_6-22-2018.csv",header=TRUE)
FAOSTATmz2<-cast(FAOSTATmz, Year~Element,value=c("Value"))
FAOSTATmz3<-merge(FAOSTATmz2,FAOSTATmzFlag,by=("Year"))
names(FAOSTATmz3)<-c("Year","Area","Production","Yield","AreaFlag","ProductionFlag","YieldFlag")
FAOSTATmz4<-subset(FAOSTATmz3, Year %in% c(1970:2014))
FAOSTATmz4$Yield<-FAOSTATmz4$Yield/10000

MTtotal<-aggregate(CrMaize14$MT, by=list(CrMaize14$Year), FUN=sum)
names(MTtotal)<-c("Year","MT")
AreaTotal<-aggregate(CrMaize14ts$Area, by=list(CrMaize14ts$Year), FUN=sum)
names(AreaTotal)<-c("Year","Area")
rm(YieldTotal)
YieldTotal<-MTtotal
YieldTotal$MT<-MTtotal$MT/AreaTotal$Area
names(YieldTotal)<-c("Year","Yield")

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# now the plots

# yields
plot(YieldTotal$Yield,FAOSTATmz4$Yield,xlab="FEWSNET",ylab="FAOSTAT",main="Yield (MT/Hectares)")

plot(YieldTotal$Yield[YieldTotal$Year %in% 2000:2014],FAOSTATmz4$Yield[FAOSTATmz4$Year %in% 2000:2014],
     xlab="FEWSNET",ylab="FAOSTAT",main="Yield (MT/Hectares)")

FAOSTATmz4[FAOSTATmz4$Yield>1.9&YieldTotal$Year %in% 2000:2014,]

    # so from the subset 2000-2014, the worst year is 2004   

# production
plot(MTtotal$MT,FAOSTATmz4$Production,xlab="FEWSNET",ylab="FAOSTAT",main="Production (MT)")
plot(MTtotal$MT[MTtotal$Year %in% 2000:2014],FAOSTATmz4$Production[FAOSTATmz4$Year %in% 2000:2014]
     ,xlab="FEWSNET",ylab="FAOSTAT",main="Production (MT)")

# area
plot(AreaTotal$Area,FAOSTATmz4$Area,xlab="FEWSNET",ylab="FAOSTAT",main="Area (Hectares)")
plot(AreaTotal$Area[AreaTotal$Year %in% 2000:2014],FAOSTATmz4$Area[FAOSTATmz4$Year %in% 2000:2014],
     xlab="FEWSNET",ylab="FAOSTAT",main="Area (Hectares)")

