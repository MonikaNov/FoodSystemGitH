rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
# setwd(WDhome)
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# 1.Temp


rm(prec)
prec<- list.files( pattern="*seas_cumul.csv",recursive=TRUE,path= "/its/home/mn301/foodSystems/dataFS/ClimateAggregM/Kenya_monthly_stat", full.names=FALSE)
prec


rm(temp)
temp<- list.files( pattern="*.csv",recursive=TRUE,path= "/its/home/mn301/foodSystems/dataFS/ClimateAggregM/Kenya_monthly_stat/Temperature", 
                   full.names=FALSE)
temp

setwd("ClimateAggregM/Kenya_monthly_stat")
Precip<-lapply(prec, function(x) read.csv2(x,na.strings="-999",dec="."))
Prec2<-lapply( seq(1,24,2) , function(x)    rbind(Precip[[x]],Precip[[x+1]])   )

setwd("Temperature")
Tempe<-lapply(temp, function(x) read.csv2(x,na.strings="-999",dec="."))
Temp2<-lapply( seq(1,24,2) , function(x)    rbind(Tempe[[x]],Tempe[[x+1]])   )

setwd(WDuni)
IDdict<-read.csv("ClimateAggregM/IDdict.csv")

Prec2<-lapply(Prec2, function(xx) merge(xx,IDdict, all.x=TRUE))
Temp2<-lapply(Temp2, function(xx) merge(xx,IDdict, all.x=TRUE))

Prec2<-lapply(Prec2, function(xx) {colnames(xx)[2:38]  <-substr(colnames(xx)[2:38],2,5); return(xx)}  )  
Temp2<-lapply(Temp2, function(xx) {colnames(xx)[2:35]  <-substr(colnames(xx)[2:35],2,5); return(xx)}  )  


library("reshape")

PrecMAM3<-lapply(PrecMAM2, function(xx) melt(xx,id.vars=c("code","ADM2_NAME","ID1")))
PrecOND3<-lapply(PrecOND2, function(xx) melt(xx,id.vars=c("code","ADM2_NAME","ID1")))




rm(temp)
temp<- list.dirs( path= "/its/home/mn301/foodSystems/dataFS/ClimateAggregM/Temp", full.names=TRUE)

temp



rm(list=c("temp","TempMarSep"))

setwd("/its/home/mn301/foodSystems/dataFS/ClimateAggregM/Temp/Mar_to_Sept")
temp<- list.files(pattern="*.csv", full.names=TRUE)




setwd("/its/home/mn301/foodSystems/dataFS/ClimateAggregM/Temp")
temp2<- list.dir(pattern="/Mar_to_Sept/*.csv", full.names=TRUE)