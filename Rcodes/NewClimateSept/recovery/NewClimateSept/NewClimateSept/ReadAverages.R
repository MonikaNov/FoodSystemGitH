# rm(list=ls())

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

# get rid of the total averages, I t hink
Temp2<-lapply(Temp2, function(xx)  {xx=xx[-36];return(xx)})
Prec2<-lapply(Prec2, function(xx)  {xx=xx[-39];return(xx)})
library("reshape")

Prec3<-lapply(Prec2, function(xx) melt(xx,id.vars=c("code","ADM2_NAME","ID1")))
Temp3<-lapply(Temp2, function(xx) melt(xx,id.vars=c("code","ADM2_NAME","ID1")))

Prec3<-   lapply (Prec3, function(x){ names(x)[4] ="Year";return(x)} )
Temp3<-   lapply (Temp3, function(x){ names(x)[4] ="Year";return(x)} )


names<-c("April","August","December","February","January","July","June","March","May","November","October","September")


Temp3<-   mapply ( function(x,y){ names(x)[5] =y;return(x)}, Temp3, names,SIMPLIFY=FALSE )
Prec3<-   mapply ( function(x,y){ names(x)[5] =y;return(x)}, Prec3, names,SIMPLIFY=FALSE )

lapply(Temp3,dim)
Temp4<-reduce(Temp3, merge )
Prec4<-reduce(Prec3, merge )

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

rm(list=setdiff(ls(), c("Temp4","Prec4","MaizeClimate")))

# save.image("~/foodSystems/dataFS/Main/climateAverages.RData")