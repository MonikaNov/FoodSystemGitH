rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
# WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
# setwd(WDhome)
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
source("/its/home/mn301/foodSystems/Rcodes/NewClimateSept/DataReading.R")
setwd(WDuni)

# rename the years to make them compatible with the yields

TempOctMar<-lapply(TempOctMar,  function(x)  {names(x)[2:34]<- substr(names(x)[2:34],7,10); return(x)})
TempMarSep<-lapply(TempMarSep,  function(x)  {names(x)[2:35]<- substr(names(x)[2:35],2,5); return(x)})
PrecMAM<-lapply(PrecMAM,  function(x)  {names(x)[2:38]<- substr(names(x)[2:38],2,5);return(x)})
PrecOND<-lapply(PrecOND,  function(x)  {names(x)[2:38]<- substr(names(x)[2:38],2,5);return(x)})

# seems that tha last year for prec is 2017. But I have to cut it at 2013 as this is the last year for yields and temeprature

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# now, I need match on the strange ID in the new climnate data set.. merge...

IDdict<-read.csv("ClimateAggregM/IDdict.csv")

PrecMAM2<-lapply(PrecMAM, function(xx) merge(xx,IDdict, all.x=TRUE))
PrecOND2<-lapply(PrecOND, function(xx) merge(xx,IDdict, all.x=TRUE))
TempMarSep2<-lapply(TempMarSep, function(xx) merge(xx,IDdict, all.x=TRUE))
TempOctMar2<-lapply(TempOctMar, function(xx) merge(xx,IDdict, all.x=TRUE))
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#checking/testing

lapply(PrecMAM2, function(xx) which(is.na(xx$ID1)==TRUE))
lapply(PrecMAM2, function(xx) length(unique(xx$ID1)))
lapply(PrecMAM2, summary)

lapply(PrecOND2, function(xx) which(is.na(xx$ID1)==TRUE))
lapply(PrecOND2, function(xx) length(unique(xx$ID1)))
lapply(PrecOND2, summary)

lapply(TempMarSep2, function(xx) which(is.na(xx$ID1)==TRUE))
lapply(TempMarSep2, function(xx) length(unique(xx$ID1)))
lapply(TempMarSep2, summary)

lapply(TempOctMar2, function(xx) which(is.na(xx$ID1)==TRUE))
lapply(TempOctMar2, function(xx) length(unique(xx$ID1)))
lapply(TempOctMar2, summary)

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# now merging

library("reshape")

PrecMAM3<-lapply(PrecMAM2, function(xx) melt(xx,id.vars=c("code","ADM2_NAME","ID1")))
PrecOND3<-lapply(PrecOND2, function(xx) melt(xx,id.vars=c("code","ADM2_NAME","ID1")))
TempMarSep3<-lapply(TempMarSep2, function(xx) melt(xx,id.vars=c("code","ADM2_NAME","ID1")))
TempOctMar3<-lapply(TempOctMar2, function(xx) melt(xx,id.vars=c("code","ADM2_NAME","ID1")))

# now put together ASAL and non-ASAL

PrecMAM3[1:8]<-lapply( PrecMAM3[1:8], function(x){ x$ASAL=1; return(x)} ) 
PrecMAM3[9:16]<-lapply( PrecMAM3[9:16], function(x){ x$ASAL=0; return(x)} ) 
PrecOND3[1:8]<-lapply( PrecOND3[1:8], function(x){ x$ASAL=1; return(x)} ) 
PrecOND3[9:16]<-lapply( PrecOND3[9:16], function(x){ x$ASAL=0; return(x)} ) 

TempMarSep3[1:5]<-lapply( TempMarSep3[1:5], function(x){ x$ASAL=1; return(x)} ) 
TempMarSep3[6:10]<-lapply( TempMarSep3[6:10], function(x){ x$ASAL=0; return(x)} ) 
TempOctMar3[1:5]<-lapply( TempOctMar3[1:5], function(x){ x$ASAL=1; return(x)} ) 
TempOctMar3[6:10]<-lapply( TempOctMar3[6:10], function(x){ x$ASAL=0; return(x)} ) 

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#now merge ASAL and non-ASAL

PrecMAM4<-lapply(1:8,  function(x)    rbind(PrecMAM3[[x]],PrecMAM3[[x+8]])   )
names<-c("PrecCoefVar_MAM","DrSpell10_MAM","DrSpell20_MAM","DrySpell_MAM","MaxRain_MAM","SeasRain_MAM","PrecStDev_MAM","Prec2Months_MAM")
PrecMAM4<-   lapply (PrecMAM4, function(x){ names(x)[4] ="Year";return(x)} )
rm(PrecMAM5); PrecMAM5<-   mapply (   FUN=function(  x,y)    { names(x)[5] =y;return(x)}   ,x=PrecMAM4,y= names,SIMPLIFY = FALSE )

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

PrecOND4<-lapply(1:8,  function(x)    rbind(PrecOND3[[x]],PrecOND3[[x+8]])   )
names2<-c("PrecCoefVar_OND","DrSpell10_OND","DrSpell20_OND","DrySpell_OND","MaxRain_OND","SeasRain_OND","PrecStDev_OND","Prec2Months_OND")
PrecOND4<-   lapply (PrecOND4, function(x){ names(x)[4] ="Year";return(x)} )
rm(PrecOND5); PrecOND5<-   mapply (   FUN=function(  x,y)    { names(x)[5] =y;return(x)}   ,x=PrecOND4,y= names2,SIMPLIFY = FALSE )

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

TempMarSep4<-lapply(1:5,  function(x)    rbind(TempMarSep3[[x]],TempMarSep3[[x+5]])   )
names3<-c("MaxTemp_MarSep","AvgTemp_MarSep","CumDD_MarSep","HeatWDays_MarSep","SDtemp_MarSep")
TempMarSep4<-   lapply (TempMarSep4, function(x){ names(x)[4] ="Year";return(x)} )
rm(TempMarSep5); TempMarSep5<-   mapply (   FUN=function(  x,y)    { names(x)[5] =y;return(x)}   ,x=TempMarSep4,y= names3,SIMPLIFY = FALSE )

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

TempOctMar4<-lapply(1:5,  function(x)    rbind(TempOctMar3[[x]],TempOctMar3[[x+5]])   )
names4<-c("MaxTemp_OctMar","AvgTemp_OctMar","CumDD_OctMar","HeatWDays_OctMar","SDtemp_OctMar")

TempOctMar4<-   lapply (TempOctMar4, function(x){ names(x)[4] ="Year";return(x)} )
rm(TempOctMar5); TempOctMar5<-   mapply (   FUN=function(  x,y)    { names(x)[5] =y;return(x)}   ,x=TempOctMar4,y= names4,SIMPLIFY = FALSE )
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

rm(list=setdiff(ls(),c("PrecMAM5","PrecOND5","TempMarSep5","TempOctMar5")))
library(purrr)
tOctMar<-reduce(TempOctMar5, merge)
tMarSep<-reduce(TempMarSep5, merge)
pOND<-reduce(PrecOND5, merge)
pMAM<-reduce(PrecMAM5, merge)

tOctMar<-droplevels(subset(tOctMar,Year%in%(1981:2014)))
tMarSep<-droplevels(subset(tMarSep,Year%in%(1981:2014)))
pOND<-droplevels(subset(pOND,Year%in%(1981:2014)))
pMAM<-droplevels(subset(pMAM,Year%in%(1981:2014)))

climateAll<-reduce(list(tOctMar,tMarSep,pOND,pMAM), function(a,b) merge(a,b,all=TRUE) )

nrow(unique(climateAll[c("code","ASAL","Year")]))
nrow(unique(pMAM[c("code","ASAL","Year")]))
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# now merge with Maize

load("Main/CrMaize15.RData") # the last (and the best available) dataset before I cut it timewise..
rm(CrMaize20)
CrMaize20<-subset(CrMaize15,Year %in% (1981:2014),select=c("Admin1","county","Admin2","Year","Area","Yield","MT","ID","west1"))
names(CrMaize20)[8]<-"ID1"

intersect(names(CrMaize20),names(climateAll))
MaizeClimate<-merge(climateAll,CrMaize20,all.x=TRUE)
MaizeClimate<-MaizeClimate[order(MaizeClimate$ID1,MaizeClimate$Year ),]

rm(list=setdiff(ls(),"MaizeClimate"))

# save.image("~/foodSystems/dataFS/Main/MaizeClimate.RData")