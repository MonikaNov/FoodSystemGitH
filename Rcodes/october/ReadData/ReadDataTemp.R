# rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma
library('dplyr')
library('purrr')
library("reshape")
setwd(WDuni)
setwd(WDhome)

load("Main/dataOctober.RData")

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo


MyPath<-"/home/trennion/foodSystems/dataFS/climateOct/Kenya_stats/Temp"
#or
MyPath<-"/its/home/mn301/foodSystems/dataFS/climateOct/Kenya_stats/Temp"

rm(avgMax)
avgMax<- list.files(pattern="*avg_max_Temp.csv",recursive=TRUE,path= MyPath, full.names=FALSE)
avgMax
rm(avgTemp)
avgTemp<- list.files(pattern="*avg_temp.csv",recursive=TRUE,path= MyPath, full.names=FALSE)
avgTemp
rm(cumDd)
cumDd<- list.files(pattern="*cum_dd.csv",recursive=TRUE,path=MyPath, full.names=FALSE)
cumDd
rm(hwD)
hwD<- list.files(pattern="*hw_days.csv",recursive=TRUE,path= MyPath, full.names=FALSE)
hwD
rm(SDtemp)
SDtemp<- list.files(pattern="*sd_temp.csv",recursive=TRUE,path= MyPath, full.names=FALSE)
SDtemp
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
setwd("/home/trennion/foodSystems/dataFS/climateOct/Kenya_stats/Temp")    # or
setwd(MyPath)

pathss<-list(avgMax,avgTemp,cumDd,hwD,SDtemp)

listss<-lapply(pathss, function(i)  { lapply(i, function(x) read.csv2(x,na.strings="-999",dec="."))}   )
listss<-lapply(listss, function(i)  {lapply( seq(1,24,2) , function(x) rbind(i[[x]],i[[x+1]])  )})

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
setwd(WDhome)
#or 
setwd(WDuni)

IDdict<-read.csv("ClimateAggregM/IDdict.csv")
IDdict<-read.csv("IDdict.csv")

listss<-lapply(listss, function(i)  lapply(i, function(x) merge(x,IDdict, all.x=TRUE))  )
listss<-lapply(listss, function(i)  lapply(i, function(xx)  {xx=xx[-36];return(xx)})  )
listss<-lapply(listss, function(i)  lapply(i, function(xx) melt(xx,id.vars=c("code","ADM2_NAME","ID1")))  )
listss<-lapply(listss, function(i)  lapply(i, function(x){ names(x)[4] ="Year";return(x)} ) )

names<-c("April","August","December","February","January","July","June","March","May","November","October","September")
listss<-lapply(listss, function(i)  mapply(function(x,y){ names(x)[5] =y;return(x)}, i, names,SIMPLIFY=FALSE))
reduced<-lapply(listss, function(i)  reduce(i, merge ))
reduced<-lapply(reduced, function(i) {i[,"Year"]<-substr(i[,"Year"],2,5);return(i)})

varnames<-list("TempAvgMx","TempAvg","TempDD","TempHwDays","TempSD")


reduced<-lapply(1:5, function(xx) {names(reduced[[xx]])[5:16]<-paste0(varnames[xx],names(reduced[[xx]])[5:16]); return(reduced[[xx]])} )

all<-reduce(reduced, merge)

dataAll<-merge(dataAll,all,all.x=TRUE)
dataAll<-dataAll[order(dataAll$ID1,dataAll$Year ),]
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#testing and checking:

dim(unique(dataAll[c("Year","code")]))
table(dataAll$code,dataAll$west1)
aggregate(dataAll,by=list(dataAll$code),FUN="mean")
summary(dataAll)



View(dataAll[1006:1011,c(1:5,93,95,60:70)])
View(dataAll[300:3010,c(1:5,7:12)])

apply(dataAll, 2,function(x) sum(is.na(x)))

table(subset(dataAll,is.na(DrySpellApril)==TRUE)$Year)


sort(apply(dataAll, 2,function(x) sum(is.na(x))))
# checked7.10.2018: seems ok 
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

rm(list=setdiff(ls(), c("dataAll")))

# ok, seems to be loaded and checked and tested even at the uni..somehow..

# save.image("~/foodSystems/dataFS/Main/dataOctober.RData")