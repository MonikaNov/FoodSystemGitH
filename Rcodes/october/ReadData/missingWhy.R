# rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma
library('dplyr')
library('purrr')
library("reshape")
setwd(WDuni)
setwd(WDhome)

load("Main/dataOctober.RData")
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

table(dataAll$Year[which(is.na(dataAll$TempAvgMxFebruary)==TRUE)])
table(dataAll$Year[which(is.na(dataAll$TempAvgMxFebruary)==TRUE)])


table(dataAll$ADM2_NAME[which(is.na(dataAll$TempAvgMxFebruary)==TRUE)])
table(dataAll$code[which(is.na(dataAll$TempAvgMxFebruary)==TRUE)])


msYears<-lapply(dataAll[96:155], function(x) {table(dataAll$Year[which(is.na(x)==TRUE)])})
msYears2<-sapply(dataAll[96:155], function(x) {table(dataAll$Year[which(is.na(x)==TRUE)])})
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooo

varI<-c(seq(5,148),seq(156,179) )
rm(myNAS)
myNAS<- lapply(varI, function(x) {return(list(name=names(dataAll)[x], 
                                              YrsNA=table(dataAll[ is.na(dataAll[x]),"Year"] ),
                                              cntyNA=table(dataAll[ is.na(dataAll[x]),"ADM2_NAME"]),
                                               cntyNA=table(dataAll[ is.na(dataAll[x]),"code"] )))    } )

myNAS[[1]]
myNAS[[2]]
#and. so

names(myNAS)[[1]]<-myNAS[[1]]$name

names(myNAS)<-sapply(1:168, function(x) {myNAS[[x]]$name}    )

names(myNAS)

myNAS$TempAvgMxApril
myNAS$TempAvgMxAugust

# C*O*E*L

myNAS[61:90]