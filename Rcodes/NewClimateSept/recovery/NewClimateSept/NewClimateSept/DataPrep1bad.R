rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

#  !!!!!!!!!!!!!!        carefull about the sets in environment. ond and mam have same names all files, so they do overwritte each other and couse confusion!!!!!!!!!!!!!!!!!!!!!!

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!


source("/its/home/mn301/foodSystems/Rcodes/NewClimateSept/DataReading.R")
# rename the years to make them compatible with the yields


TempOctMar<-lapply(TempOctMar,  function(x)  {names(x)[2:34]<- substr(names(x)[2:34],7,10)
                   return(x)})


names(TempOctMar[[1]])
View(TempOctMar[[1]])

TempMarSep<-lapply(TempMarSep,  function(x)  {names(x)[2:34]<- substr(names(x)[2:34],2,5)
return(x)})


PrecMAM<-lapply(PrecMAM,  function(x)  {names(x)[2:38]<- substr(names(x)[2:38],2,5)
return(x)})

PrecOND<-lapply(PrecOND,  function(x)  {names(x)[2:38]<- substr(names(x)[2:38],2,5)
return(x)})

# seems that tha last year for prec is 2017. But I have to cut it at 2013 as this is the last year for yields and temeprature

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# now, I need match on the strange ID in the new climnate data set.. merge...

IDdict<-read.csv("ClimateAggregM/IDdict.csv")
test1<-merge(PrecMAM[[1]],IDdict, all.x=TRUE)
which(is.na(test1$IDdict)==TRUE)
which(is.na(test1$ID1)==TRUE)
length(unique(test1$ID1))
which(is.na(test1$ADM2_NAME)==TRUE)

length(unique(test1$ADM2_NAME))
which(is.na(test1[,"1982"]==TRUE))
test2<-merge(PrecMAM[[9]],IDdict, all.x=TRUE)
which(is.na(test2$ID1)==TRUE)
length(unique(test2$ID1))
which(is.na(test2$ADM2_NAME)==TRUE)
length(unique(test2$ADM2_NAME))
which(is.na(test2[,"1981"]==TRUE))