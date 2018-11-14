rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
# setwd(WDhome)
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# 1.Temp

rm(list=c("temp","TempMarSep"))

setwd("/its/home/mn301/foodSystems/dataFS/ClimateAggregM/Temp/Mar_to_Sept")
temp<- list.files(pattern="*.csv")
TempMarSep<-lapply(temp, function(x) read.csv2(x,na.strings="-999",dec="."))

# View(TempMarSep[[1]])

#   list2env( lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))),   function(x) read.csv2(x,na.strings="-999",dec=".")       ), envir = .GlobalEnv)

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
rm(list=c("temp","TempOctMar"))

setwd("/its/home/mn301/foodSystems/dataFS/ClimateAggregM/Temp/Oct_to_Mar")
temp<- list.files(pattern="*.csv")
TempOctMar<-lapply(temp, function(x) read.csv2(x,na.strings="-999",dec="."))

# View(TempOctMar[[10]])

# list2env(lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))), function(x) read.csv2(x,na.strings="-999",dec=".")       ), envir = .GlobalEnv)

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# 2.Prec

rm(list=c("temp","PrecMAM"))

setwd("/its/home/mn301/foodSystems/dataFS/ClimateAggregM/Precip/MAM")
temp<- list.files(pattern="*.csv")
PrecMAM<-lapply(temp, function(x) read.csv2(x,na.strings="-999",dec="."))

# list2env(lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))), function(x) read.csv2(x,na.strings="-999",dec=".")       ), envir = .GlobalEnv)

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
rm(list=c("temp","PrecOND"))

setwd("/its/home/mn301/foodSystems/dataFS/ClimateAggregM/Precip/OND")
temp<- list.files(pattern="*.csv")
PrecOND<-lapply(temp, function(x) read.csv2(x,na.strings="-999",dec="."))

#list2env(lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))),  function(x) read.csv2(x,na.strings="-999",dec=".")       ), envir = .GlobalEnv)

