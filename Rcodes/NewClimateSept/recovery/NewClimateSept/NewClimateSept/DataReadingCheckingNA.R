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



summary(ASAL_cum_dd)
table(ASAL_cum_dd$code[is.na(ASAL_cum_dd)])

apply(as.matrix(ASAL_cum_dd), 2,function(x) sum(is.na(x))  )
apply(as.matrix(ASAL_cum_dd), 1,function(x) sum(is.na(x))  )






sapply       (TempMarSep,  function(y)  apply(as.matrix(y), 2,function(x) sum(is.na(x))  ))


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
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

sapply       (TempMarSep,  function(y)  apply(as.matrix(y), 2,function(x) sum(is.na(x))  ))
sapply       (TempOctMar,  function(y)  apply(as.matrix(y), 2,function(x) sum(is.na(x))  ))
sapply       (PrecMAM,  function(y)  apply(as.matrix(y), 2,function(x) sum(is.na(x))  ))
sapply       (PrecOND,  function(y)  apply(as.matrix(y), 2,function(x) sum(is.na(x))  ))

sapply(TempMarSep,function(x) sum(is.na(x)))
sapply(TempOctMar,function(x) sum(is.na(x)))
sapply(PrecMAM,function(x) sum(is.na(x)))
sapply(PrecOND,function(x) sum(is.na(x)))

setwd(WDuni)
load("Main/MaizeClimate.RData")

apply(as.matrix(MaizeClimate), 2,function(x) sum(is.na(x))  )

apply(as.matrix(MaizeClimate[6:31]),2, function(x) which(is.na(x))  )

apply(as.matrix(MaizeClimate[6:31]),2, function(x)  table(MaizeClimate$Year[ which(is.na(x))]  ))
               rowSums(apply(as.matrix(MaizeClimate[6:31]),2, function(x)  table(MaizeClimate$Year[ which(is.na(x))]  )))
               
apply(as.matrix(MaizeClimate[6:31]),2, function(x)  table(MaizeClimate$county[ which(is.na(x))]  ))
               rowSums(apply(as.matrix(MaizeClimate[6:31]),2, function(x)  table(MaizeClimate$county[ which(is.na(x))]  )))
               
               
sum(  complete.cases(  MaizeClimate[   c(  "SeasRain_OND" , "SeasRain_MAM" , "AvgTemp_MarSep", "AvgTemp_OctMar") ]    )  )      
sum(  complete.cases(  MaizeClimate[   c(  "SeasRain_OND" , "SeasRain_MAM" , "AvgTemp_MarSep","Prec2Months_OND" , "AvgTemp_OctMar") ]    )  )  
        ##  nrow(  MaizeClimate[complete.cases(  MaizeClimate[   c(  "Prec2Months_OND" , "DrSpell20_MAM" , "AvgTemp_MarSep", "AvgTemp_OctMar") ]    ) ,] )
sum(  complete.cases(  MaizeClimate[   c( "Yield","ID1", "Prec2Months_OND" , "DrSpell20_MAM" , "AvgTemp_MarSep", "AvgTemp_OctMar") ]    )  )   