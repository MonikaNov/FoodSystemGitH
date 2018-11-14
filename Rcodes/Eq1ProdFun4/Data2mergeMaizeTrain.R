rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

source("/its/home/mn301/foodSystems/Rcodes/Eq1ProdFun4/Data1ClimateShape.R")

# or just load the data:

load("Main/climate13.RData")
load("Main/CrMaize14.RData")

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# first I need to separate counties into eastern and western

zones<-read.csv(file='zonesHarvest/WestEast.csv')
CrMaize15<-merge(CrMaize14,zones,all.x=TRUE,sort=FALSE)
CrMaize15<-CrMaize15[order(CrMaize15["ID"],CrMaize15["Year"]),]

CrMaize16<-droplevels(subset(CrMaize15, Year %in% 2000:2014, select=c("ID","county","Year","Yield","MT","west1")))

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

CrMaize16$PreMean<-NA
CrMaize16$PreMeanCV<-NA
CrMaize16$PreMed<-NA
CrMaize16$PreMedCV<-NA

CrMaize16$PreMeanZ<-NA
CrMaize16$PreMeanCVz<-NA
CrMaize16$PreMedZ<-NA
CrMaize16$PreMedCVz<-NA


CrMaize16$TemMean<-NA
CrMaize16$TemMeanCV<-NA
CrMaize16$TemMed<-NA
CrMaize16$TemMedCV<-NA

CrMaize16$TemMeanZ<-NA
CrMaize16$TemMeanCVz<-NA
CrMaize16$TemMedZ<-NA
CrMaize16$TemMedCVz<-NA


#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

# ------------------------------------------------------------           West                  -------------------------------- -------       ------------------------------------ ------            ----------          --------------------------- -----------------------

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

# season March, April, May and June, July, August of the current year

for (i in unique(CrMaize16[CrMaize16$west1==1,]$ID))
{
  #****************************       A.  PRECIPIPTATION       ************************
  
# 1. Precipitation mean
  
  CrMaize16$PreMean[CrMaize16$ID==i] <-sapply(        unique( as.numeric(as.character(CrMaize16$Year[CrMaize16$ID==i])) ), function (j)
  { CrMaize16$PreMean[CrMaize16$ID==i & CrMaize16$Year==j]<-mean(climate13[[1]][climate13[[1]]$ID==i & climate13[[1]]$Year==j,5][c(3:8)]  )})

  CrMaize16$PreMeanCV[CrMaize16$ID==i] <-sapply( unique( as.numeric(as.character(CrMaize16$Year[CrMaize16$ID==i])) ), function (j)
  {CrMaize16$PreMeanCV[CrMaize16$ID==i & CrMaize16$Year==j]<-sd(  climate13[[1]][climate13[[1]]$ID==i & climate13[[1]]$Year==j,5][c(3:8)]  )/ mean( climate13[[1]][climate13[[1]]$ID==i & climate13[[1]]$Year==j,5][c(3:8)]  )
  })
  
# 2. Precipitation median
  
  CrMaize16$PreMed[CrMaize16$ID==i] <-sapply(        unique( as.numeric(as.character(CrMaize16$Year[CrMaize16$ID==i])) ), function (j)
  { CrMaize16$PreMed[CrMaize16$ID==i & CrMaize16$Year==j]<-mean(climate13[[2]][climate13[[2]]$ID==i & climate13[[2]]$Year==j,5][c(3:8)]     )})
  
  CrMaize16$PreMedCV[CrMaize16$ID==i] <-sapply( unique( as.numeric(as.character(CrMaize16$Year[CrMaize16$ID==i])) ), function (j)
  {CrMaize16$PreMedCV[CrMaize16$ID==i & CrMaize16$Year==j]<-sd( climate13[[2]][climate13[[2]]$ID==i & climate13[[2]]$Year==j,5][c(3:8)]   )/ mean(climate13[[2]][climate13[[2]]$ID==i & climate13[[2]]$Year==j,5][c(3:8)]  )
  })
  
# 3. Precipitation mean Z-score
  
  CrMaize16$PreMeanZ[CrMaize16$ID==i] <-sapply(        unique( as.numeric(as.character(CrMaize16$Year[CrMaize16$ID==i])) ), function (j)
  { CrMaize16$PreMeanZ[CrMaize16$ID==i & CrMaize16$Year==j]<-mean( climate13[[3]][climate13[[3]]$ID==i & climate13[[3]]$Year==j,5][c(3:8)]     )})
  
  CrMaize16$PreMeanCVz[CrMaize16$ID==i] <-sapply( unique( as.numeric(as.character(CrMaize16$Year[CrMaize16$ID==i])) ), function (j)
  {CrMaize16$PreMeanCVz[CrMaize16$ID==i & CrMaize16$Year==j]<-sd( climate13[[3]][climate13[[3]]$ID==i & climate13[[3]]$Year==j,5][c(3:8)]   )  / mean(climate13[[3]][climate13[[3]]$ID==i & climate13[[3]]$Year==j,5][c(3:8)]  )
  })
  
# 4. Precipitation median Z-score
  
  CrMaize16$PreMedZ[CrMaize16$ID==i] <-sapply(        unique( as.numeric(as.character(CrMaize16$Year[CrMaize16$ID==i])) ), function (j)
  { CrMaize16$PreMedZ[CrMaize16$ID==i & CrMaize16$Year==j]<-mean(climate13[[4]][climate13[[4]]$ID==i & climate13[[4]]$Year==j,5][c(3:8)]     )})
  
  CrMaize16$PreMedCVz[CrMaize16$ID==i] <-sapply( unique( as.numeric(as.character(CrMaize16$Year[CrMaize16$ID==i])) ), function (j)
  {CrMaize16$PreMedCVz[CrMaize16$ID==i & CrMaize16$Year==j]<-sd(   climate13[[4]][climate13[[4]]$ID==i & climate13[[4]]$Year==j,5][c(3:8)]   )/ mean(  climate13[[4]][climate13[[4]]$ID==i & climate13[[4]]$Year==j,5][c(3:8)]  )
  })

  #****************************       B.  TEMPERATURE       ************************
  
# 5. Temperature mean
  
  CrMaize16$TemMean[CrMaize16$ID==i] <-sapply(        unique( as.numeric(as.character(CrMaize16$Year[CrMaize16$ID==i])) ), function (j)
  { CrMaize16$TemMean[CrMaize16$ID==i & CrMaize16$Year==j]<-mean(climate13[[5]][climate13[[5]]$ID==i & climate13[[5]]$Year==j,5][c(3:8)]    )})
  
  CrMaize16$TemMeanCV[CrMaize16$ID==i] <-sapply( unique( as.numeric(as.character(CrMaize16$Year[CrMaize16$ID==i])) ), function (j)
  {CrMaize16$TemMeanCV[CrMaize16$ID==i & CrMaize16$Year==j]<-sd(climate13[[5]][climate13[[5]]$ID==i & climate13[[5]]$Year==j,5][c(3:8)]   )/ mean(   climate13[[5]][climate13[[5]]$ID==i & climate13[[5]]$Year==j,5][c(3:8)]) 
  })
  
# 6. Temperature median
  
  CrMaize16$TemMed[CrMaize16$ID==i] <-sapply(        unique( as.numeric(as.character(CrMaize16$Year[CrMaize16$ID==i])) ), function (j)
  { CrMaize16$TemMed[CrMaize16$ID==i & CrMaize16$Year==j]<-mean(climate13[[6]][climate13[[6]]$ID==i & climate13[[6]]$Year==j,5][c(3:8)]   )})
  
  CrMaize16$TemMedCV[CrMaize16$ID==i] <-sapply( unique( as.numeric(as.character(CrMaize16$Year[CrMaize16$ID==i])) ), function (j)
  {CrMaize16$TemMedCV[CrMaize16$ID==i & CrMaize16$Year==j]<-sd( climate13[[6]][climate13[[6]]$ID==i & climate13[[6]]$Year==j,5][c(3:8)]    )/ mean(climate13[[6]][climate13[[6]]$ID==i & climate13[[6]]$Year==j,5][c(3:8)]  )
  })
  
# 7. Temperature mean Z-score
  
  CrMaize16$TemMeanZ[CrMaize16$ID==i] <-sapply(        unique( as.numeric(as.character(CrMaize16$Year[CrMaize16$ID==i])) ), function (j)
  { CrMaize16$TemMeanZ[CrMaize16$ID==i & CrMaize16$Year==j]<-mean(climate13[[7]][climate13[[7]]$ID==i & climate13[[7]]$Year==j,5][c(3:8)]   )})
  
  CrMaize16$TemMeanCVz[CrMaize16$ID==i] <-sapply( unique( as.numeric(as.character(CrMaize16$Year[CrMaize16$ID==i])) ), function (j)
  {CrMaize16$TemMeanCVz[CrMaize16$ID==i & CrMaize16$Year==j]<-sd( climate13[[7]][climate13[[7]]$ID==i & climate13[[7]]$Year==j,5][c(3:8)]     )/ mean(climate13[[7]][climate13[[7]]$ID==i & climate13[[7]]$Year==j,5][c(3:8)])  
  })
  
# 8. Temperature median Z-score
  
  CrMaize16$TemMedZ[CrMaize16$ID==i] <-sapply(        unique( as.numeric(as.character(CrMaize16$Year[CrMaize16$ID==i])) ), function (j)
  { CrMaize16$TemMedZ[CrMaize16$ID==i & CrMaize16$Year==j]<-mean(  climate13[[8]][climate13[[8]]$ID==i & climate13[[8]]$Year==j,5][c(3:8)]     )})
  
  CrMaize16$TemMedCVz[CrMaize16$ID==i] <-sapply( unique( as.numeric(as.character(CrMaize16$Year[CrMaize16$ID==i])) ), function (j)
  {CrMaize16$TemMedCVz[CrMaize16$ID==i & CrMaize16$Year==j]<-sd(  climate13[[8]][climate13[[8]]$ID==i & climate13[[8]]$Year==j,5][c(3:8)]    )/ mean(climate13[[8]][climate13[[8]]$ID==i & climate13[[8]]$Year==j,5][c(3:8)]  )
  })
  
}
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo


#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

# ------------------------------------------------------------           East                  -------------------------------- -------       ------------------------------------ ------            ----------          --------------------------- -----------------------

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

# Season October, November, December previous year and March, April, May current year

for (i in unique(CrMaize16[CrMaize16$west1==0,]$ID))
{
#****************************       A.  PRECIPIPTATION       ************************
  
# 1. Precipitation mean
  
  CrMaize16$PreMean[CrMaize16$ID==i] <-sapply(        unique( as.numeric(as.character(CrMaize16$Year[CrMaize16$ID==i])) ), function (j)
  { CrMaize16$PreMean[CrMaize16$ID==i & CrMaize16$Year==j]<-mean(c  (climate13[[1]][climate13[[1]]$ID==i & climate13[[1]]$Year==(j-1),5][c(10:12)]   ,climate13[[1]][climate13[[1]]$ID==i & climate13[[1]]$Year==j,5][c(3:5)]   )  )})

  CrMaize16$PreMeanCV[CrMaize16$ID==i] <-sapply( unique( as.numeric(as.character(CrMaize16$Year[CrMaize16$ID==i])) ), function (j)
  {CrMaize16$PreMeanCV[CrMaize16$ID==i & CrMaize16$Year==j]<-sd(   c(climate13[[1]][climate13[[1]]$ID==i & climate13[[1]]$Year==(j-1),5][c(10:12)]   ,climate13[[1]][climate13[[1]]$ID==i & climate13[[1]]$Year==j,5][c(3:5)]   )  )/ mean(c(climate13[[1]][climate13[[1]]$ID==i & climate13[[1]]$Year==(j-1),5][c(10:12)]   ,climate13[[1]][climate13[[1]]$ID==i & climate13[[1]]$Year==j,5][c(3:5)])  )
  })
  
# 2. Precipitation median
  
  CrMaize16$PreMed[CrMaize16$ID==i] <-sapply(        unique( as.numeric(as.character(CrMaize16$Year[CrMaize16$ID==i])) ), function (j)
  { CrMaize16$PreMed[CrMaize16$ID==i & CrMaize16$Year==j]<-mean(c  (climate13[[2]][climate13[[2]]$ID==i & climate13[[2]]$Year==(j-1),5][c(10:12)]   ,climate13[[2]][climate13[[2]]$ID==i & climate13[[2]]$Year==j,5][c(3:5)]   )  )})
  
  CrMaize16$PreMedCV[CrMaize16$ID==i] <-sapply( unique( as.numeric(as.character(CrMaize16$Year[CrMaize16$ID==i])) ), function (j)
  {CrMaize16$PreMedCV[CrMaize16$ID==i & CrMaize16$Year==j]<-sd(   c(climate13[[2]][climate13[[2]]$ID==i & climate13[[2]]$Year==(j-1),5][c(10:12)]   ,climate13[[2]][climate13[[2]]$ID==i & climate13[[2]]$Year==j,5][c(3:5)]   )  )/ mean(c(climate13[[2]][climate13[[2]]$ID==i & climate13[[2]]$Year==(j-1),5][c(10:12)]   ,climate13[[2]][climate13[[2]]$ID==i & climate13[[2]]$Year==j,5][c(3:5)])  )
  })
  
# 3. Precipitation mean Z-score
  
  CrMaize16$PreMeanZ[CrMaize16$ID==i] <-sapply(        unique( as.numeric(as.character(CrMaize16$Year[CrMaize16$ID==i])) ), function (j)
  { CrMaize16$PreMeanZ[CrMaize16$ID==i & CrMaize16$Year==j]<-mean(c  (climate13[[3]][climate13[[3]]$ID==i & climate13[[3]]$Year==(j-1),5][c(10:12)]   ,climate13[[3]][climate13[[3]]$ID==i & climate13[[3]]$Year==j,5][c(3:5)]   )  )})
  
  CrMaize16$PreMeanCVz[CrMaize16$ID==i] <-sapply( unique( as.numeric(as.character(CrMaize16$Year[CrMaize16$ID==i])) ), function (j)
  {CrMaize16$PreMeanCVz[CrMaize16$ID==i & CrMaize16$Year==j]<-sd(   c(climate13[[3]][climate13[[3]]$ID==i & climate13[[3]]$Year==(j-1),5][c(10:12)]   ,climate13[[3]][climate13[[3]]$ID==i & climate13[[3]]$Year==j,5][c(3:5)]   )  )/ mean(c(climate13[[3]][climate13[[3]]$ID==i & climate13[[3]]$Year==(j-1),5][c(10:12)]   ,climate13[[3]][climate13[[3]]$ID==i & climate13[[3]]$Year==j,5][c(3:5)])  )
  })
  
# 4. Precipitation median Z-score
  
  CrMaize16$PreMedZ[CrMaize16$ID==i] <-sapply(        unique( as.numeric(as.character(CrMaize16$Year[CrMaize16$ID==i])) ), function (j)
  { CrMaize16$PreMedZ[CrMaize16$ID==i & CrMaize16$Year==j]<-mean(c  (climate13[[4]][climate13[[4]]$ID==i & climate13[[4]]$Year==(j-1),5][c(10:12)]   ,climate13[[4]][climate13[[4]]$ID==i & climate13[[4]]$Year==j,5][c(3:5)]   )  )})
  
  CrMaize16$PreMedCVz[CrMaize16$ID==i] <-sapply( unique( as.numeric(as.character(CrMaize16$Year[CrMaize16$ID==i])) ), function (j)
  {CrMaize16$PreMedCVz[CrMaize16$ID==i & CrMaize16$Year==j]<-sd(   c(climate13[[4]][climate13[[4]]$ID==i & climate13[[4]]$Year==(j-1),5][c(10:12)]   ,climate13[[4]][climate13[[4]]$ID==i & climate13[[4]]$Year==j,5][c(3:5)]   )  )/ mean(c(climate13[[4]][climate13[[4]]$ID==i & climate13[[4]]$Year==(j-1),5][c(10:12)]   ,climate13[[4]][climate13[[4]]$ID==i & climate13[[4]]$Year==j,5][c(3:5)])  )
  })

#****************************       B.  TEMPERATURE       ************************
  
# 5. Temperature mean
  
  CrMaize16$TemMean[CrMaize16$ID==i] <-sapply(        unique( as.numeric(as.character(CrMaize16$Year[CrMaize16$ID==i])) ), function (j)
  { CrMaize16$TemMean[CrMaize16$ID==i & CrMaize16$Year==j]<-mean(c  (climate13[[5]][climate13[[5]]$ID==i & climate13[[5]]$Year==(j-1),5][c(10:12)]   ,climate13[[5]][climate13[[5]]$ID==i & climate13[[5]]$Year==j,5][c(3:5)]   )  )})
  
  CrMaize16$TemMeanCV[CrMaize16$ID==i] <-sapply( unique( as.numeric(as.character(CrMaize16$Year[CrMaize16$ID==i])) ), function (j)
  {CrMaize16$TemMeanCV[CrMaize16$ID==i & CrMaize16$Year==j]<-sd(   c(climate13[[5]][climate13[[5]]$ID==i & climate13[[5]]$Year==(j-1),5][c(10:12)]   ,climate13[[5]][climate13[[5]]$ID==i & climate13[[5]]$Year==j,5][c(3:5)]   )  )/ mean(c(climate13[[5]][climate13[[5]]$ID==i & climate13[[5]]$Year==(j-1),5][c(10:12)]   ,climate13[[5]][climate13[[5]]$ID==i & climate13[[5]]$Year==j,5][c(3:5)])  )
  })
  
# 6. Temperature median
  
  CrMaize16$TemMed[CrMaize16$ID==i] <-sapply(        unique( as.numeric(as.character(CrMaize16$Year[CrMaize16$ID==i])) ), function (j)
  { CrMaize16$TemMed[CrMaize16$ID==i & CrMaize16$Year==j]<-mean(c  (climate13[[6]][climate13[[6]]$ID==i & climate13[[6]]$Year==(j-1),5][c(10:12)]   ,climate13[[6]][climate13[[6]]$ID==i & climate13[[6]]$Year==j,5][c(3:5)]   )  )})
  
  CrMaize16$TemMedCV[CrMaize16$ID==i] <-sapply( unique( as.numeric(as.character(CrMaize16$Year[CrMaize16$ID==i])) ), function (j)
  {CrMaize16$TemMedCV[CrMaize16$ID==i & CrMaize16$Year==j]<-sd(   c(climate13[[6]][climate13[[6]]$ID==i & climate13[[6]]$Year==(j-1),5][c(10:12)]   ,climate13[[6]][climate13[[6]]$ID==i & climate13[[6]]$Year==j,5][c(3:5)]   )  )/ mean(c(climate13[[6]][climate13[[6]]$ID==i & climate13[[6]]$Year==(j-1),5][c(10:12)]   ,climate13[[6]][climate13[[6]]$ID==i & climate13[[6]]$Year==j,5][c(3:5)])  )
  })
  
# 7. Temperature mean Z-score
  
  CrMaize16$TemMeanZ[CrMaize16$ID==i] <-sapply(        unique( as.numeric(as.character(CrMaize16$Year[CrMaize16$ID==i])) ), function (j)
  { CrMaize16$TemMeanZ[CrMaize16$ID==i & CrMaize16$Year==j]<-mean(c  (climate13[[7]][climate13[[7]]$ID==i & climate13[[7]]$Year==(j-1),5][c(10:12)]   ,climate13[[7]][climate13[[7]]$ID==i & climate13[[7]]$Year==j,5][c(3:5)]   )  )})
  
  CrMaize16$TemMeanCVz[CrMaize16$ID==i] <-sapply( unique( as.numeric(as.character(CrMaize16$Year[CrMaize16$ID==i])) ), function (j)
  {CrMaize16$TemMeanCVz[CrMaize16$ID==i & CrMaize16$Year==j]<-sd(   c(climate13[[7]][climate13[[7]]$ID==i & climate13[[7]]$Year==(j-1),5][c(10:12)]   ,climate13[[7]][climate13[[7]]$ID==i & climate13[[7]]$Year==j,5][c(3:5)]   )  )/ mean(c(climate13[[7]][climate13[[7]]$ID==i & climate13[[7]]$Year==(j-1),5][c(10:12)]   ,climate13[[7]][climate13[[7]]$ID==i & climate13[[7]]$Year==j,5][c(3:5)])  )
  })
  
# 8. Temperature median Z-score
  
  CrMaize16$TemMedZ[CrMaize16$ID==i] <-sapply(        unique( as.numeric(as.character(CrMaize16$Year[CrMaize16$ID==i])) ), function (j)
  { CrMaize16$TemMedZ[CrMaize16$ID==i & CrMaize16$Year==j]<-mean(c  (climate13[[8]][climate13[[8]]$ID==i & climate13[[8]]$Year==(j-1),5][c(10:12)]   ,climate13[[8]][climate13[[8]]$ID==i & climate13[[8]]$Year==j,5][c(3:5)]   )  )})
  
  CrMaize16$TemMedCVz[CrMaize16$ID==i] <-sapply( unique( as.numeric(as.character(CrMaize16$Year[CrMaize16$ID==i])) ), function (j)
  {CrMaize16$TemMedCVz[CrMaize16$ID==i & CrMaize16$Year==j]<-sd(   c(climate13[[8]][climate13[[8]]$ID==i & climate13[[8]]$Year==(j-1),5][c(10:12)]   ,climate13[[8]][climate13[[8]]$ID==i & climate13[[8]]$Year==j,5][c(3:5)]   )  )/ mean(c(climate13[[8]][climate13[[8]]$ID==i & climate13[[8]]$Year==(j-1),5][c(10:12)]   ,climate13[[8]][climate13[[8]]$ID==i & climate13[[8]]$Year==j,5][c(3:5)])  )
  })
  
}

# great, all checked and okkkkkkk loaded, merged (1.8.2018) and checked !!!
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# check Nandi (average of South and North)

for (i in 1:8)

{print(i)
  print(CrMaize16[CrMaize16["ID"]==80 & CrMaize16["Year"]==2001 ,2*i+5])
  print(mean(climate13[[i]][ climate13[[i]]["Year"] ==2001 &   climate13[[i]]$Month %in% 3:8 & climate13[[i]]$ID1 %in% 50:51  ,5]))
}



for (i in 1:8)
  
{print(i)
  sapply(2000:2014, function (j){
  print(j)  
  print(CrMaize16[CrMaize16["ID"]==80 & CrMaize16["Year"]==j ,2*i+5])
  print(mean(climate13[[i]][ climate13[[i]]["Year"] ==j &   climate13[[i]]$Month %in% 3:8 & climate13[[i]]$ID1 %in% 50:51  ,5]))
  })

}              #  GROOT




print(CrMaize16[CrMaize16["ID"]==80 & CrMaize16["Year"]==2001 ,])
print(mean(climate13[[7]][ climate13[[7]]["Year"] ==2001 &   climate13[[7]]$Month %in% 3:8 & climate13[[7]]$ID1 %in% 50:51  ,5]))

sapply(climate13, function(x) names(x[5]))
names(CrMaize16)[7:22]
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# now I need to check repeating numbers for the same months of the consecutive years
library(reshape)
library(reshape2)

test1<-subset(climate13[[1]],ID1==1)

test3<-cast(test1, Year~Month,value="PreMean")
test2<-cast(test1, Year~Month,value=names(test1)[5])

test4<-trunc(test2,2)

any(duplicated(test2[2]))
sapply(2:13,function(i) {any(duplicated(test2[i]))})


test2<-cast(test1, Year~Month,value=names(test1)[5])


names(test1)[5]<-"value"
test2<-cast(test1, Year~Month)
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo


rm(list=setdiff(ls(), "CrMaize16"))
save.image("Main/CrMaize16.RData")