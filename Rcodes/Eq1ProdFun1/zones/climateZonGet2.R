rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

load("~/foodSystems/dataFS/Main/CrMaize5avg.RData")
load("~/foodSystems/dataFS/Main/climate5.RData")
library(plm)

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
zones<-read.csv(file='/its/home/mn301/foodSystems/Rcodes/Eq1ProdFun/zones/WestEast.csv')
CrMaize8<-merge(CrMaize5,zones,all.x=TRUE,sort=FALSE)
CrMaize8<-CrMaize8[with(CrMaize8, order(name,Year)),]



# unique(CrMaize8[CrMaize8$west1==1,]$ID)[-1]

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

# ------------------------------           PRECIPITATION                  -------------------------------- -------       ------------------------------------ ------            ----------          --------------------------- -----------------------

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

CrMaize8$PrecZones<-NA
CrMaize8$PrecZonVar<-NA


# 1. West

CrMaize8$PrecZones[CrMaize8$ID==33] <-sapply(2000:2014, function (j)
                     {CrMaize8$PrecZones[CrMaize8$ID==33 & CrMaize8$Year==j]<-mean(climate5$Prec[["33"]][[as.character(j)]][c(5,6,7,8,9)])
                     })

CrMaize8$PrecZonVar[CrMaize8$ID==33] <-sapply(2000:2014, function (j)
{CrMaize8$PrecZonVar[CrMaize8$ID==33 & CrMaize8$Year==j]<-sd(climate5$Prec[["33"]][[as.character(j)]][c(5,6,7,8,9)])/mean(climate5$Prec[["33"]][[as.character(j)]][c(5,6,7,8,9)])
})

for (i in unique(CrMaize8[CrMaize8$west1==1,]$ID)[-1])
{
  CrMaize8$PrecZones[CrMaize8$ID==i] <-sapply(1999:2014, function (j)
  {CrMaize8$PrecZones[CrMaize8$ID==i & CrMaize8$Year==j]<-mean(climate5$Prec[[`i`]][[as.character(j)]][c(5,6,7,8,9)])})

  CrMaize8$PrecZonVar[CrMaize8$ID==i] <-sapply(1999:2014, function (j)
  {CrMaize8$PrecZonVar[CrMaize8$ID==i & CrMaize8$Year==j]<-sd(climate5$Prec[[`i`]][[as.character(j)]][c(5,6,7,8,9)])/mean(climate5$Prec[[`i`]][[as.character(j)]][c(5,6,7,8,9)])
  })
}


#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
# 2. East
for (i in unique(CrMaize8[CrMaize8$west1==0,]$ID))
{
  CrMaize8$PrecZones[CrMaize8$ID==i] <-sapply(1999:2014, function (j)
  {CrMaize8$PrecZones[CrMaize8$ID==i & CrMaize8$Year==j]<-mean(c(climate5$Prec[[`i`]][[as.character(j-1)]][c(11:12)],climate5$Prec[[`i`]][[as.character(j)]][c(1:9)]))})

CrMaize8$PrecZonVar[CrMaize8$ID==i] <-sapply(1999:2014, function (j)
{CrMaize8$PrecZonVar[CrMaize8$ID==i & CrMaize8$Year==j]<-sd(c(climate5$Prec[[`i`]][[as.character(j-1)]][c(11:12)],climate5$Prec[[`i`]][[as.character(j)]][c(1:9)]))/ mean(c(climate5$Prec[[`i`]][[as.character(j-1)]][c(11:12)],climate5$Prec[[`i`]][[as.character(j)]][c(1:9)]))
})
}


#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

# ------------------------------           TEMPERATURE                  -------------------------------- -------       ------------------------------------ ------            ----------          --------------------------- -----------------------

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

CrMaize8$TempZones<-NA
CrMaize8$TempZonVar<-NA


# 1. West

CrMaize8$TempZones[CrMaize8$ID==33] <-sapply(2000:2014, function (j)
{CrMaize8$TempZones[CrMaize8$ID==33 & CrMaize8$Year==j]<-mean(climate5$Tmx[["33"]][[as.character(j)]][c(5,6,7,8,9)])
})

CrMaize8$TempZonVar[CrMaize8$ID==33] <-sapply(2000:2014, function (j)
{CrMaize8$TempZonVar[CrMaize8$ID==33 & CrMaize8$Year==j]<-sd(climate5$Tmx[["33"]][[as.character(j)]][c(5,6,7,8,9)])/mean(climate5$Tmx[["33"]][[as.character(j)]][c(5,6,7,8,9)])
})

for (i in unique(CrMaize8[CrMaize8$west1==1,]$ID)[-1])
{
  CrMaize8$TempZones[CrMaize8$ID==i] <-sapply(1999:2014, function (j)
  {CrMaize8$TempZones[CrMaize8$ID==i & CrMaize8$Year==j]<-mean(climate5$Tmx[[`i`]][[as.character(j)]][c(5,6,7,8,9)])})
  
  CrMaize8$TempZonVar[CrMaize8$ID==i] <-sapply(1999:2014, function (j)
  {CrMaize8$TempZonVar[CrMaize8$ID==i & CrMaize8$Year==j]<-sd(climate5$Tmx[[`i`]][[as.character(j)]][c(5,6,7,8,9)])/mean(climate5$Tmx[[`i`]][[as.character(j)]][c(5,6,7,8,9)])
  })
}


#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
# 2. East
for (i in unique(CrMaize8[CrMaize8$west1==0,]$ID))
{
  CrMaize8$TempZones[CrMaize8$ID==i] <-sapply(1999:2014, function (j)
  {CrMaize8$TempZones[CrMaize8$ID==i & CrMaize8$Year==j]<-mean(c(climate5$Tmx[[`i`]][[as.character(j-1)]][c(11:12)],climate5$Tmx[[`i`]][[as.character(j)]][c(1:9)]))})

  CrMaize8$TempZonVar[CrMaize8$ID==i] <-sapply(1999:2014, function (j)
  {CrMaize8$TempZonVar[CrMaize8$ID==i & CrMaize8$Year==j]<-sd(c(climate5$Tmx[[`i`]][[as.character(j-1)]][c(11:12)],climate5$Tmx[[`i`]][[as.character(j)]][c(1:9)]))/ mean(c(climate5$Tmx[[`i`]][[as.character(j-1)]][c(11:12)],climate5$Tmx[[`i`]][[as.character(j)]][c(1:9)]))
  })
}




#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------



#  PARADA, checked, sedi 


rm(list=setdiff(ls(), "CrMaize8"))
save.image("~/foodSystems/dataFS/Main/CrMaize8.RData")
