rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

load("~/foodSystems/dataFS/Main/CrMaize5.RData")
load("~/foodSystems/dataFS/Main/climate5.RData")
library(plm)

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
zones<-read.csv(file='/its/home/mn301/foodSystems/Rcodes/Eq1ProdFun1/zones/WestEast.csv')
CrMaize6<-merge(CrMaize5,zones,all.x=TRUE,sort=FALSE)
CrMaize6<-CrMaize6[with(CrMaize6, order(name,Year)),]
#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
# West- 1 sezon

CrMaize6$PrecWplant<-0
CrMaize6$PrecWharv<-0

#--------     ---------------       --------     ---------------

#  Planting
CrMaize6$PrecWplant[CrMaize6$ID==33] <-sapply(2000:2014, function (j)
                     {CrMaize6$PrecWharv[CrMaize6$ID==33 & CrMaize6$Year==j]<-mean(climate5$Prec[["33"]][[as.character(j)]][c(4,5,6)])
                 })

for (i in unique(CrMaize6$ID)[-1])
{
  CrMaize6$PrecWplant[CrMaize6$ID==i] <-sapply(1999:2014, function (j)
  {CrMaize6$PrecWplant[CrMaize6$ID==i & CrMaize6$Year==j]<-mean(climate5$Prec[[`i`]][[as.character(j)]][c(4:6)])
  })
}



#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  Harvesting

CrMaize6$PrecWharv[CrMaize6$ID==33] <-sapply(2000:2014, function (j)
{CrMaize6$PrecWharv[CrMaize6$ID==33 & CrMaize6$Year==j]<-mean(climate5$Prec[["33"]][[as.character(j)]][c(1,10,11,12)])
})

for (i in unique(CrMaize6$ID)[-1])
{
  CrMaize6$PrecWharv[CrMaize6$ID==i] <-sapply(1999:2014, function (j)
  {CrMaize6$PrecWharv[CrMaize6$ID==i & CrMaize6$Year==j]<-mean(climate5$Prec[[`i`]][[as.character(j)]][c(1,10,11,12)])
  })
}

#--------     ---------------       --------     ---------------
#mozna spis jako interakce pak v modelu??

CrMaize6$PrecWplant<-CrMaize6$PrecWplant*CrMaize6$west1 
CrMaize6$PrecWharv<-CrMaize6$PrecWharv*CrMaize6$west1

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

# East - 2 sezons

CrMaize6$PrecEplant1<-0
CrMaize6$PrecEharv1<-0
CrMaize6$PrecEplant2<-0
CrMaize6$PrecEharv2<-0


#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  Planting 1

CrMaize6$PrecEplant1[CrMaize6$ID==33] <-sapply(2000:2014, function (j)
{CrMaize6$PrecEplant1[CrMaize6$ID==33 & CrMaize6$Year==j]<-weighted.mean(climate5$Prec[["33"]][[as.character(j)]][c(3,4,5)],w=c(0.5,1,1))
})

for (i in unique(CrMaize6$ID)[-1])
{
  CrMaize6$PrecEplant1[CrMaize6$ID==i] <-sapply(1999:2014, function (j)
  {CrMaize6$PrecEplant1[CrMaize6$ID==i & CrMaize6$Year==j]<-weighted.mean(climate5$Prec[[`i`]][[as.character(j)]][c(3,4,5)],w=c(0.5,1,1))
  })
}


#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  Harvesting 1

CrMaize6$PrecEharv1[CrMaize6$ID==33] <-sapply(2000:2014, function (j)
{CrMaize6$PrecEharv1[CrMaize6$ID==33 & CrMaize6$Year==j]<-mean(climate5$Prec[["33"]][[as.character(j)]][c(7,8)])
})

for (i in unique(CrMaize6$ID)[-1])
{
  CrMaize6$PrecEharv1[CrMaize6$ID==i] <-sapply(1999:2014, function (j)
  {CrMaize6$PrecEharv1[CrMaize6$ID==i & CrMaize6$Year==j]<-mean(climate5$Prec[[`i`]][[as.character(j)]][c(7,8)])
  })
}



#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  Planting 2 because Nov and December and harvest yearly in the year, I need to use this one from previous year, probably lagged 

CrMaize6$PrecEplant2[CrMaize6$ID==33] <-sapply(2000:2014, function (j)
{CrMaize6$PrecEplant2[CrMaize6$ID==33 & CrMaize6$Year==j]<-weighted.mean(climate5$Prec[["33"]][[as.character(j-1)]][c(11,12)],w=c(1,0.5))
})

for (i in unique(CrMaize6$ID)[-1])
{
  CrMaize6$PrecEplant2[CrMaize6$ID==i & !CrMaize6$Year==as.character(1999)] <-sapply(2000:2014, function (j)
  {CrMaize6$PrecEplant2[CrMaize6$ID==i & CrMaize6$Year==j]<-weighted.mean(climate5$Prec[[`i`]][[as.character(j-1)]][c(11,12)],w=c(1,0.5))
  })
}



CrMaize6$PrecEplant2[ CrMaize6$Year==as.character(1999)&  CrMaize6$west1==0  ]<-NA

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  Harvesting 2

CrMaize6$PrecEharv2[CrMaize6$ID==33] <-sapply(2000:2014, function (j)
{CrMaize6$PrecEharv2[CrMaize6$ID==33 & CrMaize6$Year==j]<-weighted.mean(climate5$Prec[[`i`]][[as.character(j)]][c(2,3)],w=c(1,0.5))
})

for (i in unique(CrMaize6$ID)[-1])
{
  CrMaize6$PrecEharv2[CrMaize6$ID==i] <-sapply(1999:2014, function (j)
  {CrMaize6$PrecEharv2[CrMaize6$ID==i & CrMaize6$Year==j]<-weighted.mean(climate5$Prec[[`i`]][[as.character(j)]][c(2,3)],w=c(1,0.5))
  })
}






CrMaize6$PrecEplant1<-CrMaize6$PrecEplant1*(1-CrMaize6$west1)
CrMaize6$PrecEharv1<-CrMaize6$PrecEharv1*(1-CrMaize6$west1)
CrMaize6$PrecEplant2<-CrMaize6$PrecEplant2*(1-CrMaize6$west1)
CrMaize6$PrecEharv2<-CrMaize6$PrecEharv2*(1-CrMaize6$west1)

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# CrMaize6$PrecEplant<-(CrMaize6$PrecEplant1+CrMaize6$PrecEplant2)/2
# CrMaize6$PrecEharv<-(CrMaize6$PrecEharv1+CrMaize6$PrecEharv2)/2

#---------------------------------
# CrMaize6$PrecPlant<-CrMaize6$PrecEharv+CrMaize6$PrecWharv
# CrMaize6$PrecPlant<-CrMaize6$PrecEharv+CrMaize6$PrecWharv
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------






#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

# MODELY (abyse nereklo...)

JaneyE1<-lm(Yield~PrecEplant1+PrecEharv1+PrecEplant2+PrecEharv2+PrecWplant+PrecWharv, data=CrMaize6)
summary(JaneyE1)

JaneyE1<-lm(Yield~PrecEplant1+PrecEharv1+PrecEplant2+PrecEharv2+PrecWplant+PrecWharv,weights = Area, data=CrMaize6)
summary(JaneyE1)

JaneyE2<-plm(Yield~PrecEplant1+PrecEharv1+PrecEplant2+PrecEharv2+PrecWplant+PrecWharv, index=c("ID","Year"),data=CrMaize6)
summary(JaneyE2)

JaneyE3<-plm(Yield~PrecEplant1+PrecEharv1+PrecEplant2+PrecEharv2+PrecWplant+PrecWharv,weights = Area,index=c("ID","Year"), data=CrMaize6)
summary(JaneyE3)



JaneyVC<-pvcm(Yield~PrecEplant1+PrecEharv1+PrecEplant2+PrecEharv2+PrecWplant+PrecWharv,
              model="within",weights = Area,index=c("ID","Year"), data=CrMaize6)
summary(JaneyVC)

pooltest(JaneyE3,JaneyVC)

rm(list=setdiff(ls(), "CrMaize6"))
save.image("~/foodSystems/dataFS/Main/CrMaize6.RData")

