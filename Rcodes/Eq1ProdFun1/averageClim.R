rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

load("~/foodSystems/dataFS/Main/CrMaize5.RData")
load("~/foodSystems/dataFS/Main/climate5.RData")
library(plm)

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
# 1.Precipitation

CrMaize5$i_avPrec<-NA

CrMaize5$i_avPrec[CrMaize5$ID==33] <-sapply(2000:2014, function (j)
                     {CrMaize5$i_avPrec[CrMaize5$ID==33 & CrMaize5$Year==j]<-mean(climate5$Prec[["33"]][[as.character(j)]])
                 })

for (i in unique(CrMaize5$ID)[-1])
{
  CrMaize5$i_avPrec[CrMaize5$ID==i] <-sapply(1999:2014, function (j)
  {CrMaize5$i_avPrec[CrMaize5$ID==i & CrMaize5$Year==j]<-mean(climate5$Prec[[`i`]][[as.character(j)]])
  })
}

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Temperature

CrMaize5$i_avTemp<-NA

CrMaize5$i_avTemp[CrMaize5$ID==33] <-sapply(2000:2014, function (j)
{CrMaize5$i_avTemp[CrMaize5$ID==33 & CrMaize5$Year==j]<-mean(climate5$Tmx[["33"]][[as.character(j)]])
})

for (i in unique(CrMaize5$ID)[-1])
{
  CrMaize5$i_avTemp[CrMaize5$ID==i] <-sapply(1999:2014, function (j)
  {CrMaize5$i_avTemp[CrMaize5$ID==i & CrMaize5$Year==j]<-mean(climate5$Tmx[[`i`]][[as.character(j)]])
  })
}

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Lagged Precipitation

CrMaize5$i_LavPrec<-NA

CrMaize5$i_LavPrec[CrMaize5$ID==33 & !CrMaize5$Year==as.character(2000)] <-sapply(2001:2014, function (j)
{CrMaize5$i_LavPrec[CrMaize5$ID==33 & CrMaize5$Year==j]<-mean(climate5$Prec[["33"]][[as.character(j-1)]])
})

for (i in unique(CrMaize5$ID)[-1])
{
  CrMaize5$i_LavPrec[CrMaize5$ID==i & !CrMaize5$Year==as.character(1999)] <-sapply(2000:2014, function (j)
  {CrMaize5$i_LavPrec[CrMaize5$ID==i & CrMaize5$Year==j]<-mean(climate5$Prec[[`i`]][[as.character(j-1)]])
  })
}

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Lgged Temperature

CrMaize5$i_LavTemp<-NA

CrMaize5$i_LavTemp[CrMaize5$ID==33 & !CrMaize5$Year==as.character(2000)] <-sapply(2001:2014, function (j)
{CrMaize5$i_LavTemp[CrMaize5$ID==33 & CrMaize5$Year==j]<-mean(climate5$Tmx[["33"]][[as.character(j-1)]])
})

for (i in unique(CrMaize5$ID)[-1])
{
  CrMaize5$i_LavTemp[CrMaize5$ID==i& !CrMaize5$Year==as.character(1999)] <-sapply(2000:2014, function (j)
  {CrMaize5$i_LavTemp[CrMaize5$ID==i & CrMaize5$Year==j]<-mean(climate5$Tmx[[`i`]][[as.character(j-1)]])
  })
}

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

rm(list=setdiff(ls(),"CrMaize5"))
save.image("~/foodSystems/dataFS/Main/CrMaize5avg.RData")

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

# MODELY (abyse nereklo...)

Billy1<-lm(Yield~i_avTemp+i_avPrec, data=CrMaize5)
summary(Billy1)

Billy1<-lm(Yield~i_avTemp+i_avPrec, data=CrMaize5,weights = Area)
summary(Billy1)

Billy2<-plm(Yield~i_avTemp+i_avPrec,index=c("ID","Year"), data=CrMaize5)
summary(Billy2)

Billy7<-plm(Yield~i_avTemp+i_avPrec,model='pooling',index=c("ID","Year"), data=CrMaize5)
summary(Billy7)

Billy6<-lm(Yield~i_avTemp+i_avPrec+as.factor(ID), data=CrMaize5)
summary(Billy6)

BillyVC<-pvcm(Yield~i_avTemp+i_avPrec,index=c("ID","Year"), model="within",data=CrMaize5)
summary(BillyVC)

pooltest(Billy2,BillyVC)



Billy2<-plm(Yield~i_avPrec+i_LavPrec+i_avTemp+i_LavTemp,index=c("ID","Year"), data=CrMaize5)
summary(Billy2)


Billy6<-plm(Yield~i_avPrec+i_LavPrec+i_avTemp+i_LavTemp,index=c("ID","Year"), data=CrMaize5,weights=Area)
summary(Billy6)



Billy2<-plm(Yield~i_avPrec+i_LavPrec+i_avTemp+i_LavTemp,index=c("ID","Year"), data=CrMaize5,
            subset=(!Year==1999),weights=Area )
summary(Billy2)



Billy7<-plm(Yield~i_avPrec+i_LavPrec+i_avTemp+i_LavTemp,index=c("ID","Year"), data=CrMaize5,weights=Area )
summary(Billy7)


summary(BillyVC)

BillyVC<-pvcm(Yield~i_avPrec+i_LavPrec+i_avTemp+i_LavTemp,index=c("ID","Year"),model="within",
              data=CrMaize5,weights=Area )

summary(BillyVC)
pooltest(Billy7,BillyVC)


#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# no weights pooling test


Billy8<-plm(Yield~i_avPrec+i_LavPrec+i_avTemp+i_LavTemp,index=c("ID","Year"), data=CrMaize5)
summary(Billy8)


BillyVC<-pvcm(Yield~i_avPrec+i_LavPrec+i_avTemp+i_LavTemp,index=c("ID","Year"),model="within",
              data=CrMaize5)

summary(BillyVC)

pooltest(Billy8,BillyVC)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2.5.2018
BillyVC<-pvcm(Yield~i_avPrec+i_LavPrec+i_avTemp+i_LavTemp,index=c("ID","Year"),model="random",
              data=CrMaize5,weights=Area )

summary(BillyVC)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 14.5.2018
Billy7<-plm(Yield~i_avPrec+i_LavPrec+i_avTemp+i_LavTemp,index=c("ID","Year"), data=CrMaize5,weights=Area )
summary(Billy7)

Billy9<-plm(Yield~i_avPrec+i_LavPrec+i_avTemp+i_LavTemp,index=c("ID","Year"),model="random", data=CrMaize5,weights=Area )
summary(Billy9)

# here interesting change of sign for lagged temperature...

Billy10<-plm(Yield~i_avPrec+i_LavPrec+i_avTemp+i_LavTemp,index=c("ID","Year"),model="pooling", data=CrMaize5,weights=Area )
summary(Billy10)

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#1.5.2018

Billy7<-plm(Yield~i_avPrec+i_LavPrec+i_avTemp+i_LavTemp,index=c("ID","Year"), data=CrMaize5,weights=Area )
summary(Billy7)
Billy8<-lm(Yield~i_avPrec+i_LavPrec+i_avTemp+i_LavTemp+as.factor(ID), data=CrMaize5,weights=Area )
summary(Billy8)


Billy7<-plm(Yield~i_avPrec+i_LavPrec+i_avTemp+i_LavTemp,index=c("ID","Year"), data=CrMaize5)
summary(Billy7)
Billy8<-lm(Yield~i_avPrec+i_LavPrec+i_avTemp+i_LavTemp+as.factor(ID), data=CrMaize5)
summary(Billy8)


Billy7<-plm(Yield~i_avPrec+i_LavPrec+i_avTemp+i_LavTemp+LH1+LH2+LH3+LH4+LM1+LM2+LM3+LM4+IL2+L3+L4+UH2+UH1+UM1+UM2+UM3+UM4,index=c("ID","Year"), data=CrMaize6,weights=Area )
summary(Billy7)

Billy17<-plm(Yield~i_avPrec+i_LavPrec+i_avTemp+i_LavTemp+LH1+LH2+LH3+LH4+LM1+LM2+LM3+LM4+IL2+L3+L4+UH2+UH1+UM1+UM2+UM3+UM4,index=c("ID","Year"), data=CrMaize6,weights=Area )
summary(Billy17)