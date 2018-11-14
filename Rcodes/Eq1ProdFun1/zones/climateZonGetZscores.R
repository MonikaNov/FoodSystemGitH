rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

load("~/foodSystems/dataFS/Main/CrMaize8.RData")
load("~/foodSystems/dataFS/Main/climate5.RData")
library(plm)

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

# ------------------------------           PRECIPITATION                  -------------------------------- -------       ------------------------------------ ------            ----------          --------------------------- -----------------------

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

CrMaize8$PrecZscore<-NA
CrMaize8$PrecZscoreVar<-NA

# 1. West

CrMaize8$PrecZscore[CrMaize8$ID==33] <-sapply(2000:2014, function (j)
                     {CrMaize8$PrecZscore[CrMaize8$ID==33 & CrMaize8$Year==j]<-mean(climate5$PrecZ[["33"]][[as.character(j)]][c(5,6,7,8,9)])
                     })

CrMaize8$PrecZscoreVar[CrMaize8$ID==33] <-sapply(2000:2014, function (j)
{CrMaize8$PrecZscoreVar[CrMaize8$ID==33 & CrMaize8$Year==j]<-sd(climate5$PrecZ[["33"]][[as.character(j)]][c(5,6,7,8,9)])/mean(climate5$PrecZ[["33"]][[as.character(j)]][c(5,6,7,8,9)])
})

for (i in unique(CrMaize8[CrMaize8$west1==1,]$ID)[-1])
{
  CrMaize8$PrecZscore[CrMaize8$ID==i] <-sapply(1999:2014, function (j)
  {CrMaize8$PrecZscore[CrMaize8$ID==i & CrMaize8$Year==j]<-mean(climate5$PrecZ[[`i`]][[as.character(j)]][c(5,6,7,8,9)])})

  CrMaize8$PrecZscoreVar[CrMaize8$ID==i] <-sapply(1999:2014, function (j)
  {CrMaize8$PrecZscoreVar[CrMaize8$ID==i & CrMaize8$Year==j]<-sd(climate5$PrecZ[[`i`]][[as.character(j)]][c(5,6,7,8,9)])/mean(climate5$PrecZ[[`i`]][[as.character(j)]][c(5,6,7,8,9)])
  })
}


#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
# 2. East
for (i in unique(CrMaize8[CrMaize8$west1==0,]$ID))
{
  CrMaize8$PrecZscore[CrMaize8$ID==i] <-sapply(1999:2014, function (j)
  {CrMaize8$PrecZscore[CrMaize8$ID==i & CrMaize8$Year==j]<-mean(c(climate5$PrecZ[[`i`]][[as.character(j-1)]][c(11:12)],climate5$PrecZ[[`i`]][[as.character(j)]][c(1:9)]))})

CrMaize8$PrecZscoreVar[CrMaize8$ID==i] <-sapply(1999:2014, function (j)
{CrMaize8$PrecZscoreVar[CrMaize8$ID==i & CrMaize8$Year==j]<-sd(c(climate5$PrecZ[[`i`]][[as.character(j-1)]][c(11:12)],climate5$PrecZ[[`i`]][[as.character(j)]][c(1:9)]))/ mean(c(climate5$PrecZ[[`i`]][[as.character(j-1)]][c(11:12)],climate5$PrecZ[[`i`]][[as.character(j)]][c(1:9)]))
})
}


#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

# ------------------------------           TEMPERATURE                  -------------------------------- -------       ------------------------------------ ------            ----------          --------------------------- -----------------------

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

CrMaize8$TempZscore<-NA
CrMaize8$TempZscoreVar<-NA


# 1. West

CrMaize8$TempZscore[CrMaize8$ID==33] <-sapply(2000:2014, function (j)
{CrMaize8$TempZscore[CrMaize8$ID==33 & CrMaize8$Year==j]<-mean(climate5$TmxZ[["33"]][[as.character(j)]][c(5,6,7,8,9)])
})

CrMaize8$TempZscoreVar[CrMaize8$ID==33] <-sapply(2000:2014, function (j)
{CrMaize8$TempZscoreVar[CrMaize8$ID==33 & CrMaize8$Year==j]<-sd(climate5$TmxZ[["33"]][[as.character(j)]][c(5,6,7,8,9)])/mean(climate5$TmxZ[["33"]][[as.character(j)]][c(5,6,7,8,9)])
})

for (i in unique(CrMaize8[CrMaize8$west1==1,]$ID)[-1])
{
  CrMaize8$TempZscore[CrMaize8$ID==i] <-sapply(1999:2014, function (j)
  {CrMaize8$TempZscore[CrMaize8$ID==i & CrMaize8$Year==j]<-mean(climate5$TmxZ[[`i`]][[as.character(j)]][c(5,6,7,8,9)])})
  
  CrMaize8$TempZscoreVar[CrMaize8$ID==i] <-sapply(1999:2014, function (j)
  {CrMaize8$TempZscoreVar[CrMaize8$ID==i & CrMaize8$Year==j]<-sd(climate5$TmxZ[[`i`]][[as.character(j)]][c(5,6,7,8,9)])/mean(climate5$TmxZ[[`i`]][[as.character(j)]][c(5,6,7,8,9)])
  })
}


#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
# 2. East
for (i in unique(CrMaize8[CrMaize8$west1==0,]$ID))
{
  CrMaize8$TempZscore[CrMaize8$ID==i] <-sapply(1999:2014, function (j)
  {CrMaize8$TempZscore[CrMaize8$ID==i & CrMaize8$Year==j]<-mean(c(climate5$TmxZ[[`i`]][[as.character(j-1)]][c(11:12)],climate5$TmxZ[[`i`]][[as.character(j)]][c(1:9)]))})

  CrMaize8$TempZscoreVar[CrMaize8$ID==i] <-sapply(1999:2014, function (j)
  {CrMaize8$TempZscoreVar[CrMaize8$ID==i & CrMaize8$Year==j]<-sd(c(climate5$TmxZ[[`i`]][[as.character(j-1)]][c(11:12)],climate5$TmxZ[[`i`]][[as.character(j)]][c(1:9)]))/ mean(c(climate5$TmxZ[[`i`]][[as.character(j-1)]][c(11:12)],climate5$TmxZ[[`i`]][[as.character(j)]][c(1:9)]))
  })
}
#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
#adding a better var for Area. As raw area cause variable scale and problems with convergence..

aa<-CrMaize8
CrMaize8$AreaSc<-((0.01+CrMaize8$Area)/10000)

all.equal(CrMaize8[1:23],aa)
#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww



#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------



#  PARADA, checked, sedi 
#checked ok 8.5.2018

rm(list=setdiff(ls(), "CrMaize8"))
save.image("~/foodSystems/dataFS/Main/CrMaize8.RData")
