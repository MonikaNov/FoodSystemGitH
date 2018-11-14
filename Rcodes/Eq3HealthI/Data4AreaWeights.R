rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)

load("Main/Phase23.RData")
load("Main/CrMaize16.RData")
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
head(Phase22)

Are<-CrMaize16[CrMaize16$Year==2014,c("ID","Area")]
names(Are)[1]<-"CountyID"
test1<-Phase23
Phase23<-merge(Phase23,Are,all.x=TRUE)

Phase23<-Phase23[with(Phase23, order(CountyID,Year,Month)),]
all.equal(test1, Phase23[1:44])
library(dply)
all_equal(test1, Phase23[1:44]  )  
names(Phase23)[45]<-"Area2014"

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# save game
rm(list=setdiff(ls(),"Phase23"))
save.image("Main/Phase23.RData")