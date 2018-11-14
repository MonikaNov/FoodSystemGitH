rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(plm)
library(pglm)
load("~/foodSystems/dataFS/Phase06.RData")
Phase07<-pdata.frame(Phase06,index=c("CountyID","T"))
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
rm(zoneID)

zoneID<-Phase06[!duplicated(Phase06$CountyID),c("CountyID","County")]



Arid<-rep(0,nrow(zoneID))
SemiArid<-rep(0,nrow(zoneID))

zoneID<-cbind(zoneID,Arid,SemiArid)

write.csv(zoneID, "~/foodSystems/Rcodes/Climate/plots/ZonesSep/zoneID.csv")

rm(zoneID2)
zoneID2<-read.csv( "~/foodSystems/Rcodes/Climate/plots/ZonesSep/zoneID2.csv",header=TRUE)

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
# Merging and checking

Phase10<-merge(Phase06,zoneID2[c(1,3,4)],by=("CountyID"),all.x=TRUE)

all.equal(Phase06,Phase10[1:19])

summary(Phase10)
table(Phase10$County,Phase10$Arid)
table(Phase10$County,Phase10$SemiArid)
table(Phase10$Arid,Phase10$SemiArid)

# Ijara is probably no longer a county. Probably merged with Garrisa, so I think that it should be Arid if something
# good
#-------------------------------------------------------------------------------------------------------------------------------------------------
rm(list=setdiff(ls(),"Phase10"))
save.image("~/foodSystems/dataFS/Main/Phase10zones.RData")
save.image( "~/foodSystems/Rcodes/Climate/plots/ZonesSep/Phase10zones.RData")