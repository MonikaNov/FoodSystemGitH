rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
rm(list=ls())

WDuni<-c("/home/m/mn/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
load("Main/Crops.RData")
load("Main/CrMaize14.RData")
load("Main/CrMaize13.RData")
rm(CrMaize13ts)
CrMaize13ts<-pdata.frame(CrMaize13,index=c("ID","Year"))
rm(CrMaize14ts)
CrMaize14ts<-pdata.frame(CrMaize14,index=c("ID","Year"))
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

hist(CrMaize13ts$Yield,100)
hist(CrMaize13ts$Yield,100,xlim=c(0,5))
hist(CrMaize14ts$Yield,50,xlim=c(0,5))
# loggss

hist(log(CrMaize13ts$Yield),100)
hist(log(CrMaize14ts$Yield),100)
hist(log(CrMaize14ts$Yield),100,xlim=c(-3.5,2))
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

unique(CrMaize13ts$ID)
CrMaize13ts<-pdata.frame(CrMaize13,index=c("ID","Year"))
ts13Part<-subset(CrMaize13ts,ID %in% 1) # from about 1990,1991,1992 trend switch from negative to positive!!!then around 1996 it gets significant


# just playing to see if I manage to get the separate distributions

hist(ts13Part$Yield,50)
hist(ts13Part$Yield,100)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
