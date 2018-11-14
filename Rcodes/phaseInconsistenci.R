rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(plm)
library(pglm)
load("Main/VCI_NDMAphase.RData")

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
VCI2phase[which(VCI2phase$PhaseInt==1 & VCI2phase$VCI_ndma<35),]

subset(VCI2phase,PhaseInt==1 & VCI_ndma<35)



subset(VCI2phase,PhaseInt==1 & VCI_ndma<35)[c(2:4,19)]


NotConsistent<-subset(VCI2phase,PhaseInt==1 & VCI_ndma<35)

table(NotConsistent$Month)
table(NotConsistent$Year)
table(NotConsistent$County.x)