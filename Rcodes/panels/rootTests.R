rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(plm)
library(pglm)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

filenames <- list.files("Main", pattern="*.RData", full.names=TRUE)
lapply(filenames,load,.GlobalEnv)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Phase07<-pdata.frame(Phase06,index=c("CountyID","T"))

purtest(Phase07$Prec,q=1,
        test = c( "ips"),
        exo = ("trend"),
        lags =3, pmax = 10, Hcons = TRUE)



aa<-c(11,17)
mapply(function(df=Phase07,aa,ll) 
{
 ll<-purtest(Phase07[,aa],q=1,
          test = c( "ips"),
          exo = ("trend"),
          lags =3, pmax = 10, Hcons = TRUE)
print(ll)
},2,aa)
  
  