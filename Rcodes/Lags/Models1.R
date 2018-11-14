rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

# setwd(WDuni)
# setwd(WDhome)

library('dplyr')
library('tseries')
library(plm)
library(lme4)
library(lattice)
library(car)
library(lmerTest)
library(optimx)

load("Main/data.RData")
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

laggy0<-lm(Yield ~ I(SeasRain_OND_L1*(1-west1))+I(SeasRain_MAM*(1-west1)) +I(SeasRain_MAM*(west1)) 
           +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1),data=dataScTS )
summary(laggy0)     


laggy0b<-lm(Yield ~ I(SeasRain_OND_L1*(1-west1))+SeasRain_MAM
           +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1),data=dataScTS )
summary(laggy0b)     

laggy0b<-lm(Yield ~ SeasRain_OND_L1+I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1),data=dataScTS )
summary(laggy0b)     


laggy0<-lm(Yield ~ I((SeasRain_OND_L1+SeasRain_MAM)*(1-west1)) +I(SeasRain_MAM*(west1)) 
           +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1),data=dataScTS )
summary(laggy0)     


laggy0<-lm(Yield ~ SeasRain_OND_L1+SeasRain_MAM+I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1),data=dataScTS )
summary(laggy0)     

#------------------------------------------------------
  
#now mixed models

laggy1<-lmer(Yield ~ I(SeasRain_OND_L1*(1-west1))+I(SeasRain_MAM*(1-west1)) +I(SeasRain_MAM*(west1)) 
           +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1) 
           +(1+ I(SeasRain_OND_L1*(1-west1))+I(SeasRain_MAM*(1-west1)) +I(SeasRain_MAM*(west1))
             +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1)|ID1)
           ,data=dataScTS )
summary(laggy1)     


laggy1b<-lmer(Yield ~ I(SeasRain_OND_L1*(1-west1))+I(SeasRain_MAM*(1-west1)) +I(SeasRain_MAM*(west1)) 
             +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1) 
             +(1+SeasRain_OND_L1+I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1)|ID1)
             ,data=dataScTS )
summary(laggy1b)     


laggy1b<-lmer(Yield ~ I(SeasRain_OND_L1*(1-west1))+I(SeasRain_MAM*(1-west1)) +I(SeasRain_MAM*(west1)) 
              +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1) 
              +(1+SeasRain_OND_L1+I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1)|ID1)
              ,data=dataScTS )
summary(laggy1b)     




             
laggy1<-lmer(Yield ~ I(SeasRain_OND_L1*(1-west1)) +I(SeasRain_MAM*(west1)) 
             +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1) 
             +(1+ I(SeasRain_OND_L1*(1-west1))+I(SeasRain_MAM*(1-west1)) +I(SeasRain_MAM*(west1))
               +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1)|ID1)
             ,data=dataScTS )
summary(laggy1)     


laggy1<-lmer(Yield ~ I(SeasRain_OND_L1)
             +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1) 
             +(1+ I(SeasRain_OND_L1*(1-west1))+I(SeasRain_MAM*(1-west1)) +I(SeasRain_MAM*(west1))
               +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1)|ID1)
             ,data=dataScTS )
summary(laggy1)     

#------------------------------------------------------

#now mixed models

baggy1<-lmer(Yield ~ I(Prec2Months_OND_L1*(1-west1))+I(Prec2Months_MAM*(1-west1)) +I(Prec2Months_MAM*(west1)) 
             +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1) 
             +(1+ I(Prec2Months_OND_L1*(1-west1))+I(Prec2Months_MAM*(1-west1)) +I(Prec2Months_MAM*(west1))
               +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1)|ID1)
             ,data=dataScTS )
summary(baggy1)     

baggy2<-lmer(Yield ~ I(Prec2Months_OND_L1*(1-west1))+I(Prec2Months_MAM*(1-west1)) +I(Prec2Months_MAM*(west1)) 
             +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1) 
             +(1+I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1)|ID1)
             ,data=dataScTS )
summary(baggy2)  


baggy2b<-lmer(Yield ~ I(Prec2Months_OND_L1*(1-west1))+Prec2Months_MAM 
             +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1) 
             +(1+I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1)|ID1)
             ,data=dataScTS )
summary(baggy2b)  


baggy2bl<-lmer(Yield ~ I(SeasRain_OND_L1*(1-west1))+SeasRain_MAM 
              +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1) 
              +(1+I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1)|ID1)
              ,data=dataScTS )
summary(baggy2bl)  #nice

baggy2bk<-lmer(Yield ~ I(SeasRain_OND_L1*(1-west1))+I(SeasRain_MAM *west1)
               +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1) 
               +(1+I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1)|ID1)
               ,data=dataScTS )
summary(baggy2bk)  

baggy2bk<-lmer(Yield ~ I(SeasRain_OND_L1*(1-west1))+I(SeasRain_MAM *west1)
               +I(SeasRain_MAM *(west1-1))
               +I(AvgTemp_OctMar*(west1-1))+I(AvgTemp_MarSep_L1*west1) 
               +(1+I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1)|ID1)
               ,data=dataScTS )
summary(baggy2bk)  


baggy2bk<-lmer(Yield ~ I(SeasRain_OND_L1*(1-west1))+I(SeasRain_MAM *west1)
               +I(SeasRain_MAM *(west1-1))
               +I(AvgTemp_OctMar*(west1-1))+I(AvgTemp_MarSep_L1*west1) 
               +(1+I(SeasRain_OND_L1*(1-west1))+I(SeasRain_MAM *west1)
                 +I(SeasRain_MAM *(west1-1))
                 +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1)|ID1)
               ,data=dataScTS )
summary(baggy2bk)  



baggy2<-lmer(Yield ~ Prec2Months_OND_L1+Prec2Months_MAM   +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1) 
             +(1+Prec2Months_OND_L1+Prec2Months_MAM +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1)|ID1)
             ,data=dataScTS )
summary(baggy2)  
 