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

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

# N.I.C.E. as B.A.S.E.

laggy1c<-lmer(Yield ~ I((SeasRain_MAM_L1+SeasRain_OND_L1)*(1-west1))+I(SeasRain_MAM*(1-west1)) +I(SeasRain_MAM*(west1)) 
              +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1) 
              +(1+ I((SeasRain_MAM_L1+SeasRain_OND_L1)*(1-west1))+SeasRain_MAM
                +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1)|ID1)
              ,data=dataScTS )
summary(laggy1c)    


# not sure about scaling and squares. IS T correct???

laggy1cSq<-lmer(Yield ~ I ((SeasRain_MAM_L1+SeasRain_OND_L1)*(1-west1))+I (((SeasRain_MAM_L1+SeasRain_OND_L1)*(1-west1))^2)
                +I(SeasRain_MAM*(1-west1)) +I((SeasRain_MAM*(1-west1)^2)) +I(SeasRain_MAM*(west1))  +I((SeasRain_MAM*(west1))^2) 
              +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1) 
              +(1+ I((SeasRain_MAM_L1+SeasRain_OND_L1)*(1-west1))+I(((SeasRain_MAM_L1+SeasRain_OND_L1)*(1-west1))^2)
                +SeasRain_MAM+I(SeasRain_MAM^2)
                +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1)|ID1)
              ,data=dataScTS )
summary(laggy1cSq)    

laggy1cSq2<-lmer(Yield ~ I((SeasRain_MAM_L1+SeasRain_OND_L1)*(1-west1))+I(SeasRain_MAM*(1-west1)) +I(SeasRain_MAM*(west1)) 
              +I(AvgTemp_OctMar*(1-west1)) +I((AvgTemp_OctMar*(1-west1))^2)   +I(AvgTemp_MarSep_L1*west1) +I((AvgTemp_MarSep_L1*west1)^2) 
              +(1+ I((SeasRain_MAM_L1+SeasRain_OND_L1)*(1-west1))+SeasRain_MAM
                +I(AvgTemp_OctMar*(1-west1)) +I((AvgTemp_OctMar*(1-west1))^2) +I(AvgTemp_MarSep_L1*west1+I((AvgTemp_MarSep_L1*west1)^2) )|ID1)
              ,data=dataScTS )
summary(laggy1cSq2)    



laggy1cSq2<-lmer(Yield ~ I ((SeasRain_MAM_L1+SeasRain_OND_L1)*(1-west1))+I (((SeasRain_MAM_L1+SeasRain_OND_L1)*(1-west1))^2)
                 +I(SeasRain_MAM*(1-west1)) +I((SeasRain_MAM*(1-west1)^2)) +I(SeasRain_MAM*(west1))  +I((SeasRain_MAM*(west1))^2) 
                 +I(AvgTemp_OctMar*(1-west1)) +I((AvgTemp_OctMar*(1-west1))^2)   +I(AvgTemp_MarSep_L1*west1) +I((AvgTemp_MarSep_L1*west1)^2) 
                 +(1+ I((SeasRain_MAM_L1+SeasRain_OND_L1)*(1-west1))+SeasRain_MAM
                   +I(AvgTemp_OctMar*(1-west1)) +I((AvgTemp_OctMar*(1-west1))^2) +I(AvgTemp_MarSep_L1*west1+I((AvgTemp_MarSep_L1*west1)^2) )|ID1)
                 ,data=dataScTS )
summary(laggy1cSq2)    


#OK, NOT SO GOOD, REALLY  ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''


# I think that I can use this as a base and for comparison with some of the following models with more fancy variables.

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Peggy<-lmer(Yield ~ I((SeasRain_MAM_L1+SeasRain_OND_L1)*(1-west1))+I(SeasRain_MAM*(1-west1)) +I(SeasRain_MAM*(west1)) 
              +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1) 
              
              +  CumDD_OctMar   +   SeasRain_OND   +  SeasRain_MAM   +SeasRain_MAM_L1  +SeasRain_OND_L1
            +SDtemp_OctMar_L1  +   SDtemp_MarSep_L1   # these are a bit dodgy..reconsider??
                +MaxTemp_MarSep  + HeatWDays_OctMar   +DrSpell20_OND+DrSpell10_MAM+MaxTemp_MarSep_L1+AvgTemp_MarSep_L1
                
              +(1+ I((SeasRain_MAM_L1+SeasRain_OND_L1)*(1-west1))+SeasRain_MAM
                +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1)|ID1)
              ,data=dataScTS )
summary(laggy1c)    

