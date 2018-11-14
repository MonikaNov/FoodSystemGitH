# rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma
library('dplyr')
library('purrr')
library("reshape")
setwd(WDuni)
setwd(WDhome)

load("Main/dataOct.RData")

#-------------------------------------------------------------------------------------------------------------------------------------------

names(dataAll)
foo<-lm(Yield ~.-Yield,data=dataAll[-c(3,4,89:92,94,95)])
summary(foo)


names(dataAll)
foo<-lm(Yield ~.-Yield-Area-Admin1-Admin2-Yield-MT-west1-county-ADM2_NAME-code-ID1-Year,data=dataAll)
summary(foo)
nobs(foo)

#-------------------------------------------------------------------------------------------------------------------------------------------

names(dataAll)
foo2<-lm(Yield ~.-Yield,data=dataAll[-c(1,2,3,4,149:152,154:155)])
summary(foo2)

# okk. still, too small number of observations

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# Now I will try to test it into Peggy

Peggy45<-lmer(Yield ~ I((SeasRain_MAM_L1+SeasRain_OND_L1)*(1-west1)) + I((SeasRain_MAM_L1)*west1)+ I(AvgTemp_MarSep_L1*(1-west1)) + I(AvgTemp_MarSep_L1*(west1))
              +I(SDtemp_OctMar_L1*(1-west1))+    +I(SDtemp_OctMar_L1*west1)
              
              +   I(MaxRain_OND*(1-west1)) +   I(MaxRain_OND*west1) 
              
              +(1+MaxRain_OND+I(SeasRain_MAM_L1+SeasRain_OND_L1) +I(AvgTemp_MarSep_L1)|ID1) ,data=dataScTS )
summary(Peggy45)  
nobs(Peggy45)
Peggy44<-update(Peggy44,data=dataScTS)
anova(Peggy44,Peggy45)  

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

fby45<-lmer(Yield ~ I((PrecTot_MAM_L1+PrecTot_OND_L1)*(1-west1)) + I((PrecTot_MAM_L1)*west1)+ I(TempAvg_MAM_L1*(1-west1)) + I(TempAvg_MAM_L1*(west1))
              +I(TempSD_OND_L1*(1-west1))+    +I(TempSD_OND_L1*west1)
              
              +   I(MaxRain_OND*(1-west1)) +   I(MaxRain_OND*west1) 
              
              +(1+MaxRain_OND+I(PrecTot_MAM_L1+PrecTot_OND_L1) +I(TempAvg_MAM_L1)|ID1) ,data=dataAllScTS )
summary(fby45)  
nobs(fby45)

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

fby45<-lmer(Yield ~ I((PrecTot_MAM_L1+PrecTot_OND_L1)*(1-west1)) + I((PrecTot_MAM_L1)*west1)+ I(TempAvg_MAM_L1*(1-west1)) + I(TempAvg_MAM_L1*(west1))
            +I(TempSD_OND_L1*(1-west1))+    +I(TempSD_OND_L1*west1)
            
            +   I(MaxRain_OND*(1-west1)) +   I(MaxRain_OND*west1) 
            
            +(1+MaxRain_OND+I(PrecTot_MAM_L1+PrecTot_OND_L1) +I(TempAvg_MAM_L1)|ID1) ,data=dataAllScTS )
summary(fby45)  
nobs(fby45)
# if unscaled?? ecetera

fby45<-lmer(Yield ~ I((PrecTot_MAM+PrecTot_OND)*(1-west1)) + I((PrecTot_MAM)*west1)+ I(TempAvg_MAM*(1-west1)) + I(TempAvg_MAM*(west1))
            +I(TempSD_OND*(1-west1))+    +I(TempSD_OND*west1)
            
            +   I(MaxRain_OND*(1-west1)) +   I(MaxRain_OND*west1) 
            
            +(1+MaxRain_OND+I(PrecTot_MAM+PrecTot_OND) +I(TempAvg_MAM)|ID1) ,data=dataAllTS )
summary(fby45)  
nobs(fby45)

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# now are the missing values  just or mostly from temperature??remove all temperature variables

baz45<-lmer(Yield ~ I((PrecTot_MAM_L1+PrecTot_OND_L1)*(1-west1)) + I((PrecTot_MAM_L1)*west1)
           +   
            
            +   I(MaxRain_OND*(1-west1)) +   I(MaxRain_OND*west1) 
            
            +(1+MaxRain_OND+I(PrecTot_MAM_L1+PrecTot_OND_L1)|ID1) ,data=dataAllScTS )
summary(baz45)  
nobs(baz45)

