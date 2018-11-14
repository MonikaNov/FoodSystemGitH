rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

# setwd(WDuni)
# setwd(WDhome)


library('plyr')
library('dplyr')
library('tseries')
library('data.table')
library(plm)
library(lme4)
library(lattice)
library(car)
library(lmerTest)
library(optimx)

load("Main/data.RData")
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

plot(Yield~Prec2Months_OND_L1, dataScTS,type="l") 
plot(Yield~ DrSpell10_OND, dataScTS)

summary(lm(Yield~DrSpell10_OND,dataScTS));
summary(lm(Yield~DrSpell10_OND+I(Prec2Months_OND_L1^2),dataScTS))
  

plot(Yield~ I(SeasRain_MAM_L1+SeasRain_OND_L1), dataScTS)
plot(Yield~ I(SeasRain_MAM_L1), dataScTS)
plot(Yield~ I(SeasRain_OND_L1), dataScTS)

plot(Yield~ I(SeasRain_MAM_L1+SeasRain_OND_L1), DataPeggy44)
plot(Yield~ I(SeasRain_MAM_L1), DataPeggy44)
plot(Yield~ I(SeasRain_OND_L1), DataPeggy44)

summary(lm(Yield~DrSpell20_OND,dataScTS));
summary(lm(Yield~DrSpell20_OND+I(Prec2Months_OND_L1^2),dataScTS))



names(dataScTS)
Xi<-setdiff((1:124),c(1:5,32:38))
i<-0

i<-i+1
plot(Yield~dataScTS[,Xi[i]], dataScTS,xlab=names(dataScTS[Xi[i]])) 
names(dataScTS[Xi[i]])
summary(lm(Yield~dataScTS[,Xi[i]],dataScTS));
summary(lm(Yield~dataScTS[,Xi[i]]+I(dataScTS[,Xi[i]]^2),dataScTS))
names(dataScTS[Xi[i]])

anova(lm(Yield~dataScTS[,Xi[i]],dataScTS),lm(Yield~dataScTS[,Xi[i]]+I(dataScTS[,Xi[i]]^2),dataScTS))

#  +        "CumDD_OctMar"      "MaxRain_OND"    "SeasRain_OND"   ?"PrecStDev_OND"?     "MaxRain_MAM"    "SeasRain_MAM"      ?"PrecStDev_MAM"? 
          ?"Prec2Months_MAM"?    

                #   PREVIOUS YEAR:   "SDtemp_OctMar_L1"   "SDtemp_MarSep_L1"    "MaxRain_OND_L1"   "MaxRain_MAM_L1"  ("SeasRain_MAM_L1")
                                        ?"PrecStDev_MAM_L1"?    "Prec2Months_MAM_L1"
                                    

#  -   "MaxTemp_MarSep"   "HeatWDays_OctMar" "MaxTemp_MarSep"    "AvgTemp_MarSep"    "DrSpell10_OND"
          "DrSpell20_OND"    "DrySpell_OND"    "DrSpell10_MAM"   "DrSpell20_MAM"     "DrySpell_MAM" 
               
                #   PREVIOUS YEAR:   "MaxTemp_MarSep_L1"   "AvgTemp_MarSep_L1"     "DrSpell10_OND_L1"    "DrSpell20_MAM_L1"   "DrSpell20_MAM_L1"

#  hill     "MaxTemp_OctMar"    "AvgTemp_OctMar"   

#  U     ("HeatWDays_MarSep")   ("DrSpell20_OND")  ( "DrySpell_OND")   "Prec2Months_MAM"     "HeatWDays_MarSep_L1"    ("DrSpell20_OND_L1")
            ("DrSpell20_MAM_L1")





laggy0<-lm(Yield ~ I(SeasRain_OND_L1*(1-west1))+I(SeasRain_MAM*(1-west1)) +I(SeasRain_MAM*(west1)) 
           +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1),data=dataScTS )
summary(laggy0)     

laggy1<-lmer(Yield ~ I(SeasRain_OND_L1*(1-west1))+I(SeasRain_MAM*(1-west1)) +I(SeasRain_MAM*(west1)) 
             +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1) 
             +(1+ I(SeasRain_OND_L1*(1-west1))+I(SeasRain_MAM*(1-west1)) +I(SeasRain_MAM*(west1))
               +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1)|ID1)
             ,data=dataScTS )
summary(laggy1)    
nobs(laggy1)   


#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

# various correlations and descriptives

cor.test(dataScTS$MaxRain_OND,dataScTS$SeasRain_OND)

cor.test(dataScTS$MaxRain_OND,dataScTS$PrecStDev_OND)

cor.test(dataScTS$MaxRain_OND,dataScTS$MaxRain_MAM)

cor.test(dataScTS$DrSpell10_MAM,dataScTS$DrySpell_MAM)

cor.test(dataScTS$DrSpell20_MAM,dataScTS$DrySpell_MAM)
cor.test(dataScTS$DrSpell20_MAM,dataScTS$DrSpell10_MAM)