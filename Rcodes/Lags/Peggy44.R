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

laggy1c<-lmer(Yield ~ I((SeasRain_MAM_L1+SeasRain_OND_L1)*(1-west1))       +    I(SeasRain_MAM*(1-west1)) +   I(SeasRain_MAM*(west1)) 
              +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1) 
              +(1+ I((SeasRain_MAM_L1+SeasRain_OND_L1)*(1-west1))+SeasRain_MAM
                +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1)|ID1)
              ,data=dataScTS )
summary(laggy1c)    # this was meant to be the base originally.

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#         ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#                 oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#                           ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#                                    ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#                                                Peggy44 the best for now!!!!!!
# this is the resulting model..

Peggy44<-lmer(Yield ~ I(SeasRain_MAM_L1+SeasRain_OND_L1) +  I(AvgTemp_MarSep_L1) 
              +SDtemp_OctMar_L1   
              
              +   MaxRain_OND 
              
              +(1+MaxRain_OND+I(SeasRain_MAM_L1+SeasRain_OND_L1) +I(AvgTemp_MarSep_L1)|ID1) ,data=dataScTS )
summary(Peggy44)    
nobs(Peggy44)                                   #ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#                                    ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#                           ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#                  oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#         ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo


# ....... so the actual base now is this one:   ....................

PeggyB0<-lmer(Yield ~ I(SeasRain_MAM_L1+SeasRain_OND_L1) +I(AvgTemp_MarSep_L1) 
             
             +(1+I(SeasRain_MAM_L1+SeasRain_OND_L1)
               +I(AvgTemp_MarSep_L1)|ID1) ,data=dataScTS)

summary(PeggyB0)    
nobs(PeggyB0)     # now the base has less variables, thus it can use more observations... to be able to run the anova tes, I need to estimate both using the same number of observations


DataPeggy44<-dataScTS[rownames(dataScTS)%in% rownames(  model.frame(Peggy44)),]


#---------------------------------------------------------------------------------------------------------------------------
PeggyB<-lmer(Yield ~ I(SeasRain_MAM_L1+SeasRain_OND_L1)+I(AvgTemp_MarSep_L1) 
             
             +(1+I(SeasRain_MAM_L1+SeasRain_OND_L1)
               +I(AvgTemp_MarSep_L1)|ID1) ,data=DataPeggy44)

summary(PeggyB)    
nobs(PeggyB)

#  for anova, all models need to be fitted to the same data (which even have the same name). So I also have to write Peggy44 again

Peggy44<-update(Peggy44,data=DataPeggy44)
anova(PeggyB,Peggy44)  # N.I.C.E

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# now I will also test if the models with separate season not better

laggy1c<-lmer(Yield ~ I((SeasRain_MAM_L1+SeasRain_OND_L1)*(1-west1))       +    I(SeasRain_MAM*(1-west1)) +   I(SeasRain_MAM*(west1)) 
              +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1) 
              +(1+ I((SeasRain_MAM_L1+SeasRain_OND_L1)*(1-west1))+SeasRain_MAM
                +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1)|ID1)
              ,data=dataScTS )
summary(laggy1c) 


Peggy44<-lmer(Yield ~ I(SeasRain_MAM_L1+SeasRain_OND_L1) +  I(AvgTemp_MarSep_L1) 
              +SDtemp_OctMar_L1   
              
              +   MaxRain_OND 
              
              +(1+MaxRain_OND+I(SeasRain_MAM_L1+SeasRain_OND_L1) +I(AvgTemp_MarSep_L1)|ID1) ,data=dataScTS )
summary(Peggy44)  
#---------------------------
Peggy45<-lmer(Yield ~ I((SeasRain_MAM_L1+SeasRain_OND_L1)*(1-west1)) + I((SeasRain_MAM_L1)*west1)+ I(AvgTemp_MarSep_L1*(1-west1)) + I(AvgTemp_MarSep_L1*(west1))
              +I(SDtemp_OctMar_L1*(1-west1))+    +I(SDtemp_OctMar_L1*west1)
              
              +   I(MaxRain_OND*(1-west1)) +   I(MaxRain_OND*west1) 
              
              +(1+MaxRain_OND+I(SeasRain_MAM_L1+SeasRain_OND_L1) +I(AvgTemp_MarSep_L1)|ID1) ,data=dataScTS )
summary(Peggy45)  
nobs(Peggy45)
Peggy44<-update(Peggy44,data=dataScTS)
anova(Peggy44,Peggy45)  # takze Peggy45 lepsi??? mozna

# ok this would be even better. I also need to adjust the random part...

Peggy47<-lmer(Yield ~ I((SeasRain_MAM_L1+SeasRain_OND_L1)*(1-west1)) + I((SeasRain_MAM_L1)*west1)
              + I(AvgTemp_MarSep_L1*(1-west1)) + I(AvgTemp_MarSep_L1*(west1))
              +I(SDtemp_OctMar_L1*(1-west1))+    +I(SDtemp_OctMar_L1*west1)
              
              +   I(MaxRain_OND*(1-west1)) +   I(MaxRain_OND*west1) 
              
              +(1+MaxRain_OND+I((SeasRain_MAM_L1+SeasRain_OND_L1)*(1-west1)) + I((SeasRain_MAM_L1)*west1)+I(AvgTemp_MarSep_L1)|ID1) ,data=dataScTS )
summary(Peggy47)  
nobs(Peggy47)
anova(Peggy47,Peggy45) #podle ANOVA je to jedno, tak ais muzu nechat PEGGY45 jako nejlepsi...Je jednodussi
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# now the base for Peggy45 and compare..


Peggy45B<-lmer(Yield ~ I((SeasRain_MAM_L1+SeasRain_OND_L1)*(1-west1))  + I((SeasRain_MAM_L1)*west1)+I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1) 
               
              +(1+ I((SeasRain_MAM_L1+SeasRain_OND_L1)*(1-west1)
               
                 +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1)|ID1)
              ,data=dataScTS )
summary(Peggy45B) 
nobs(Peggy45B) 

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# another adjustments:

Peggy46<-lmer(Yield ~ I((SeasRain_MAM_L1+SeasRain_OND_L1)*(1-west1)) + I((SeasRain_MAM_L1+SeasRain_OND_L1)*west1)+ I(AvgTemp_MarSep_L1*(1-west1)) + I(AvgTemp_MarSep_L1*(west1))
              +I(SDtemp_OctMar_L1*(1-west1))+    +I(SDtemp_OctMar_L1*west1)
              
              +   I(MaxRain_OND*(1-west1)) +   I(MaxRain_OND*west1) 
              
              +(1+MaxRain_OND+I(SeasRain_MAM_L1+SeasRain_OND_L1) +I(AvgTemp_MarSep_L1)|ID1) ,data=dataScTS )
summary(Peggy46) 

anova(Peggy46,Peggy45) 

