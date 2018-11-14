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

# I think that I can use this as a base and for comparison with some of the following models with more fancy variables.

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Peggy1<-lmer(Yield ~ I((SeasRain_MAM_L1+SeasRain_OND_L1)*(1-west1))+I(SeasRain_MAM*(1-west1)) +I(SeasRain_MAM*(west1)) 
              +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1) 
              
              +  CumDD_OctMar   +   SeasRain_OND   +  SeasRain_MAM   +SeasRain_MAM_L1  +SeasRain_OND_L1
            +SDtemp_OctMar_L1  +   SDtemp_MarSep_L1   # these are a bit dodgy..reconsider??
                +MaxTemp_MarSep  + HeatWDays_OctMar   +DrSpell20_OND+DrSpell10_MAM
            +Prec2Months_MAM +I(Prec2Months_MAM^2)+  HeatWDays_MarSep_L1 +I(HeatWDays_MarSep_L1^2)
                
              +(1+ I((SeasRain_MAM_L1+SeasRain_OND_L1)*(1-west1))+SeasRain_MAM
                +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1)|ID1)
              ,data=dataScTS )
summary(Peggy1)    
nobs(Peggy1)    


# remova all insignificant (except those which in the basis):


Peggy2<-lmer(Yield ~ I((SeasRain_MAM_L1+SeasRain_OND_L1)*(1-west1))+I(SeasRain_MAM*(1-west1)) +I(SeasRain_MAM*(west1)) 
             +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1) 
             
             +SDtemp_OctMar_L1   # these are a bit dodgy..reconsider??
             +MaxTemp_MarSep  + HeatWDays_OctMar   +DrSpell20_OND+DrSpell10_MAM
             +Prec2Months_MAM +I(Prec2Months_MAM^2)+  HeatWDays_MarSep_L1 +I(HeatWDays_MarSep_L1^2)
             
             +(1+ I((SeasRain_MAM_L1+SeasRain_OND_L1)*(1-west1))+SeasRain_MAM
               +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1)|ID1)
             ,data=dataScTS )
summary(Peggy2)    
nobs(Peggy2)   





Peggy2<-lmer(Yield ~ I((SeasRain_MAM_L1+SeasRain_OND_L1)*(1-west1))+I(SeasRain_MAM*(1-west1)) +I(SeasRain_MAM*(west1)) 
             +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1) 
             
             +SDtemp_OctMar_L1   # these are a bit dodgy..reconsider??
            
             +Prec2Months_MAM +  HeatWDays_MarSep_L1 +I(HeatWDays_MarSep_L1^2)
             
             +(1+ I((SeasRain_MAM_L1+SeasRain_OND_L1)*(1-west1))+SeasRain_MAM
               +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1)|ID1)
             ,data=dataScTS )
summary(Peggy2)    
nobs(Peggy2)   


#------------------------------------------------------------------------------

Peggy3<-lmer(Yield ~ I((SeasRain_MAM_L1+SeasRain_OND_L1)*(1-west1))
             +I(AvgTemp_MarSep_L1*west1) 
             
             +SDtemp_OctMar_L1   # these are a bit dodgy..reconsider??
             
             +  HeatWDays_MarSep_L1 +I(HeatWDays_MarSep_L1^2)
             
             +(1+ I(SeasRain_MAM_L1+SeasRain_OND_L1)+SeasRain_MAM
           +I(AvgTemp_MarSep_L1)|ID1) ,data=dataScTS )
summary(Peggy3)    
nobs(Peggy3)   





#------------------------------------------------------------------------------

Peggy4<-lmer(Yield ~ I((SeasRain_MAM_L1+SeasRain_OND_L1))
             +I(AvgTemp_MarSep_L1) 
             
             +SDtemp_OctMar_L1   # these are a bit dodgy..reconsider??
             
             +  HeatWDays_MarSep_L1 +I(HeatWDays_MarSep_L1^2)
             
             +(1+ I(SeasRain_MAM_L1+SeasRain_OND_L1)+SeasRain_MAM
               +I(AvgTemp_MarSep_L1)|ID1) ,data=dataScTS )
summary(Peggy4)      # ok, I would end it here............
nobs(Peggy4)   

anova(Peggy3,Peggy4)  # OK, Peggy 3 seems to be better for now!!!!!!!!!!

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# Peggy44 the best for now!!!!!!

Peggy44<-lmer(Yield ~ I(SeasRain_MAM_L1+SeasRain_OND_L1)
             +I(AvgTemp_MarSep_L1) 
             +SDtemp_OctMar_L1   
             
             +   MaxRain_OND 
             
             +(1+MaxRain_OND+I(SeasRain_MAM_L1+SeasRain_OND_L1)
               +I(AvgTemp_MarSep_L1)|ID1) ,data=dataScTS )
summary(Peggy44)    
nobs(Peggy44)   



#--------------------------------- new matching base:
PeggyB<-lmer(Yield ~ I(SeasRain_MAM_L1+SeasRain_OND_L1)
             +I(AvgTemp_MarSep_L1) 
             
             +(1+I(SeasRain_MAM_L1+SeasRain_OND_L1)
               +I(AvgTemp_MarSep_L1)|ID1) ,data=dataScTS[rownames(dataScTS)%in% rownames(  model.frame(Peggy44)),])
    
summary(PeggyB)    
nobs(PeggyB)   

# for the bloody anova purpose   llllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllll

aa<-dataScTS[rownames(dataScTS)%in% rownames(  model.frame(Peggy44)),]

Peggy44<-lmer(Yield ~ I(SeasRain_MAM_L1+SeasRain_OND_L1)
              +I(AvgTemp_MarSep_L1) 
              +SDtemp_OctMar_L1   
              
              +   MaxRain_OND 
              
              +(1+MaxRain_OND+I(SeasRain_MAM_L1+SeasRain_OND_L1)
                +I(AvgTemp_MarSep_L1)|ID1) ,data=aa )
summary(Peggy44)    
nobs(Peggy44)   



#--------------------------------- new matching base:
PeggyB<-lmer(Yield ~ I(SeasRain_MAM_L1+SeasRain_OND_L1)
             +I(AvgTemp_MarSep_L1) 
             
             +(1+I(SeasRain_MAM_L1+SeasRain_OND_L1)
               +I(AvgTemp_MarSep_L1)|ID1) ,data=aa)

summary(PeggyB)    
nobs(PeggyB)   

aa<-dataScTS[rownames(dataScTS)%in% rownames(  model.frame(Peggy44)),]
anova(PeggyB,Peggy44)  # good

# SEE BELOW PEGGY44NN THAT THE AVERAGE TEMPERATURE SD IS NOT SIGNIFICANT JUST BECAUSE IT IS A PROXY OF TEMPERATURE OF THAT PERIOS
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

# one more idea- maybe the heatwave days will now have a good sign......
#.........OK NOPE!!!!!!!!!!!!

Peggy44b<-lmer(Yield ~ I(SeasRain_MAM_L1+SeasRain_OND_L1)
              +I(AvgTemp_MarSep_L1) 
              +SDtemp_OctMar_L1   
              
              +   MaxRain_OND +HeatWDays_MarSep +I(HeatWDays_MarSep^2)
              
              +(1+MaxRain_OND+I(SeasRain_MAM_L1+SeasRain_OND_L1)
                +I(AvgTemp_MarSep_L1+HeatWDays_MarSep)|ID1) ,data=dataScTS )
summary(Peggy44b)    
nobs(Peggy44b)   
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

Peggy44nn<-lmer(Yield ~ I(SeasRain_MAM_L1+SeasRain_OND_L1)
              +I(AvgTemp_MarSep_L1) 
              +AvgTemp_OctMar_L1 
              
              +   MaxRain_OND 
              
              +(1+MaxRain_OND+I(SeasRain_MAM_L1+SeasRain_OND_L1)
                +I(AvgTemp_MarSep_L1)|ID1) ,data=aa )
summary(Peggy44nn)    