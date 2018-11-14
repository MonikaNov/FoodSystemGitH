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
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

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

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# N.I.C.E.

laggy1c<-lmer(Yield ~ I((SeasRain_MAM_L1+SeasRain_OND_L1)*(1-west1))+I(SeasRain_MAM*(1-west1)) +I(SeasRain_MAM*(west1)) 
             +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1) 
             +(1+ I((SeasRain_MAM_L1+SeasRain_OND_L1)*(1-west1))+I(SeasRain_MAM*(1-west1)) +I(SeasRain_MAM*(west1))
               +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1)|ID1)
             ,data=dataScTS )
summary(laggy1c)    

#-------  uper better as it converge

laggy1d<-lmer(Yield ~ I((SeasRain_MAM_L1)*(1-west1))+I((SeasRain_OND_L1)*(1-west1))+I(SeasRain_MAM*(1-west1)) +I(SeasRain_MAM*(west1)) 
              +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1) 
              +(1+ I((SeasRain_MAM_L1+SeasRain_OND_L1)*(1-west1))+I(SeasRain_MAM*(1-west1)) +I(SeasRain_MAM*(west1))
                +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1)|ID1)
              ,data=dataScTS )
summary(laggy1d)    


laggy1e<-lmer(Yield ~ I((SeasRain_MAM_L1+SeasRain_OND_L1+SeasRain_MAM)*(1-west1)) +I(SeasRain_MAM*(west1)) 
              +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1) 
              +(1+ I((SeasRain_MAM_L1+SeasRain_OND_L1+SeasRain_MAM)*(1-west1)) 
                +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1)|ID1)
              ,data=dataScTS )
summary(laggy1e)    


laggy1e2<-lmer(Yield ~ I((SeasRain_MAM_L1+SeasRain_OND_L1+SeasRain_MAM)*(1-west1)) +I(SeasRain_MAM*(west1)) 
              +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1) 
              +(I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1)|ID1)
              ,data=dataScTS )
summary(laggy1e2)    

laggy1f<-lmer(Yield ~ I(SeasRain_MAM_L1+SeasRain_OND_L1+SeasRain_MAM)+I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1) 
              +(1+ I(SeasRain_MAM_L1+SeasRain_OND_L1+SeasRain_MAM) +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1)|ID1)
              ,data=dataScTS )
summary(laggy1f)    


#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo


laggy1b<-lmer(Yield ~ I(SeasRain_OND_L1*(1-west1))+I(SeasRain_MAM*(1-west1)) +I(SeasRain_MAM*(west1)) 
             +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1) 
             +(1+I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1)|ID1)
             ,data=dataScTS )
summary(laggy1b)     # good, this one DOES CONVERGE... also quite nice


laggy1c<-lmer(Yield ~ I(SeasRain_OND_L1*(1-west1))+I(SeasRain_MAM*(1-west1)) +I(SeasRain_MAM*(west1)) 
             +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1) 
             +(1|ID1)
             ,data=dataScTS )
summary(laggy1c)     # good, this one DOES CONVERGE... also quite nice


#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# now trying various other variants

eggy<-lmer(Yield ~ SeasRain_MAM_L1+SeasRain_OND_L1+SeasRain_MAM+SeasRain_MAM_L1 
              +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1) 
              +(I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1) +SeasRain_MAM_L1+SeasRain_OND_L1+SeasRain_MAM+SeasRain_MAM_L1 +1|ID1)
              ,data=dataScTS )
summary(eggy)     # good, this one DOES CONVERGE... also quite nice
# nope..
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
##oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
##oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#
# Now I will try what worked for the shorter dataset

iggy0<-lmer(Yield ~ I((OctoberP_L1+ NovemberP_L1+ DecemberP_L1+  MarchP+ AprilP+ MayP)/6*(1-west1))+
  I( (MarchP+ AprilP+ MayP+ JuneP+ JulyP+ AugustP)/6*(west1)  )+ 
    I((OctoberT_L1+ NovemberT_L1+ DecemberT_L1+  MarchT+ AprilT+ MayT)/6*(1-west1))+
    I(( MarchT+ AprilT+ MayT+ JuneT+ JulyT+ AugustT)/6*(west1)  ) +(1|ID1) ,data=dataScTS)

summary(iggy0)  #seems to be working about half way
nobs(iggy0)


iggy1<-lmer(Yield ~ I((OctoberP_L1+ NovemberP_L1+ DecemberP_L1+  MarchP+ AprilP+ MayP)/6*(1-west1))+
              I( (MarchP+ AprilP+ MayP+ JuneP+ JulyP+ AugustP)/6*(west1)  )+ 
              I((OctoberT_L1+ NovemberT_L1+ DecemberT_L1+  MarchT+ AprilT+ MayT)/6*(1-west1))+
              I(( MarchT+ AprilT+ MayT+ JuneT+ JulyT+ AugustT)/6*(west1)  ) +
              (1+I((OctoberP_L1+ NovemberP_L1+ DecemberP_L1+  MarchP+ AprilP+ MayP)/6*(1-west1))+
                 I( (MarchP+ AprilP+ MayP+ JuneP+ JulyP+ AugustP)/6*(west1)  )+ 
                 I((OctoberT_L1+ NovemberT_L1+ DecemberT_L1+  MarchT+ AprilT+ MayT)/6*(1-west1))+
                 I(( MarchT+ AprilT+ MayT+ JuneT+ JulyT+ AugustT)/6*(west1)  )|ID1) ,data=dataScTS)

summary(iggy1) 

iggy1b<-lmer(Yield ~ I((OctoberP_L1+ NovemberP_L1+ DecemberP_L1+  MarchP+ AprilP+ MayP)/6*(1-west1))+
              I( (MarchP+ AprilP+ MayP+ JuneP+ JulyP+ AugustP)/6*(west1)  )+ 
              I((OctoberT_L1+ NovemberT_L1+ DecemberT_L1+  MarchT+ AprilT+ MayT)/6*(1-west1))+
              I(( MarchT+ AprilT+ MayT+ JuneT+ JulyT+ AugustT)/6*(west1)  ) +
              (I((OctoberP_L1+ NovemberP_L1+ DecemberP_L1+  MarchP+ AprilP+ MayP)/6*(1-west1))+
                 I( (MarchP+ AprilP+ MayP+ JuneP+ JulyP+ AugustP)/6*(west1)  )+ 
                 I((OctoberT_L1+ NovemberT_L1+ DecemberT_L1+  MarchT+ AprilT+ MayT)/6*(1-west1))+
                 I(( MarchT+ AprilT+ MayT+ JuneT+ JulyT+ AugustT)/6*(west1)  )+0|ID1) ,data=dataScTS)

summary(iggy1b) 


iggy2<-lmer(Yield ~ I((OctoberP_L1+ NovemberP_L1+ DecemberP_L1)/3)+  I( (MarchP+ AprilP+ MayP+ JuneP+ JulyP+ AugustP)/6 )+ 
              I((OctoberT_L1+ NovemberT_L1+ DecemberT_L1)/3)+I(( MarchT+ AprilT+ MayT+ JuneT+ JulyT+ AugustT)/6 ) +
              (1+I((OctoberP_L1+ NovemberP_L1+ DecemberP_L1)/3)+  I( (MarchP+ AprilP+ MayP+ JuneP+ JulyP+ AugustP)/6 )+ 
                 I((OctoberT_L1+ NovemberT_L1+ DecemberT_L1)/3)+I(( MarchT+ AprilT+ MayT+ JuneT+ JulyT+ AugustT)/6 )  |ID1) ,data=dataScTS)

summary(iggy2) 


iggy2b<-lmer(Yield ~ I((OctoberP_L1+ NovemberP_L1+ DecemberP_L1)/3)+  I( (MarchP+ AprilP+ MayP+ JuneP+ JulyP+ AugustP)/6 )+ 
              I((OctoberT_L1+ NovemberT_L1+ DecemberT_L1)/3)+I(( MarchT+ AprilT+ MayT+ JuneT+ JulyT+ AugustT)/6 ) +
              (0+I((OctoberP_L1+ NovemberP_L1+ DecemberP_L1)/3)+  I( (MarchP+ AprilP+ MayP+ JuneP+ JulyP+ AugustP)/6 )+ 
                 I((OctoberT_L1+ NovemberT_L1+ DecemberT_L1)/3)+I(( MarchT+ AprilT+ MayT+ JuneT+ JulyT+ AugustT)/6 )  |ID1) ,data=dataScTS)

summary(iggy2b) 



iggy2c<-lmer(Yield ~ I((OctoberP_L1+ NovemberP_L1+ DecemberP_L1)/3)+  I( (MarchP+ AprilP+ MayP+ JuneP+ JulyP+ AugustP)/6 )+ 
              I((OctoberT_L1+ NovemberT_L1+ DecemberT_L1)/3)+I(( MarchT+ AprilT+ MayT+ JuneT+ JulyT+ AugustT)/6 ) +
              (1  |ID1) ,data=dataScTS)

summary(iggy2c) 