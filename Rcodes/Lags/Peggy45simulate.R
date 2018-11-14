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

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#                  ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#                                    ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#                                                 # so for now Peggy 45 seems to be the best

Peggy45<-lmer(Yield ~ I((SeasRain_MAM_L1+SeasRain_OND_L1)*(1-west1)) + I((SeasRain_MAM_L1)*west1)+ I(AvgTemp_MarSep_L1*(1-west1)) 
              + I(AvgTemp_MarSep_L1*(west1))+I(SDtemp_OctMar_L1*(1-west1))+    +I(SDtemp_OctMar_L1*west1)
              
              +   I(MaxRain_OND*(1-west1)) +   I(MaxRain_OND*west1) 
              
              +(1+MaxRain_OND+I(SeasRain_MAM_L1+SeasRain_OND_L1) +I(AvgTemp_MarSep_L1)|ID1) ,data=dataScTS )
summary(Peggy45)  
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------

# try to remove insignificant,,

Peggy450<-lmer(Yield ~ I((SeasRain_MAM_L1+SeasRain_OND_L1)*(1-west1)) +  I(AvgTemp_MarSep_L1*(west1))
               +   +I(SDtemp_OctMar_L1*west1)
               
               +   I(MaxRain_OND*(1-west1)) +   I(MaxRain_OND*west1) 
               
               +(1+MaxRain_OND+I(SeasRain_MAM_L1+SeasRain_OND_L1) +I(AvgTemp_MarSep_L1)|ID1) ,data=dataScTS )
summary(Peggy450)  
anova(Peggy45,Peggy450) # good, not much changes. maybe Pedram will like this one more..
dotplot(ranef(Peggy45))
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
firsts<-simulate(Peggy45)
nobs(Peggy45)
summary(Peggy45)  
summary(Peggy45)$coeff 
model.frame(Peggy45)  
ranef(Peggy45)$ID1[1,]  

# so now I will try to get the the same values as from simulate myslelf:

summary(Peggy45)$coeff[,1] 

model.frame(Peggy45)[2:9]

#   test<-sapply(1:584,function(x)  sum(cbind(1,model.frame(Peggy45)[x,2:9])*summary(Peggy45)$coeff[,1])  )  +   sapply(1:584,function(x) sum(unlist(ranef(Peggy45)$ID1[1,])*cbind(1,model.frame(Peggy45)[x,10:12])  ))

fixefF<-as.matrix(cbind(1,model.frame(Peggy45)[2:9]))%*%as.numeric(summary(Peggy45)$coeff[,1]  )
raneF<-as.matrix(cbind(1,model.frame(Peggy45)[,10:12])  )  %*%  as.numeric(ranef(Peggy45)$ID1[1,])

fittedAll<-fixefF+raneF 

#ok, it is not the same as fitted(Peggz45) because´I onlz use raneffects from one county:same