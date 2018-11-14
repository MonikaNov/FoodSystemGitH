rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma
library(dplyr); library(tseries); library(plm); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)

load("Main/isdataTS.RData")
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

Beckey0B<-lm(Yield~SeasPr+AvgTemp,data=isdataTS)
summary(Beckey0B) 

Beckey0Bln<-lm(log(Yield)~SeasPr+AvgTemp,data=isdataTS)
summary(Beckey0Bln) 
#--------------------------
Rawdon0<-lmer(Yield~SeasPr+AvgTemp+(SeasPr+AvgTemp|ASAL),data=isdataTS)
summary(Rawdon0) 

Rawdon0ln<-lmer(log(Yield)~SeasPr+AvgTemp+(SeasPr+AvgTemp|ASAL),data=isdataTS)
summary(Rawdon0ln)
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Rawdon1<-lmer(Yield~SeasPr+AvgTemp+(1|ASAL),data=isdataTS) # ehm.. much better
summary(Rawdon1) 
    # should be basically the same as lm with dummy for ASAL?
    Rawd1<-lm(Yield~SeasPr+AvgTemp+as.factor(ASAL),data=isdataTS)
    summary(Rawd1) 

Rawdon1ln<-lmer(log(Yield)~SeasPr+AvgTemp+(1|ASAL),data=isdataTS)
summary(Rawdon1ln)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Rawdon2<-lmer(Yield~SeasPr+AvgTemp+I(SeasPr^2)+I(AvgTemp^2)+(1|ASAL),data=isdataTS) 
summary(Rawdon2)  # ok, scale would be better

Rawdon2ln<-lmer(log(Yield)~SeasPr+AvgTemp+I(SeasPr^2)+I(AvgTemp^2)+(1|ASAL),data=isdataTS) 
summary(Rawdon2ln)


Rawdon2<-lmer(Yield~SeasPr+AvgTemp+I(SeasPr*AvgTemp) +I(SeasPr^2)+I(AvgTemp^2)+(1|ASAL),data=isdataTS) 
summary(Rawdon2)  

Rawdon2ln<-lmer(log(Yield)~SeasPr+AvgTemp+I(SeasPr*AvgTemp)+I(SeasPr^2)+I(AvgTemp^2)+(1|ASAL),data=isdataTS) 
summary(Rawdon2ln)



Rawdon3<-lmer(Yield~SeasPr+AvgTemp+I(SeasPr*AvgTemp) +I(SeasPr^2)+I(AvgTemp^2)+(SeasPr+AvgTemp+1|ASAL),data=isdataTS) 
summary(Rawdon3)  

Rawdon3ln<-lmer(log(Yield)~SeasPr+AvgTemp+I(SeasPr*AvgTemp)+I(SeasPr^2)+I(AvgTemp^2)+(SeasPr+AvgTemp+1|ASAL),data=isdataTS) 
summary(Rawdon3ln)