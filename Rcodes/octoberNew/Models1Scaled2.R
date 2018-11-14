rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma
# load("~/foodSystems/Rcodes/octoberNew/Models1.RData")

library(dplyr); library(tseries); library(plm); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)

load("Main/isdataTS.RData")
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
Amelie0B<-lm(Yield~SeasPr+AvgTemp,data=isdataScTS) # this is basically the same as if unscaled
summary(Amelie0B) 

Amelie0Bln<-lm(log(Yield)~SeasPr+AvgTemp,data=isdataScTS)
summary(Amelie0Bln) # this is basically the same as if unscaled
#----------------------------------
Amelie0<-lmer(Yield~SeasPr+AvgTemp+(SeasPr+AvgTemp|ID1),data=isdataScTS)
summary(Amelie0) # much better than beckey

Amelie0ln<-lmer(log(Yield)~SeasPr+AvgTemp+(SeasPr+AvgTemp|ID1),data=isdataScTS)
summary(Amelie0ln) # failed to converge...scaling???

Amelie1<-lmer(Yield~SeasPr+AvgTemp+(0+SeasPr+AvgTemp|ID1),data=isdataScTS)
summary(Amelie1) # much better than beckey

Amelie1ln<-lmer(log(Yield)~SeasPr+AvgTemp+(0+SeasPr+AvgTemp|ID1),data=isdataScTS)
summary(Amelie1ln)  # no significance

Amelie2<-lmer(Yield~SeasPr+AvgTemp+(1|ID1),data=isdataScTS)
summary(Amelie2)

Amelie2ln<-lmer(log(Yield)~SeasPr+AvgTemp+(1|ID1),data=isdataScTS)
summary(Amelie2ln) 
#---------------------------------------------------------------------------------------------------------------------------------------------------
Amelie1b<-lmer(Yield~SeasPr+I(SeasPr^2)+AvgTemp+I(AvgTemp^2)+(SeasPr+AvgTemp|ID1),data=isdataScTS)
summary(Amelie1b)

Amelie1bln<-lmer(log(isdataTS$Yield)~SeasPr+I(SeasPr^2)+AvgTemp+I(AvgTemp^2)+(0+SeasPr+AvgTemp|ID1),data=isdataScTS)
summary(Amelie1bln)  # cannt do log of scaled yield cos of negative numbers

# interactrions
Amelie1d<-lmer(Yield~SeasPr+I(SeasPr^2)+AvgTemp+I(AvgTemp^2)+ I(SeasPr*AvgTemp)+ (SeasPr+AvgTemp|ID1),data=isdataScTS)
summary(Amelie1d)

Amelie1dln<-lmer(log(isdataTS$Yield)~SeasPr+I(SeasPr^2)+ I(SeasPr*AvgTemp)+AvgTemp+I(AvgTemp^2)+(0+SeasPr+AvgTemp|ID1),data=isdataScTS)
summary(Amelie1dln) 

#---------------
Amelie1c<-lmer(Yield~SeasPr+I(SeasPr^2)+AvgTemp+I(AvgTemp^2)+ I(SeasPr*AvgTemp)+
                 (0+SeasPr+AvgTemp+I(SeasPr^2)+I(AvgTemp^2)+I(SeasPr*AvgTemp)   |ID1),data=isdataScTS)
summary(Amelie1c)

Amelie1cln<-lmer(log(isdataTS$Yield)~SeasPr+I(SeasPr^2)+ I(SeasPr*AvgTemp)+AvgTemp+I(AvgTemp^2)
                 +(0+SeasPr+AvgTemp+I(SeasPr^2)+I(AvgTemp^2)+I(SeasPr*AvgTemp)   |ID1),data=isdataScTS)
summary(Amelie1cln) 