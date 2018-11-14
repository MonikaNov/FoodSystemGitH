rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

library(dplyr); library(tseries); library(plm); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)

load("Main/isdataTS.RData")
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

Beckey1bln<-lmer(log(Yield)~SeasPr+I(SeasPr^2)+AvgTemp+I(AvgTemp^2)+(SeasPr+AvgTemp|ID1),data=isdataTS)
summary(Beckey1bln)

Beckey1b<-lmer(Yield~SeasPr+I(SeasPr^2)+AvgTemp+I(SeasPr*AvgTemp)+I(AvgTemp^2)+(SeasPr+AvgTemp|ID1),data=isdataTS)
summary(Beckey1b)  



Beckey4b<-lmer(Yield~SeasPr+I(SeasPr^2)+ Prec2m+ CVPrec+  Spell+   Spell10
               + Spell20+ MaxP+AvgTemp+I(AvgTemp^2)+ I(SeasPr*AvgTemp)+SDTemp+  DDays+   HWDays+  MaxT
               +(SeasPr+AvgTemp|ID1),data=isdataTS)
summary(Beckey4b) 

Beckey4bln<-lmer(log(Yield)~SeasPr+I(SeasPr^2)+ Prec2m+ CVPrec+  Spell+   Spell10
                 + Spell20+ MaxP+AvgTemp+I(AvgTemp^2)+ I(SeasPr*AvgTemp)+SDTemp+  DDays+   HWDays+  MaxT
                 +(SeasPr+AvgTemp|ID1),data=isdataTS)
summary(Beckey4bln) 

#OR:

Beckey4c<-lmer(Yield~SeasPr+I(SeasPr^2)+ Prec2m+ CVPrec+  Spell+   Spell10
               + Spell20+ MaxP+AvgTemp+I(AvgTemp^2)+ I(SeasPr*AvgTemp)+SDTemp+  DDays+   HWDays
               +(SeasPr+AvgTemp|ID1),data=isdataTS)
summary(Beckey4c) 

Beckey4cln<-lmer(log(Yield)~SeasPr+I(SeasPr^2)+ Prec2m+ CVPrec+  Spell+   Spell10
                 + Spell20+ MaxP+AvgTemp+I(AvgTemp^2)+ I(SeasPr*AvgTemp)+SDTemp+  DDays+   HWDays
                 +(SeasPr+AvgTemp|ID1),data=isdataTS)
summary(Beckey4cln) 

