rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma
# load("~/foodSystems/Rcodes/octoberNew/Models1.RData")

library(dplyr); library(tseries); library(plm); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)

load("Main/isdataTS.RData")
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

Beckey0B<-lm(Yield~SeasPr+AvgTemp,data=isdataTS)
summary(Beckey0B) 

Beckey0Bln<-lm(log(Yield)~SeasPr+AvgTemp,data=isdataTS)
summary(Beckey0Bln) 

Beckey0<-lmer(Yield~SeasPr+AvgTemp+(SeasPr+AvgTemp|ID1),data=isdataTS)
summary(Beckey0) 

Beckey0ln<-lmer(log(Yield)~SeasPr+AvgTemp+(SeasPr+AvgTemp|ID1),data=isdataTS)
summary(Beckey0ln) # failed to converge...scaling???
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
Beckey1<-lmer(Yield~SeasPr+AvgTemp+(0+SeasPr+AvgTemp|ID1),data=isdataTS)
summary(Beckey1) 

Beckey1ln<-lmer(log(Yield)~SeasPr+AvgTemp+(0+SeasPr+AvgTemp|ID1),data=isdataTS)
summary(Beckey1ln)
nobs(Beckey1ln)
vif(Beckey1ln)

Beckey2<-lmer(Yield~SeasPr+AvgTemp+(1|ID1),data=isdataTS)
summary(Beckey2)   # GOOD, THIS ONE DOESN'T SEEM TO HAVE PROBLEMS CONVERGING

Beckey2ln<-lmer(log(Yield)~SeasPr+AvgTemp+(1|ID1),data=isdataTS)  
summary(Beckey2ln)         # GOOD, THIS ONE DOESN'T SEEM TO HAVE PROBLEMS CONVERGING EITHER
nobs(Beckey2ln)
vif(Beckey2ln)

Beckey1b<-lmer(Yield~SeasPr+I(SeasPr^2)+AvgTemp+I(AvgTemp^2)+(SeasPr+AvgTemp|ID1),data=isdataTS)
summary(Beckey1b) 

Beckey1bln<-lmer(log(Yield)~SeasPr+I(SeasPr^2)+AvgTemp+I(AvgTemp^2)+(SeasPr+AvgTemp|ID1),data=isdataTS)
summary(Beckey1bln)

Beckey1b<-lmer(Yield~SeasPr+I(SeasPr^2)+AvgTemp+I(SeasPr*AvgTemp)+I(AvgTemp^2)+(SeasPr+AvgTemp|ID1),data=isdataTS)
summary(Beckey1b)  

Beckey1bln<-lmer(log(Yield)~SeasPr+I(SeasPr^2)+AvgTemp+I(SeasPr*AvgTemp)+I(AvgTemp^2)+(SeasPr+AvgTemp|ID1),data=isdataTS)
summary(Beckey1bln)  # VERRY interesting


Beckey1c<-lmer(Yield~SeasPr+I(SeasPr^2)+AvgTemp+I(SeasPr*AvgTemp)+I(AvgTemp^2)+
                 (0+SeasPr+I(SeasPr^2)+AvgTemp+I(SeasPr*AvgTemp)+I(AvgTemp^2)|ID1),data=isdataTS)
summary(Beckey1c)  

Beckey1cln<-lmer(log(Yield)~SeasPr+I(SeasPr^2)+AvgTemp+I(SeasPr*AvgTemp)+I(AvgTemp^2)+
                   (0+SeasPr+I(SeasPr^2)+AvgTemp+I(SeasPr*AvgTemp)+I(AvgTemp^2)|ID1),data=isdataTS)
summary(Beckey1cln) 
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
Beckey3<-lmer(Yield~SeasPr+ Prec2m+ CVPrec+  Spell+   Spell10
              + Spell20+ MaxP+AvgTemp+ SDTemp+  DDays+   HWDays+  MaxT
              +(0+SeasPr+AvgTemp|ID1),data=isdataTS)
summary(Beckey3) 

Beckey3ln<-lmer(log(Yield)~SeasPr+ Prec2m+ CVPrec+  Spell+   Spell10
                + Spell20+ MaxP+AvgTemp+ SDTemp+  DDays+   HWDays+  MaxT
                +(0+SeasPr+AvgTemp|ID1),data=isdataTS)
summary(Beckey3ln)
nobs(Beckey3ln)
vif(Beckey3ln)

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
Beckey4<-lmer(Yield~SeasPr+ Prec2m+ CVPrec+  Spell+   Spell10
              + Spell20+ MaxP+AvgTemp+ SDTemp+  DDays+   HWDays+  MaxT
              +(SeasPr+AvgTemp|ID1),data=isdataTS)
summary(Beckey4) 

Beckey4ln<-lmer(log(Yield)~SeasPr+ Prec2m+ CVPrec+  Spell+   Spell10
                + Spell20+ MaxP+AvgTemp+ SDTemp+  DDays+   HWDays+  MaxT
                +(SeasPr+AvgTemp|ID1),data=isdataTS)
summary(Beckey4ln)  # max temp is significant, but according to vif very correlated with seasonal temp..maybe also picks effects of squared temp??
cor.test(isdataTS$MaxP,isdataTS$AvgTemp)

nobs(Beckey4ln)
vif(Beckey4ln)


Beckey4b<-lmer(Yield~SeasPr+I(SeasPr^2)+ Prec2m+ CVPrec+  Spell+   Spell10
              + Spell20+ MaxP+AvgTemp+I(AvgTemp^2)+ I(SeasPr*AvgTemp)+SDTemp+  DDays+   HWDays+  MaxT
              +(SeasPr+AvgTemp|ID1),data=isdataTS)
summary(Beckey4b) 

Beckey4bln<-lmer(log(Yield)~SeasPr+I(SeasPr^2)+ Prec2m+ CVPrec+  Spell+   Spell10
               + Spell20+ MaxP+AvgTemp+I(AvgTemp^2)+ I(SeasPr*AvgTemp)+SDTemp+  DDays+   HWDays+  MaxT
               +(SeasPr+AvgTemp|ID1),data=isdataTS)
summary(Beckey4bln) 



Beckey4c<-lmer(Yield~SeasPr+I(SeasPr^2)+ Prec2m+ CVPrec+  Spell+   Spell10
               + Spell20+ MaxP+AvgTemp+I(AvgTemp^2)+ I(SeasPr*AvgTemp)+SDTemp+  DDays+   HWDays
               +(SeasPr+AvgTemp|ID1),data=isdataTS)
summary(Beckey4c) 

Beckey4cln<-lmer(log(Yield)~SeasPr+I(SeasPr^2)+ Prec2m+ CVPrec+  Spell+   Spell10
                 + Spell20+ MaxP+AvgTemp+I(AvgTemp^2)+ I(SeasPr*AvgTemp)+SDTemp+  DDays+   HWDays
                 +(SeasPr+AvgTemp|ID1),data=isdataTS)
summary(Beckey4cln) 






Beckey5<-lmer(Yield~SeasPr+ Prec2m+ CVPrec+  Spell+   Spell10
              + Spell20+ MaxP+AvgTemp+ SDTemp+  DDays+   HWDays
              +(SeasPr+AvgTemp|ID1),data=isdataTS)
summary(Beckey5)   # thats better I guess...
vif(Beckey5)

Beckey5ln<-lmer(log(Yield)~SeasPr+ Prec2m+ CVPrec+  Spell+   Spell10
                + Spell20+ MaxP+AvgTemp+ SDTemp+  DDays+   HWDays
                +(SeasPr+AvgTemp|ID1),data=isdataTS)
summary(Beckey5ln)  # much better - so the high vif for Beckey4ln MaxT were due to correlation with seasonal rain
# squared temperature doesnt sem to be significant through (see Beckey1b above) CORRECTION: ACTUALLY, IF 0 REMOVED FROM THE FE EQUATION, SQ. TEMP IS SIGNIFICANT
vif(Beckey5ln)

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------

# now also properly populate the FE equation..


Beckey6<-lmer(Yield~SeasPr+ Prec2m+ CVPrec+  Spell+   Spell10
              + Spell20+ MaxP+AvgTemp+ SDTemp+  DDays+   HWDays
              +(SeasPr+ Prec2m+ CVPrec+  Spell+   Spell10
                + Spell20+ MaxP+AvgTemp+ SDTemp+  DDays+   HWDays|ID1),data=isdataTS)
summary(Beckey6)   # thats better I guess...
vif(Beckey6)

Beckey6ln<-lmer(log(Yield)~SeasPr+ Prec2m+ CVPrec+  Spell+   Spell10
                + Spell20+ MaxP+AvgTemp+ SDTemp+  DDays+   HWDays
                +(SeasPr+ Prec2m+ CVPrec+  Spell+   Spell10
                  + Spell20+ MaxP+AvgTemp+ SDTemp+  DDays+   HWDays|ID1),data=isdataTS)
summary(Beckey6ln)

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#                save.image("~/foodSystems/Rcodes/octoberNew/Models1.RData")