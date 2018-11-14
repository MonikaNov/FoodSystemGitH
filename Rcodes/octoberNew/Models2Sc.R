rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma
# load("~/foodSystems/Rcodes/octoberNew/Models1.RData")

library(dplyr); library(tseries); library(plm); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)
setwd(WDhome)
load("Main/isdataTS.RData")
#           load("~/foodSystems/Rcodes/octoberNew/Models2Sc.RData")
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

#       SO NOW AMELIE4LN SEEMS TO BE THE BEST   (see below.. a lot below...)!!!!!!!!!!!!

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

Amelie1d<-lmer(Yield~SeasPr+I(SeasPr^2)+AvgTemp+I(AvgTemp^2)+ I(SeasPr*AvgTemp)+ (SeasPr+AvgTemp|ID1),
               data=isdataScTS)
summary(Amelie1d)

Amelie1dln<-lmer(log(isdataTS$Yield)~SeasPr+I(SeasPr^2)+ I(SeasPr*AvgTemp)+AvgTemp+I(AvgTemp^2)+(SeasPr+AvgTemp|ID1),data=isdataScTS)
summary(Amelie1dln) 
vif(Amelie1dln) #ok, so let this one as the base??

#---------------------------------------------------------------------------------------------------------------------------------------------------
Amelie1eln<-lmer(log(isdataTS$Yield)~SeasPr+I(SeasPr^2)+ I(SeasPr*AvgTemp)+AvgTemp+I(AvgTemp^2)
                 +(SeasPr+I(SeasPr^2)+ I(SeasPr*AvgTemp)+AvgTemp+I(AvgTemp^2)|ID1),data=isdataScTS)
summary(Amelie1eln) # or possibly this one as the base??
ranova(Amelie1eln) 

Amelie1fln<-lmer(log(isdataTS$Yield)~SeasPr+I(SeasPr^2)+ I(SeasPr*AvgTemp)+AvgTemp+I(AvgTemp^2)
                 +(SeasPr+I(SeasPr^2)+ I(SeasPr*AvgTemp)+AvgTemp|ID1),data=isdataScTS)
summary(Amelie1fln) # or this one??
anova(Amelie1dln,Amelie1eln)
anova(Amelie1dln,Amelie1fln)                        # NOT really SURE ABOUT THIS, but: I think that this anova only for comparison of the models with the same fixed effects
anova(Amelie1eln,Amelie1fln) 

Amelie1g<-lmer(isdataTS$Yield~SeasPr+I(SeasPr^2)+ I(SeasPr*AvgTemp)+AvgTemp
                 +(SeasPr+I(SeasPr^2)+ I(SeasPr*AvgTemp)+AvgTemp|ID1),data=isdataScTS)
summary(Amelie1g) 

Amelie1hln<-lmer(log(isdataTS$Yield)~SeasPr+ I(SeasPr*AvgTemp)+AvgTemp+I(AvgTemp^2)
                 +(SeasPr+I(SeasPr^2)+ I(SeasPr*AvgTemp)+AvgTemp|ID1),data=isdataScTS)
summary(Amelie1hln) 
anova(Amelie1hln,Amelie1fln) 
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# so, according to anova, the model with least par. in the random effects is the best..so far. so test even less par. in the random effects

Amelie1dln<-lmer(log(isdataTS$Yield)~SeasPr+I(SeasPr^2)+ I(SeasPr*AvgTemp)+AvgTemp+I(AvgTemp^2)+(SeasPr+AvgTemp|ID1),data=isdataScTS)
summary(Amelie1dln) 

Amelie1hln<-lmer(log(isdataTS$Yield)~SeasPr+I(SeasPr^2)+ I(SeasPr*AvgTemp)+AvgTemp+I(AvgTemp^2)+(0+SeasPr+AvgTemp|ID1),data=isdataScTS)
summary(Amelie1hln)

anova(Amelie1hln,Amelie1dln) # ok, so the county level dummies seem to be needed

Amelie1iln<-lmer(log(isdataTS$Yield)~SeasPr+I(SeasPr^2)+ I(SeasPr*AvgTemp)+AvgTemp+I(AvgTemp^2)+(1|ID1),data=isdataScTS)
summary(Amelie1hln)

anova(Amelie1iln,Amelie1dln) #WELP, TEMP SQUARED DOES NOT SEEM TO BE SIGNIFICANT.. BUT I SHOULD TEST IT MORE...
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Amelie4ln<-lmer(log(isdataTS$Yield)~SeasPr+I(SeasPr^2)+ I(SeasPr*AvgTemp)+AvgTemp+(1|ID1),data=isdataScTS)
summary(Amelie4ln)
coef(Amelie4ln)

Amelie4bln<-lmer(log(isdataTS$Yield)~SeasPr+I(SeasPr^2)+ I(SeasPr*AvgTemp)+AvgTemp
                 +(SeasPr+I(SeasPr^2)+ I(SeasPr*AvgTemp)+AvgTemp|ID1),data=isdataScTS)
summary(Amelie4bln)

anova(Amelie4ln,Amelie4bln) #WELP, TEMP SQUARED DOES NOT SEEM TO BE SIGNIFICANT.. BUT I SHOULD TEST IT MORE...

# SO NOW AMELIE4LN SEEMS TO BE THE BEST !!!!!!!!!!!!
anova(Amelie4ln)
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
