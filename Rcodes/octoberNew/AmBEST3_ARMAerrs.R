rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma
# load("~/foodSystems/Rcodes/octoberNew/Models1.RData")

library(dplyr); library(tseries); library(plm); library(lme4);library(nlme); library(lattice); library(car); library(lmerTest); library(sandwich); library(lmtest)
setwd(WDhome)
setwd(WDuni)
load("Main/isdataTS.RData")

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
AmBEST<-lmer(log(isdataTS$Yield)~SeasPr + I(SeasPr^2) + I(SeasPr * AvgTemp) + 
               AvgTemp + CVPrec + SDTemp + (1 | ID1),data=isdataScTS)

rem<-c("27-1995",names(which(resid(AmBEST2,type="pearson")<(-2))))
# so this model, AmBEST3 is essentially the same specifiction as the AmBEST, the difference is that I got rid of the residual outliers..

AmBEST3<-lmer(log(isdataTS$Yield[!(rownames(isdataScTS) %in% rem)])~SeasPr + I(SeasPr^2) + I(SeasPr * AvgTemp) + 
                AvgTemp + CVPrec + SDTemp + (1 | ID1),data=isdataScTS[!(rownames(isdataScTS) %in% rem),])
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# alternatives: Temp and Precip in random effects

AmBEST31<-lmer(log(isdataTS$Yield[!(rownames(isdataScTS) %in% rem)])~SeasPr + I(SeasPr^2) + I(SeasPr * AvgTemp) + 
                 AvgTemp + CVPrec + SDTemp + (1+SeasPr+AvgTemp | ID1),data=isdataScTS[!(rownames(isdataScTS) %in% rem),])
summary(AmBEST31)

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# now the arma errors:
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

# 1. all observations

isdataScTS$Yield0<-isdataTS$Yield

reML<-lme(log(Yield0)~SeasPr + I(SeasPr^2) + I(SeasPr * AvgTemp) + 
             AvgTemp + CVPrec + SDTemp, random= ~1 | ID1,na.action=na.exclude,
           data=isdataScTS)

AmArE<-lme(log(Yield0)~SeasPr + I(SeasPr^2) + I(SeasPr * AvgTemp) + 
               AvgTemp + CVPrec + SDTemp, random= ~1 | ID1,correlation=corAR1(0,form=~as.numeric(Year)|ID1) ,
           data=isdataScTS,na.action=na.exclude)

summary(AmArE)

anova(reML,AmArE)
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

AmArE01<-lme(log(Yield0)~SeasPr + I(SeasPr^2) + I(SeasPr * AvgTemp) + 
             AvgTemp + CVPrec + SDTemp, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=0,q=1),
           data=isdataScTS,na.action=na.exclude)

summary(AmArE01)

anova(reML,AmArE01)

AmArE11<-lme(log(Yield0)~SeasPr + I(SeasPr^2) + I(SeasPr * AvgTemp) + 
               AvgTemp + CVPrec + SDTemp, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
             data=isdataScTS,na.action=na.exclude)

summary(AmArE11)

anova(AmArE,AmArE11)

AmArE21<-lme(log(Yield0)~SeasPr + I(SeasPr^2) + I(SeasPr * AvgTemp) + 
               AvgTemp + CVPrec + SDTemp, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=2,q=1),
             data=isdataScTS,na.action=na.exclude)

summary(AmArE21)

anova(AmArE,AmArE21)


AmArE12<-lme(log(Yield0)~SeasPr + I(SeasPr^2) + I(SeasPr * AvgTemp) + 
               AvgTemp + CVPrec + SDTemp, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=2),
             data=isdataScTS,na.action=na.exclude)

summary(AmArE12)

anova(AmArE,AmArE12)



AmArE22<-lme(log(Yield0)~SeasPr + I(SeasPr^2) + I(SeasPr * AvgTemp) + 
               AvgTemp + CVPrec + SDTemp, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=2,q=2),
             data=isdataScTS,na.action=na.exclude)

summary(AmArE22)

anova(AmArE,AmArE22)


AmArE32<-lme(log(Yield0)~SeasPr + I(SeasPr^2) + I(SeasPr * AvgTemp) + 
               AvgTemp + CVPrec + SDTemp, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=3,q=2),
             data=isdataScTS,na.action=na.exclude)

summary(AmArE32)

anova(AmArE,AmArE32)


AmArE23<-lme(log(Yield0)~SeasPr + I(SeasPr^2) + I(SeasPr * AvgTemp) + 
               AvgTemp + CVPrec + SDTemp, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=2,q=3),
             data=isdataScTS,na.action=na.exclude)
summary(AmArE23)  # no convergence
anova(AmArE,AmArE23)


AmArE33<-lme(log(Yield0)~SeasPr + I(SeasPr^2) + I(SeasPr * AvgTemp) + 
               AvgTemp + CVPrec + SDTemp, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=2,q=3),
             data=isdataScTS,na.action=na.exclude)
summary(AmArE33)  # no convergence
anova(AmArE,AmArE33)

anova(AmArE,AmArE21,AmArE22,AmArE32,AmArE33)

anova(AmArE21,AmArE22)
anova(AmArE21,AmArE12)
anova(AmArE12,AmArE22)
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# 2. without the residual outliers

isdataScTS$Yield0<-isdataTS$Yield

re1ML<-lme(log(Yield0)~SeasPr + I(SeasPr^2) + I(SeasPr * AvgTemp) + 
            AvgTemp + CVPrec + SDTemp, random= ~1 | ID1,
          data=isdataScTS[!(rownames(isdataScTS) %in% rem),],na.action=na.exclude)

AmAr1E<-lme(log(Yield0)~SeasPr + I(SeasPr^2) + I(SeasPr * AvgTemp) + 
             AvgTemp + CVPrec + SDTemp, random= ~1 | ID1,correlation=corAR1(0,form=~as.numeric(Year)|ID1) ,
           data=isdataScTS[!(rownames(isdataScTS) %in% rem),],na.action=na.exclude)
summary(AmAr1E)

anova(re1ML,AmAr1E)

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
AmAr1E01<-lme(log(Yield0)~SeasPr + I(SeasPr^2) + I(SeasPr * AvgTemp) + 
               AvgTemp + CVPrec + SDTemp, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=0,q=1),
             data=isdataScTS[!(rownames(isdataScTS) %in% rem),],na.action=na.exclude)

summary(AmAr1E01)

anova(re1ML,AmAr1E01)

AmAr1E11<-lme(log(Yield0)~SeasPr + I(SeasPr^2) + I(SeasPr * AvgTemp) + 
               AvgTemp + CVPrec + SDTemp, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
             data=isdataScTS[!(rownames(isdataScTS) %in% rem),],na.action=na.exclude)

summary(AmAr1E11)

anova(AmAr1E,AmAr1E11)

AmAr1E21<-lme(log(Yield0)~SeasPr + I(SeasPr^2) + I(SeasPr * AvgTemp) + 
               AvgTemp + CVPrec + SDTemp, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=2,q=1),
             data=isdataScTS[!(rownames(isdataScTS) %in% rem),],na.action=na.exclude)
summary(AmAr1E21)

anova(AmAr1E,AmAr1E21)


AmAr1E12<-lme(log(Yield0)~SeasPr + I(SeasPr^2) + I(SeasPr * AvgTemp) + 
               AvgTemp + CVPrec + SDTemp, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=2),
             data=isdataScTS[!(rownames(isdataScTS) %in% rem),],na.action=na.exclude)

summary(AmAr1E12)

anova(AmAr1E,AmAr1E12)



AmAr1E22<-lme(log(Yield0)~SeasPr + I(SeasPr^2) + I(SeasPr * AvgTemp) + 
               AvgTemp + CVPrec + SDTemp, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=2,q=2),
             data=isdataScTS[!(rownames(isdataScTS) %in% rem),],na.action=na.exclude)

summary(AmAr1E22)

anova(AmAr1E,AmAr1E22)


AmAr1E32<-lme(log(Yield0)~SeasPr + I(SeasPr^2) + I(SeasPr * AvgTemp) + 
               AvgTemp + CVPrec + SDTemp, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=3,q=2),
             data=isdataScTS[!(rownames(isdataScTS) %in% rem),],na.action=na.exclude)
summary(AmAr1E32)
anova(AmAr1E,AmAr1E32)


AmAr1E23<-lme(log(Yield0)~SeasPr + I(SeasPr^2) + I(SeasPr * AvgTemp) + 
               AvgTemp + CVPrec + SDTemp, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=2,q=3),
             data=isdataScTS[!(rownames(isdataScTS) %in% rem),],na.action=na.exclude)
summary(AmAr1E23)  # no convergence
anova(AmAr1E,AmAr1E23)


AmAr1E33<-lme(log(Yield0)~SeasPr + I(SeasPr^2) + I(SeasPr * AvgTemp) + 
               AvgTemp + CVPrec + SDTemp, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=2,q=3),
             data=isdataScTS[!(rownames(isdataScTS) %in% rem),],na.action=na.exclude)
summary(AmAr1E33)  # no convergence
anova(AmAr1E,AmAr1E33)

anova(AmAr1E,AmAr1E21,AmAr1E22,AmAr1E32)

anova(AmAr1E21,AmAr1E22)
anova(AmAr1E21,AmAr1E12)
anova(AmAr1E22,AmAr1E12)

# seems to be the best!!!  ARMA21
plot(AmAr1E21)
plot(AmBEST31)


#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

#the best
AmArE21<-lme(log(Yield0)~SeasPr + I(SeasPr^2) + I(SeasPr * AvgTemp) + 
               AvgTemp + CVPrec + SDTemp, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=2,q=1),
             data=isdataScTS,na.action=na.exclude)

summary(AmArE21)
anova(AmArE21)
drop1(AmArE21)
#for show purpose
plot(function(x) exp(0.159*x)*exp(-0.079*(x)^2),-1.6,2.60   ,ylab="Effect on yield (multiplicative)",xlab="Precipitation (standardised)" )

AmArE2100<-lme(log(Yield0)~SeasPr + I(SeasPr^2) + AvgTemp, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=2,q=1),
             data=isdataScTS,na.action=na.exclude)

summary(AmArE2100)
anova(AmArE2100)

AmArE2199<-lme(log(Yield0)~SeasPr + I(SeasPr^2) + AvgTemp+ I(SeasPr * AvgTemp) + 
                 CVPrec + SDTemp + Prec2m+Spell+Spell10+MaxP+DDays+ HWDays + MaxT
               , random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=2,q=1),
               data=isdataScTS,na.action=na.exclude)

summary(AmArE2199)
anova(AmArE2199)