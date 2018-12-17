rm(list=ls())
library(MASS);library(dplyr); library(tseries); library(plm); library(nlme); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)
load("dataFS/Main/DaTS.RData")

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# these models are the best ones for now:

KEN11d<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
            +AvgTemp + CVTempK, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
            data=ScaledTS,na.action=na.exclude); summary(KEN11a); exp(summary(KEN11a)$coef[[1]])

fullishML<-lme(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec+Spell+Spell4 +MaxP
                 + CVTempK + DDays, random= ~1 | ID1,method="ML",
            data=ScaledTS,na.action=na.exclude); summary(fullishLM); exp(summary(fullishLM)$coef[[1]])

    CaryML_stepAIC<-stepAIC(fullishML)
    summary(CaryML_stepAIC)
          CaryML_step<-step(fullishML) # step not working for lme??
          
                                      # both step and stepAIC undefined for REML fit
                                              fullishRELM<-lme(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec+Spell+Spell4 +MaxP
                                                           + CVTempK + DDays, random= ~1 | ID1,method="REML",
                                                           data=ScaledTS,na.action=na.exclude); summary(fullishRELM); exp(summary(fullishRELM)$coef[[1]])
                                              CaryREML_stepAIC<-step(fullishRELM); CaryREML_stepAIC<-step(fullishRELM)
                                      # both step and stepAIC undefined for REML fit
                                              

fullish_lmer<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec+Spell+Spell4 +MaxP
              + CVTempK + DDays +(1|ID1),data=ScaledTS) #

Cary_stepAIC<-stepAIC(fullish_lmer)


summary(get_model(Cary))

drop1(fullish)
summary(get_model(Kalinda))
summary(get_model(Cary))
extractAIC(get_model(Cary));extractAIC(get_model(Kalinda)) # WOW, Kalinda lower AIC here

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

full<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec+Spell+Spell4 +MaxP
              + CVTempK + DDays +HWDays+MaxT+(1|ID1),data=ScaledTS) #

Cary2<-step(full, ddf = c("Satterthwaite"), keep=attr(terms(full), "term.labels")[1:3])
summary(get_model(Cary2))   ;extractAIC(get_model(Cary2));extractAIC(get_model(Kalinda))

Cary3<-step(full, ddf = c("Satterthwaite"), keep=attr(terms(full), "term.labels")[1:2])
summary(get_model(Cary3))   ;extractAIC(get_model(Cary3));extractAIC(get_model(Kalinda))

#------------------------------------------------------

full<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec+Spell+Spell4 +MaxP
           + CVTempK + DDays +HWDays+(1|ID1),data=ScaledTS) #

Cary2<-step(full, ddf = c("Satterthwaite"), keep=attr(terms(full), "term.labels")[1:3])
summary(get_model(Cary2))  ;extractAIC(get_model(Cary2));extractAIC(get_model(Kalinda))

Cary3<-step(full, ddf = c("Satterthwaite"), keep=attr(terms(full), "term.labels")[1:2])
summary(get_model(Cary3))  ;extractAIC(get_model(Cary3));extractAIC(get_model(Kalinda))
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo


save.image("Rcodes/December18/Models1step.RData")