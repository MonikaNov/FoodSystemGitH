rm(list=ls())
library(MASS);library(dplyr); library(tseries); library(plm); library(nlme); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)
load("dataFS/Main/DaTS.RData")
# load("Rcodes/DecemberNew/KEN11d_stepNice.RData")
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# these models are the best ones for now:

KEN11d<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
            +AvgTemp + CVTempK, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
            data=ScaledTS,na.action=na.exclude); summary(KEN11a); exp(summary(KEN11a)$coef[[1]])

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# what I can do with step and stepAIC (each of them works for some cases, here are ONLY those which work...and then theor varieties which I will use..)

fullishML<-lme(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec+Spell+Spell4 +MaxP
               + CVTempK + DDays, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),method="ML",
               data=ScaledTS,na.action=na.exclude); summary(fullishLM); exp(summary(fullishLM)$coef[[1]])
  CaryML_stepAIC<-stepAIC(fullishML)
  summary(CaryML_stepAIC) #..>> GOOD THE SAME PREDICTORS IN AS I HAD CHOSEN BEFORE...GROOT
                      vif(CaryML_stepAIC)
# .... . . . .  .... . . . . . . . . . ..    .... . . . .  .... . . . . . . . . . ..    .... . . . .  .... . . . . . . . . . ..   
# now the step method for lmer
fullish_lmer<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec+Spell+Spell4 +MaxP
                     + CVTempK + DDays +(1|ID1),data=ScaledTS) 
  Cary_step2<-step(fullish_lmer, keep=attr(terms(fullish_lmer), "term.labels")[1:3])
  summary(get_model(Cary_step2))  # v. nice VERY SIMILAR RESULTS TO CaryML_stepAIC. Perhaps the difference is just the pattern in autocorrelation in errorrs.

  
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# now I want to test two things for each of the models above..
    # 1. what happens if I start with the full full set of variables-that means including HWDays and MaxT??
    # 2. what happens if I start with all vars. in random effects?? theis may take a while..

# .... . . . .  .... . . . . . . . . . ..    .... . . . .  .... . . . . . . . . . ..    .... . . . .  .... . . . . . . . . . ..   
# 1.   

fullML<-lme(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec+Spell+Spell4 +MaxP
                 + CVTempK + DDays+HWDays+MaxT, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),method="ML",
                 data=ScaledTS,na.action=na.exclude); summary(fullML); exp(summary(fullML)$coef[[1]])
  CaryML_stepAICfull<-stepAIC(fullML)
  summary(CaryML_stepAICfull); vif(CaryML_stepAICfull)

# . .        . . .              . . . .       . . ..    .                     .... . .     .

full_lmer<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec+Spell+Spell4 +MaxP
                + CVTempK + DDays +HWDays+MaxT+(1|ID1),data=ScaledTS) 
  Cary_step_full<-step(full_lmer, keep=attr(terms(full_lmer), "term.labels")[1:3])
  summary(get_model(Cary_step_full))  #   navic je tu MaxT a chybi spells..
# see file KEN11d_stepNicePlaying.R
  
# .... . . . .  .... . . . . . . . . . ..    .... . . . .  .... . . . . . . . . . ..    .... . . . .  .... . . . . . . . . . ........................................   
#   2. The random effects:
  
full_lmer2_re<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec+Spell+Spell4 +MaxP
                 + CVTempK + DDays +HWDays+(1+SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec+Spell+Spell4 +MaxP
                 + CVTempK + DDays +HWDays|ID1),data=ScaledTS) ;summary(full_lmer2_re)
  step_full_lmer2_re<-step(full_lmer2_re, keep=attr(terms(full_lmer2_re), "term.labels")[1:3])
  summary(get_model(step_full_lmer2_re))


fullML_re<-lme(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec+Spell+Spell4 +MaxP
            + CVTempK + DDays+HWDays, random= ~1+SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec+Spell+Spell4 +MaxP
            + CVTempK + DDays+HWDays | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),method="ML",
            data=ScaledTS,na.action=na.exclude); summary(fullML_re); exp(summary(fullML_re)$coef[[1]])
CaryML_stepAIC_re<-stepAIC(fullML_re)


# iterration limit withou convergence>>make it a bit smaller:

fullML_re2<-lme(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+CVPrec+Spell+Spell4 +MaxP
            + CVTempK + DDays, random= ~1+SeasPr+AvgTemp+I(SeasPr^2)+CVPrec+Spell+Spell4 +MaxP
            + CVTempK + DDays| ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),method="ML",
            data=ScaledTS,na.action=na.exclude); summary(fullML_re); exp(summary(fullML_re)$coef[[1]])
CaryML_stepAIC_re2<-stepAIC(fullML_re2)

myoption <- lmeControl(opt='optim')

fullML_re2<-lme(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+CVPrec+Spell+Spell4 +MaxP
            + CVTempK, random= ~1+SeasPr+AvgTemp+Spell+MaxP| ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),method="ML",
            data=ScaledTS,na.action=na.exclude,control=lmeControl(maxIter=300,msMaxIter=300,msMaxEval=500)); summary(fullML_re2); exp(summary(fullML_re2)$coef[[1]])
CaryML_stepAIC_re2<-stepAIC(fullML_re2)


fullML_re3<-lme(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+CVPrec+Spell+Spell4 +MaxP
            + CVTempK + DDays, random= ~1+SeasPr+AvgTemp+I(SeasPr^2)+CVPrec+Spell+Spell4 +MaxP
            + CVTempK + DDays| ID1,method="ML",
            data=ScaledTS,na.action=na.exclude,control=lmeControl(maxIter=300,msMaxIter=300,msMaxEval=500)); summary(fullML_re3); exp(summary(fullML_re3)$coef[[1]])
CaryML_stepAIC_re3<-stepAIC(fullML_re3)


save.image("\\\\smbhome.uscs.susx.ac.uk\\mn301\\FoodSystemGitH\\Rcodes\\DecemberNew\\KEN11d_stepNice3.RData")


summary(CaryML_stepAIC_re); vif(CaryML_stepAIC_re)
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
save.image("Rcodes/DecemberNew/KEN11d_stepNice2.RData")