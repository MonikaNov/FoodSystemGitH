rm(list=ls())
library(MASS);library(dplyr); library(tseries); library(plm); library(nlme); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)
load("dataFS/Main/DaTS.RData")
# load("Rcodes/DecemberNew/KEN11d_re.RData")
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# these models are the best ones for now:

KEN11d<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
            +AvgTemp + CVTempK, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
            data=ScaledTS,na.action=na.exclude); summary(KEN11d); exp(summary(KEN11d)$coef[[1]])
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# drop1 undefined for restricted ML, have to refit with ML

KEN11d_re1<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
            +AvgTemp + CVTempK, random= ~1+SeasPr+AvgTemp | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
            control=lmeControl(maxIter=300,msMaxIter=300, msMaxEval=500),
            data=ScaledTS,na.action=na.exclude); summary(KEN11d_re1) # singular convergence

#ehm..the following seems to be going...
KEN11d_re1<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
                    +AvgTemp + CVTempK, random= ~CVTempK | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
                    control=lmeControl(maxIter=300,msMaxIter=300, msMaxEval=500),
                    data=ScaledTS,na.action=na.exclude);summary(KEN11d_re1) # this is the same as if:  random= ~CVTempK | ID1,
  anova(KEN11d,KEN11d_re1)
  ranova(KEN11d,KEN11d_re1)
  ranova(KEN11d,KEN11d_re1,reduce.terms=FALSE)

KEN11d_re2<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
                +AvgTemp + CVTempK, random= ~CVTempK-1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
                control=lmeControl(maxIter=300,msMaxIter=300, msMaxEval=500),
                data=ScaledTS,na.action=na.exclude);summary(KEN11d_re2)
anova(KEN11d,KEN11d_re2)
ranova(KEN11d,KEN11d_re2,reduce.terms=FALSE)

KEN11d_re1<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
                +AvgTemp + CVTempK, random= ~1+SeasPr+AvgTemp | ID1,
                control=lmeControl(maxIter=300,msMaxIter=300, msMaxEval=500),
                data=ScaledTS,na.action=na.exclude); summary(KEN11d_re1) # not working...

# so I have to do it through lmer...
# - -  --  - - - - - - ------- - - - - - - - -- - - - -- - - - -- - - -- - - -  --  - - - - - - ------- - - - - - - - -- - - - -- - - - -- - - -- - - -  --  - - - - - - ------- - - - - - - - -- - - - -- - - - -- - - -- - 
KENd_lmer<-lmer(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4+AvgTemp + CVTempK+(1 | ID1),data=ScaledTS,na.action=na.exclude)
step(KENd_lmer)

KENd_lmer_reAll<-lmer(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4+AvgTemp + CVTempK+(SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4+AvgTemp + CVTempK | ID1),data=ScaledTS,na.action=na.exclude)
KENd_lmer_reAll_step<-step(KENd_lmer_reAll,reduce.fixed=FALSE)

# welp, it seems that CVTemp in random effects could improve the fit a liitle bit. let's see:

summary(get_model(KENd_lmer_reAll_step))
summary(KENd_lmer); 
extractAIC(KENd_lmer);  extractAIC(get_model(KENd_lmer_reAll_step))

anova(get_model(KENd_lmer_reAll_step),KENd_lmer)

KEN11m<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
            +AvgTemp + CVTempK, random= ~(CVTempK) | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
            data=ScaledTS,na.action=na.exclude); summary(KEN11m); exp(summary(KEN11m)$coef[[1]])

KEN11n<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
            +AvgTemp + CVTempK, random= ~CVTempK | ID1,
            data=ScaledTS,na.action=na.exclude); summary(KEN11m) 
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

save.image("Rcodes/DecemberNew/KEN11d_re.RData")