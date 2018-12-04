rm(list=ls())
library(dplyr); library(tseries); library(plm); library(nlme); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)
load("dataFS/Main/DaTS.RData")

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# these models are the best ones for now:

KEN11a<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
            +AvgTemp + SDTemp, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
            data=ScaledTS,na.action=na.exclude); summary(KEN11a); exp(summary(KEN11a)$coef[[1]])
# and subsamples
      KEN11a_ASAL<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
                       +AvgTemp + SDTemp, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
                       data=ScaledTS[ScaledTS$ASAL==1,],na.action=na.exclude); summary(KEN11a_ASAL); exp(summary(KEN11a_ASAL)$coef[[1]])
      
      KEN11a_nonASAL<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
                          +AvgTemp + SDTemp, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
                          data=ScaledTS[ScaledTS$ASAL==0,],na.action=na.exclude); summary(KEN11a_nonASAL); exp(summary(KEN11a_nonASAL)$coef[[1]])
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
      
# so I can try to add the new variables:
      
Logan1<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
              +AvgTemp + SDTemp+cum95, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
              data=ScaledTS,na.action=na.exclude); summary(Logan1)
anova(KEN11a,Logan1)

Logan1<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
            +AvgTemp + SDTemp+cum99, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
            data=ScaledTS,na.action=na.exclude); summary(Logan1)

vif(Logan1)




Logan12<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
            +AvgTemp + SDTemp+cum95, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
            data=ScaledTS,na.action=na.exclude); summary(Logan1)

vif(Logan12)
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# and adding into RE

Logan5<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
            +AvgTemp + SDTemp, random= ~1+cum95 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
            data=ScaledTS,na.action=na.exclude); summary(Logan5)

anova(KEN11a,Logan5)

Logan52<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
            +AvgTemp + SDTemp+cum95, random= ~1+cum95 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
            data=ScaledTS,na.action=na.exclude); summary(Logan5)

anova(Logan12,Logan52)

ranova(Logan5)

# to apply ranova method, I will have to refit it with lmer
LoganB0<-lmer(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4 +AvgTemp + SDTemp+(1| ID1),
             data=ScaledTS,na.action=na.exclude); summary(LoganB0)

ranova(LoganB0)

LoganB<-lmer(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4 +AvgTemp + SDTemp+cum95+(1| ID1),
             data=ScaledTS,na.action=na.exclude); summary(LoganB)

ranova(LoganB)
anova(LoganB0,LoganB)

LoganB1<-lmer(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4 +AvgTemp + SDTemp+(cum95| ID1),
             data=ScaledTS,na.action=na.exclude); summary(LoganB1)

ranova(LoganB1)


LoganB2<-lmer(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4 +AvgTemp + SDTemp+(0+cum95 | ID1),
             data=ScaledTS,na.action=na.exclude); summary(LoganB2)
 
ranova(LoganB2,reduce.terms=FALSE)
anova(LoganB1,LoganB2)


LoganB3<-lmer(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4 +AvgTemp + SDTemp+(1 | ID1),
              data=ScaledTS,na.action=na.exclude); summary(LoganB3)

ranova(LoganB3,reduce.terms=FALSE)

anova(LoganB1,LoganB3)
anova(LoganB2,LoganB)