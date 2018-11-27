rm(list=ls())
library(ggeffects);library(dplyr); library(tseries); library(plm); library(nlme); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)
load("dataFS/Main/DaTS.RData")

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# these models are the best ones for now:

KEN11a<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
            +AvgTemp + SDTemp, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
            data=ScaledTS,na.action=na.exclude); summary(KEN11a)
# and subsamples
KEN11a_ASAL<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
                 +AvgTemp + SDTemp, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
                 data=ScaledTS[ScaledTS$ASAL==1,],na.action=na.exclude); summary(KEN11a_ASAL)

KEN11a_nonASAL<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
                    +AvgTemp + SDTemp, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
                    data=ScaledTS[ScaledTS$ASAL==0,],na.action=na.exclude); summary(KEN11a_nonASAL)

#---------------------------------------------------------------------------------
test<-ggeffect(KEN11a)
test
exp(summary(KEN11a)$tTable[,1]);ranef(KEN11a)[1]

# and subsamples

test2<-ggpredict(KEN11a);test2
test4<-ggaverage(KEN11a);test4


KenTest<-lme(Yield~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
                  +AvgTemp + SDTemp, random= ~1 | ID1,data=DaTS,na.action=na.exclude); summary(KenTest); 
summary(KenTest)$tTable[,1]; ranef(KenTest)

test1<-ggeffect(KenTest,terms=c("AvgTemp"));test1
test<-ggpredict(KenTest,terms=c("AvgTemp"));test
te<-ggpredict(KenTest,terms=c("Spell"));te
te<-ggpredict(KenTest,terms=c("AvgTemp"));te
hist(ScaledTS$AvgTemp,40)

KenTe<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
             +AvgTemp + SDTemp, random= ~1 | ID1,data=ScaledTS,na.action=na.exclude); summary(KenTe); 
exp(summary(KenTe)$tTable[,1]); ranef(KenTe)

ete<-ggpredict(KenTe);ete
ete<-ggpredict(KenTe,terms=c("AvgTemp"));ete
efe<-ggeffect(KenTe,terms=c("AvgTemp","ID1"));efe


#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# back to the real model

KEN11a<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
            +AvgTemp + SDTemp, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
            
kene1<-ggpredict(KEN11a);
kene2<-ggpredict(KEN11a,terms=c("AvgTemp")); kene2
kene3<-ggeffect(KEN11a,terms=c("AvgTemp")); kene3            
            data=ScaledTS,na.action=na.exclude); summary(KEN11a)