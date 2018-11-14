rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

load("Main/CrMaize8.RData")
library(nlme)
library(lme4)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------


Kyle151w<-lme(Yield~1+PrecZscore +PrecZonVar,random= ~1+PrecZscore+TempZscore|ID,weights=~AreaSc  ,data=CrMaize8,
              correlation=corAR1(0,form= ~ as.numeric(Year)|ID))
summary(Kyle151w)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------

KyW22<-lme(Yield~1+PrecZscore +PrecZonVar,random= ~1+PrecZscore+TempZscore|ID,weights=~AreaSc  ,data=CrMaize8,
           correlation=corARMA(form = ~ as.numeric(Year)|ID, p=2,q=2))
summary(KyW22) 
anova(KyW22)
anova(KyW22,Kyle151w)

KyW21<-lme(Yield~1+PrecZscore +PrecZonVar,random= ~1+PrecZscore+TempZscore|ID,weights=~AreaSc  ,data=CrMaize8,
          correlation=corARMA(form = ~ as.numeric(Year)|ID, p=2,q=1))

KyW12<-lme(Yield~1+PrecZscore +PrecZonVar,random= ~1+PrecZscore+TempZscore|ID,weights=~AreaSc  ,data=CrMaize8,
           correlation=corARMA(form = ~ as.numeric(Year)|ID, p=1,q=2))

KyW11<-lme(Yield~1+PrecZscore +PrecZonVar,random= ~1+PrecZscore+TempZscore|ID,weights=~AreaSc  ,data=CrMaize8,
           correlation=corARMA(form = ~ as.numeric(Year)|ID, p=1,q=1))

KyW01<-lme(Yield~1+PrecZscore +PrecZonVar,random= ~1+PrecZscore+TempZscore|ID,weights=~AreaSc  ,data=CrMaize8,
           correlation=corARMA(form = ~ as.numeric(Year)|ID, p=0,q=1))

anova(KyW22,KyW21)
anova(KyW22,KyW12)
anova(KyW22,KyW11)
anova(KyW22,KyW01)


KyW23<-lme(Yield~1+PrecZscore +PrecZonVar,random= ~1+PrecZscore+TempZscore|ID,weights=~AreaSc  ,data=CrMaize8,
           correlation=corARMA(form = ~ as.numeric(Year)|ID, p=2,q=3))
KyW24<-lme(Yield~1+PrecZscore +PrecZonVar,random= ~1+PrecZscore+TempZscore|ID,weights=~AreaSc  ,data=CrMaize8,
           correlation=corARMA(form = ~ as.numeric(Year)|ID, p=2,q=4))

KyW32<-lme(Yield~1+PrecZscore +PrecZonVar,random= ~1+PrecZscore+TempZscore|ID,weights=~AreaSc  ,data=CrMaize8,
           correlation=corARMA(form = ~ as.numeric(Year)|ID, p=3,q=2))
anova(KyW22,KyW32)

KyW42<-lme(Yield~1+PrecZscore +PrecZonVar,random= ~1+PrecZscore+TempZscore|ID,weights=~AreaSc  ,data=CrMaize8,
           correlation=corARMA(form = ~ as.numeric(Year)|ID, p=4,q=2))
anova(KyW22,KyW42)

KyW33<-lme(Yield~1+PrecZscore +PrecZonVar,random= ~1+PrecZscore+TempZscore|ID,weights=~AreaSc  ,data=CrMaize8,
           correlation=corARMA(form = ~ as.numeric(Year)|ID, p=3,q=3))
anova(KyW22,KyW33)

anova(KyW22,KyW33)

KyW44<-lme(Yield~1+PrecZscore +PrecZonVar,random= ~1+PrecZscore+TempZscore|ID,weights=~AreaSc  ,data=CrMaize8,
           correlation=corARMA(form = ~ as.numeric(Year)|ID, p=4,q=4))
anova(KyW22,KyW44)

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
# now the formal LR tests procedure

#KyW22 seems to be the best

KyW22<-lme(Yield~1+PrecZscore +PrecZonVar,random= ~1+PrecZscore+TempZscore|ID,weights=~AreaSc  ,data=CrMaize8,
           correlation=corARMA(form = ~ as.numeric(Year)|ID, p=2,q=2))

#---------------------------------------------------------------------------------------------------------------------------------------------------------------

KyW22noRandom<-gls(Yield~1+PrecZscore +PrecZonVar,weights=~AreaSc  ,data=CrMaize8,
                   correlation=corARMA(form = ~ as.numeric(Year)|ID, p=2,q=2))

KyW00g<-gls(Yield~1+PrecZscore +PrecZonVar,weights=~AreaSc  ,data=CrMaize8)

anova(KyW22noRandom,KyW00g)    #  1)

KyW00<-lme(Yield~1+PrecZscore +PrecZonVar,random= ~1+PrecZscore+TempZscore|ID,weights=~AreaSc  ,data=CrMaize8)

anova(KyW00,KyW22)    #  2)

anova(KyW00,KyW00g)    #  3)

anova(KyW22noRandom,KyW22)    #  4)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------

KyW21g<-gls(Yield~1+PrecZscore +PrecZonVar,weights=~AreaSc  ,data=CrMaize8,
                   correlation=corARMA(form = ~ as.numeric(Year)|ID, p=2,q=1))

anova(KyW22noRandom,KyW21g)   #ARGGGGGGA  #  5)  MY buut these have no RE

KyW12g<-gls(Yield~1+PrecZscore +PrecZonVar,weights=~AreaSc  ,data=CrMaize8,
            correlation=corARMA(form = ~ as.numeric(Year)|ID, p=1,q=2))

anova(KyW22noRandom,KyW12g)  #ARGGGGGGA  #  5b)  MY buut these have no RE

KyW11g<-gls(Yield~1+PrecZscore +PrecZonVar,weights=~AreaSc  ,data=CrMaize8,
            correlation=corARMA(form = ~ as.numeric(Year)|ID, p=1,q=1))

anova(KyW22noRandom,KyW11g) # 5b)  MY: GOOD

#----------------

# I think that I have done analogous remaining