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
#---------------------------------------------------------------------------------------------------------------------------------------------------------------
#


KyW22<-lme(Yield~1+PrecZscore +PrecZonVar,random= ~1+PrecZscore+TempZscore|ID,weights=~AreaSc  ,data=CrMaize8,
           correlation=corARMA(form = ~ as.numeric(Year)|ID, p=2,q=2))
summary(KyW22) 

coef(KyW22$modelStruct$corStruct, unconstrained = FALSE)

KyW22noRandom<-gls(Yield~1+PrecZscore +PrecZonVar,weights=~AreaSc  ,data=CrMaize8,
           correlation=corARMA(form = ~ as.numeric(Year)|ID, p=2,q=2))
summary(KyW22noRandom) 

anova(KyW22noRandom,KyW22) 


KyW22noRandomNoCor<-gls(Yield~1+PrecZscore +PrecZonVar,weights=~AreaSc  ,data=CrMaize8,)
summary(KyW22noRandomNoCor) 
anova(KyW22noRandomNoCor,KyW22noRandom) 
#---------------------------------------------------------------------------------------------------------------------------------------------------------------
Kyle151w<-lme(Yield~1+PrecZscore +PrecZonVar,random= ~1+PrecZscore+TempZscore|ID,weights=~AreaSc  ,data=CrMaize8,
              correlation=corAR1(0,form= ~ as.numeric(Year)|ID)) # original
anova(Kyle151w,KyW22) 
summary(KyW22) 
summary(Kyle151w) 
summary(Kyle51sw)

# show these pictures !!!!!!!!

plot(KyW22) 
plot(Kyle151w) 
plot(Kyle51sw)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
##good, KyW22 is the best

