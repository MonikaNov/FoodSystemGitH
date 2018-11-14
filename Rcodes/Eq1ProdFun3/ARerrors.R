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
# Kyle2s and Kyle51sw are the best ->> following is based on Kyle2s and Kyle51sw 
#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

# 1. Unweighted

Kyle12<-lme(Yield~1+PrecZscore +TempZscore,random= ~PrecZscore+TempZscore|ID 
            ,data=CrMaize8,control = lmeControl(opt = "optim"),correlation=corAR1(0,form= ~ as.numeric(Year)|ID))
summary(Kyle12) 
anova(Kyle12)
coef(Kyle12$modelStruct$corStruct, unconstrained = FALSE)
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# LR test for serial correlation and random effects
# I first need to estimate some more models.

Kyle129<-gls(Yield~1+PrecZscore +TempZscore ,data=CrMaize8,control = lmeControl(opt = "optim"))
summary(Kyle129) 

Kyle1295<-gls(Yield~1+PrecZscore +TempZscore ,data=CrMaize8,control = lmeControl(opt = "optim")
              ,correlation=corAR1(0,form= ~ as.numeric(Year)|ID))

anova(Kyle129,Kyle1295)   #  1)

#-------------
Kyle012<-lme(Yield~PrecZscore +TempZscore,random= ~1+PrecZscore+TempZscore|ID ,data=CrMaize8,control = lmeControl(opt = "optim"))
 
anova(Kyle012,Kyle12)    #  2)

#-------------
anova(Kyle129,Kyle012)     # 3)
anova(Kyle1295,Kyle12)     # 4)
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# so Kyle12 is the best so far

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

# 2. Weighted

Kyle151w<-lme(Yield~1+PrecZscore +PrecZonVar,random= ~1+PrecZscore+TempZscore|ID,weights=~AreaSc  ,data=CrMaize8,
              correlation=corAR1(0,form= ~ as.numeric(Year)|ID))
summary(Kyle151w)
anova(Kyle151w)
coef(Kyle151w$modelStruct$corStruct, unconstrained = FALSE)
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# LR test for serial correlation and random effects
# I first need to estimate some more models.

Kyw0<-gls(Yield~1+PrecZscore +PrecZonVar,weights=~AreaSc  ,data=CrMaize8)

Kyw1<-gls(Yield~1+PrecZscore +PrecZonVar,weights=~AreaSc  ,data=CrMaize8,
             correlation=corAR1(0,form= ~ as.numeric(Year)|ID))

anova(Kyw0,Kyw1) #  1)

Kyw3<-lme(Yield~1+PrecZscore +PrecZonVar,random= ~1+PrecZscore+TempZscore|ID,weights=~AreaSc  ,data=CrMaize8)

anova(Kyle151w,Kyw3) #  2)
#-------------

anova(Kyw0,Kyw3) #  3)

anova(Kyle151w,Kyw1) #  4)

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# so Kyle151w is the best unweighted so far