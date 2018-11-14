rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

load("Main/CrMaize8.RData")
load("Main/CrMaize9.RData")
library(nlme)
library(lme4)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# The best models: Kyle2s and Kyle51sw...
#
Kyle51sw<-lmer(Yield~PrecZscore+PrecZonVar+
                 (PrecZscore+TempZscore|ID),weights=AreaSc, data=CrMaize8) 
summary(Kyle51sw)

Kyle2s<-lmer(Yield~PrecZscore +TempZscore  +  (PrecZscore+TempZscore|ID),data=CrMaize8)
summary(Kyle2s) # quite similar to Kyle 1s >>good


#---------------------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------------------------------------
#
Kyle012<-lme(Yield~PrecZscore +TempZscore,random= ~1+PrecZscore+TempZscore|ID ,data=CrMaize8,control = lmeControl(opt = "optim"))
summary(Kyle012) 

Kyle12<-lme(Yield~1+PrecZscore +TempZscore,random= ~PrecZscore+TempZscore|ID 
            ,data=CrMaize8,control = lmeControl(opt = "optim"),correlation=corAR1(0,form= ~ as.numeric(Year)|ID))
summary(Kyle12) 
anova(Kyle12) 

coef(Kyle12$modelStruct$corStruct, unconstrained = FALSE)

#AIC better (=lower) for AR1>v.good
#-------------------------#-------------------------#-------------------------
# LR test for serial correlation:
Kyle129<-gls(Yield~1+PrecZscore +TempZscore ,data=CrMaize9,control = lmeControl(opt = "optim"))
summary(Kyle129) 

anova(Kyle129,Kyle12)

# and some more tests:
anova(Kyle012,Kyle12)

anova(Kyle129,Kyle012)


Kyle1295<-gls(Yield~1+PrecZscore +TempZscore ,data=CrMaize9,control = lmeControl(opt = "optim")
             ,correlation=corAR1(0,form= ~ as.numeric(Year)|ID))
summary(Kyle1295) 

anova(Kyle1295,Kyle12) # see page 37,Croissant and Millo (2008), Journal of Statistical Software
#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# now the weights...

Kyle051w<-lme(Yield~PrecZscore +PrecZonVar,random= ~1+PrecZscore+TempZscore|ID,weights=~AreaSc ,data=CrMaize8,control = lmeControl(opt = "optim"))
summary(Kyle051w) 

Kyle151w<-lme(Yield~1+PrecZscore +PrecZonVar,random= ~1+PrecZscore+TempZscore|ID,weights=~AreaSc  ,data=CrMaize8,
             correlation=corAR1(0,form= ~ as.numeric(Year)|ID))
summary(Kyle151w) 


Kyle152w<-lme(Yield~1+PrecZscore +PrecZonVar,random= ~1+PrecZscore+TempZscore|ID,weights=~AreaSc,control = lmeControl(opt = "optim")
              ,data=CrMaize8,correlation=corAR1(0,form= ~ as.numeric(Year)|ID))
summary(Kyle152w) # good, very similar results if I use different optimizers...


coef(Kyle151w$modelStruct$corStruct, unconstrained = FALSE)
coef(Kyle152w$modelStruct$corStruct, unconstrained = FALSE)
#-------------------------#-------------------------#-------------------------
# LR test for serial correlation:

Kyle059w<-gls(Yield~1+PrecZscore +PrecZonVar,weights=~AreaSc,data=CrMaize8)
summary(Kyle059w) 

anova(Kyle059w,Kyle151w)
anova(Kyle059w,Kyle152w)
# and some more tests

anova(Kyle059w,Kyle051w)

Kyle159w<-gls(Yield~1+PrecZscore +PrecZonVar,data=CrMaize8,weights=~AreaSc, correlation=corAR1(0,form= ~ as.numeric(Year)|ID))
summary(Kyle159w) 


anova(Kyle159w,Kyle151w)
anova(Kyle159w,Kyle152w)

#COOL ALLES GUTTE
#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

# good, overal it looks good, coef. similar if AR(1) included. but seems to be better based on AIC/BIC 