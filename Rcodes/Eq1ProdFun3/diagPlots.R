rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

load("~/foodSystems/dataFS/Main/CrMaize8.RData")
library(plm)
library(lme4)
library(nlme)
library(lattice)
#---------------------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# all the best models, both with and without ARMA errors

Kyle2s<-lmer(Yield~PrecZscore +TempZscore  +  (PrecZscore+TempZscore|ID),data=CrMaize8)
summary(Kyle2s) 

Kyle12<-lme(Yield~1+PrecZscore +TempZscore,random= ~PrecZscore+TempZscore|ID ,data=CrMaize8,control = lmeControl(opt = "optim"),
            correlation=corAR1(0,form= ~ as.numeric(Year)|ID))
summary(Kyle12)

Kyle213<-lme(Yield~1+PrecZscore +TempZscore,random= ~PrecZscore+TempZscore|ID ,data=CrMaize8,control = lmeControl(opt = "optim"),
  correlation=corARMA(form = ~ as.numeric(Year)|ID, p=0,q=3))
summary(Kyle213) 

#---  weighted:

Kyle51sw<-lmer(Yield~PrecZscore+PrecZonVar+(PrecZscore+TempZscore|ID),weights=AreaSc, data=CrMaize8) 
summary(Kyle51sw)

KyW22<-lme(Yield~1+PrecZscore +PrecZonVar,random= ~1+PrecZscore+TempZscore|ID,weights=~AreaSc  ,data=CrMaize8,
           correlation=corARMA(form = ~ as.numeric(Year)|ID, p=2,q=2))
summary(KyW22) 

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
# Plots Diagnostics

plot(Kyle2s,type=c("p","smooth"))
plot(Kyle12,type=c("p","smooth"))
plot(Kyle213,type=c("p","smooth"))
plot(Kyle51sw,type=c("p","smooth"))
plot(KyW22,type=c("p","smooth"))

plot(Kyle2s,sqrt(abs(resid(.)))~fitted(.),type=c("p","smooth"))
plot(Kyle12,sqrt(abs(resid(.)))~fitted(.),type=c("p","smooth"))
plot(Kyle213,sqrt(abs(resid(.)))~fitted(.),type=c("p","smooth"))
plot(Kyle51sw,sqrt(abs(resid(.)))~fitted(.),type=c("p","smooth"))
plot(KyW22,sqrt(abs(resid(.)))~fitted(.),type=c("p","smooth"))

qqmath(Kyle2s,id=0.05)
qqmath(Kyle51sw,id=0.05)

# this plot not implemented for those with ARMA error


#---------------------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------------------------------------

# is the model a good fit in general?  = POSTERIOR PREDICTIVE SIMULATION Bates et al. (2015)

iqrvec <- sapply(simulate(Kyle2s, 1000), IQR)
obsval <- IQR(CrMaize8$Yield)
post.pred.p <- mean(obsval >= c(obsval, iqrvec)) 
post.pred.p  # relatively good. maybe the models predicts a bit highr values.


iqrvec <- sapply(simulate(Kyle51sw, 1000), IQR)
obsval <- IQR(CrMaize8$Yield)
post.pred.p <- mean(obsval >= c(obsval, iqrvec)) 
post.pred.p  

# simulate not implemented for those with ARMA error
