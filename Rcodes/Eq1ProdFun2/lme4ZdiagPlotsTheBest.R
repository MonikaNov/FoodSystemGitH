rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

load("~/foodSystems/dataFS/Main/CrMaize8.RData")
library(plm)
library(lme4)
library(lattice)
#---------------------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------------------------------------

Kyle2s<-lmer(Yield~PrecZscore +TempZscore  +  (PrecZscore+TempZscore|ID),data=CrMaize8)
summary(Kyle2s) # quite similar to Kyle 1s >>good


Kyle51sw<-lmer(Yield~PrecZscore+PrecZonVar+
                 (PrecZscore+TempZscore|ID),weights=AreaSc, data=CrMaize8) 
summary(Kyle51sw)
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

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# Plots Diagnostics

plot(Kyle2s,type=c("p","smooth"))
plot(Kyle2s,sqrt(abs(resid(.)))~fitted(.),type=c("p","smooth"))
qqmath(Kyle2s,id=0.05)
qqmath(Kyle2s,id=0.00025)


plot(Kyle51sw,type=c("p","smooth"))
plot(Kyle51sw,sqrt(abs(resid(.)))~fitted(.),type=c("p","smooth"))
qqmath(Kyle51sw,id=0.05)
qqmath(Kyle51sw,id=0.00025)



