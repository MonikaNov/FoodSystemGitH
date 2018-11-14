rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma
library(dplyr); library(tseries); library(plm); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)

load("Main/Da.RData")
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo




GaleLn03<-lm(log(Yield0)~AvgTemp   + Prec2m+CVPrec+Spell+Spell4 +MaxP
             + SDTemp + DDays + HWDays+MaxT  ,data=ScaledTS)
summary(GaleLn03) ##prc2m still negative..interesting
# CV precip is also negative and significant. And interestingly enough, CV precip and prec2m are strongly negatively correlated
cor.test(ScaledTS$CVPrec,ScaledTS$Prec2m)
cor.test(DaTS$CVPrec,DaTS$Prec2m)
#...but, all vars in Gale3ln seem to be negatively correlated with Prec2m..
#.. and positively with CVPrec


GaleLn04<-lm(log(Yield0)~AvgTemp   + Prec2m+I(Prec2m^2)+CVPrec+Spell+Spell4 +MaxP
             + SDTemp + DDays + HWDays+MaxT  ,data=ScaledTS)
summary(GaleLn04) 
curve(exp(-0.240453*x)*exp(0.068198*x^2),-2,2 ) # hm..dost divny. vztah vypada naopak nez by mel byt??