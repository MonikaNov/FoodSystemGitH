rm(list=ls())
setwd("foodSystems/dataFS") 
# load("../Rcodes/octoberNew/Models1.RData")
library(dplyr); library(tseries); library(plm); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)

load("Main/DaTS.RData")
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# AN idea: difference between seasonal rain and 2 months rainfall


GaleLn01<-lm(log(Yield0)~SeasPr+I(SeasPr^2)+AvgTemp   + Prec2m+CVPrec+Spell+Spell4 +MaxP
             + SDTemp + DDays + HWDays+MaxT  ,data=ScaledTS)
summary(GaleLn01)

GaleLn09<-lm(log(Yield0)~SeasPr+AvgTemp   +CVPrec+Spell+Spell4 +MaxP
             + SDTemp + DDays + HWDays+MaxT  ,data=ScaledTS)
summary(GaleLn09)
vif(GaleLn09)

GaleLn09<-lm(log(Yield0)~Prec2m+AvgTemp   +CVPrec+Spell+Spell4 +MaxP
             + SDTemp + DDays + HWDays+MaxT  ,data=ScaledTS)
summary(GaleLn09)
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


GaleLn2<-lm(log(Yield0)~I(SeasPr-Prec2m)+AvgTemp  +CVPrec+Spell+Spell4 +MaxP
             + SDTemp + DDays + HWDays+MaxT  ,data=ScaledTS)
summary(GaleLn2)
vif(GaleLn2)

GaleLn3<-lm(log(Yield0)~Prec2m+I(SeasPr-Prec2m)+AvgTemp  +CVPrec+Spell+Spell4 +MaxP
            + SDTemp + DDays + HWDays+MaxT  ,data=ScaledTS)
summary(GaleLn3)
vif(GaleLn3)

cor.test((ScaledTS$SeasPr-ScaledTS$Prec2m), ScaledTS$Prec2m)
cor.test(ScaledTS$SeasPr, ScaledTS$Prec2m)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


GaleLn01<-lm(log(Yield0)~SeasPr+AvgTemp   + Prec2m+CVPrec+Spell+Spell4 +MaxP
             + SDTemp + DDays + HWDays+MaxT  ,data=ScaledTS)
summary(GaleLn01)



GaleLn01<-lm(log(Yield0)~SeasPr+AvgTemp   + Prec2m+CVPrec+Spell+Spell4
             + SDTemp + DDays + HWDays+MaxT  ,data=ScaledTS)
summary(GaleLn01)



GaleLn01<-lm(log(Yield0)~I(SeasPr-Prec2m)   + Prec2m+AvgTemp+CVPrec+Spell+Spell4
             + SDTemp + DDays + HWDays+MaxT  ,data=ScaledTS)
summary(GaleLn01)


GaleLn01<-lm(log(Yield0)~I(SeasPr-Prec2m)   +AvgTemp+CVPrec+Spell+Spell4
             + SDTemp + DDays + HWDays+MaxT  ,data=ScaledTS)
summary(GaleLn01)

# so everything suggest that the last month of rainy season actually more important..


GaleLn01<-lm(log(Yield0)~SeasPr+AvgTemp   + Prec2m  ,data=ScaledTS)
summary(GaleLn01)


GaleLn01<-lm(log(Yield0)~I(SeasPr-Prec2m)   + Prec2m+AvgTemp  ,data=ScaledTS)
summary(GaleLn01)

GaleLn01<-lm(log(Yield0)~I(SeasPr-Prec2m)   + Prec2m+AvgTemp+CVPrec+Spell+Spell4
             + SDTemp + DDays + HWDays+MaxT  ,data=ScaledTS)
summary(GaleLn01)