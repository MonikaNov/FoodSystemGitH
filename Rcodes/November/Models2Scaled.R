rm(list=ls())
setwd("foodSystems/dataFS") 
# load("../Rcodes/octoberNew/Models1.RData")

library(dplyr); library(tseries); library(plm); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)

load("Main/DaTS.RData")
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

Emmit0<-lm(Yield~SeasPr+AvgTemp,data=ScaledTS);  summary(Emmit0) 

Emmit0ln<-lm(log(Yield0)~SeasPr+AvgTemp,data=ScaledTS);  summary(Emmit0ln) 
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------

Gale00<-lm(Yield~SeasPr+AvgTemp   + Prec2m+CVPrec+Spell+Spell4 +MaxP
                                  + SDTemp + DDays + HWDays+MaxT  ,data=ScaledTS)
summary(Gale00)

Gale01<-lm(Yield~SeasPr+I(SeasPr^2)+AvgTemp   + Prec2m+CVPrec+Spell+Spell4 +MaxP
           + SDTemp + DDays + HWDays+MaxT  ,data=ScaledTS)
summary(Gale01)
Gale02<-lm(Yield~SeasPr+I(SeasPr^2)+AvgTemp  + Prec2m+CVPrec+Spell+Spell10 +MaxP
           + SDTemp + DDays + HWDays+MaxT  ,data=ScaledTS)  # ehm- spell10 positive...
summary(Gale02)  
Gale03<-lm(Yield~SeasPr+I(SeasPr^2)+AvgTemp  + Prec2m+CVPrec+Spell+Spell20 +MaxP
           + SDTemp + DDays + HWDays+MaxT  ,data=ScaledTS)  # max temperature is also positive. interesting
summary(Gale03)  


#--------------------------------------------------------------------------------------------------------------------------------------------------------------------

GaleLn00<-lm(log(Yield0)~SeasPr+AvgTemp   + Prec2m+CVPrec+Spell+Spell4 +MaxP
           + SDTemp + DDays + HWDays+MaxT  ,data=ScaledTS)
summary(GaleLn00) # prec stop being significant if log..maybe square??
vif(GaleLn00)


            GaleLn00<-lm(log(Yield0)~AvgTemp   + Prec2m+CVPrec+Spell+Spell4 
                       + SDTemp  + HWDays+MaxT  ,data=ScaledTS)
            summary(GaleLn00) # prec stop being significant if log..maybe square??
            vif(GaleLn00)

GaleLn01<-lm(log(Yield0)~SeasPr+I(SeasPr^2)+AvgTemp   + Prec2m+CVPrec+Spell+Spell4 +MaxP
           + SDTemp + DDays + HWDays+MaxT  ,data=ScaledTS)
summary(GaleLn01)
        curve(exp(0.129537*x)*exp(  -0.028052*x^2),-3,3 )
        
        GaleLn03<-lm(log(Yield0)~AvgTemp   + Prec2m+CVPrec+Spell+Spell4 +MaxP
                     + SDTemp + DDays + HWDays+MaxT  ,data=ScaledTS)
        summary(GaleLn03) ##prc2m still negative..interesting
        cor.test(ScaledTS$CVPrec,ScaledTS$Prec2m)
        
        
        GaleLn04<-lm(log(Yield0)~AvgTemp   + Prec2m+I(Prec2m^2)+CVPrec+Spell+Spell4 +MaxP
                     + SDTemp + DDays + HWDays+MaxT  ,data=ScaledTS)
        summary(GaleLn04) 
        curve(exp(-0.240453*x)*exp(0.068198*x^2),-2,2 ) # hm..dost divny. vztah vypada naopak nez by mel byt??
        hist(ScaledTS$Prec2m,40)
            GaleLn05<-lm(log(Yield0)~AvgTemp +SeasPr+I(SeasPr^2)  + Prec2m+I(Prec2m^2)+CVPrec+Spell+Spell4 +MaxP
                 + SDTemp + DDays + HWDays+MaxT  ,data=ScaledTS)
            summary(GaleLn05) 
            vif(GaleLn05)
            curve(exp(-0.3473314*x)*exp(0.1301487*x^2),-2,2 ) # hm..dost divny. vztah vypada naopak nez by mel byt??
            hist(ScaledTS$Prec2m,40)
            curve(exp(0.2681131*x)*exp(-0.1015550*x^2),-2,2 ) 
            hist(ScaledTS$SeasPr,40)
            

GaleLn01<-lm(log(Yield0)~SeasPr+I(SeasPr^2)+AvgTemp   + Prec2m+CVPrec+Spell+Spell4 +MaxP
               + SDTemp + DDays + HWDays+MaxT  ,data=ScaledTS)
summary(GaleLn01)
vif(GaleLn01)     #  REMOVE HIGH VIF - CORRELATED
                          GaleLn01<-lm(log(Yield0)~SeasPr+I(SeasPr^2)+AvgTemp+I(AvgTemp^2)   + Prec2m+CVPrec+Spell+Spell4 +MaxP
                                       + SDTemp + DDays + HWDays+MaxT  ,data=ScaledTS)
                          summary(GaleLn01)  # squared temperature just not significant
                          curve(exp(0.129537*x)*exp(-0.028052*x^2),-2,2 )      
                          hist(ScaledTS$SeasPr,40)
                        
GaleLn02<-lm(log(Yield0)~SeasPr+I(SeasPr^2)+AvgTemp  + Prec2m+CVPrec+Spell+Spell10 +MaxP
           + SDTemp + DDays + HWDays+MaxT  ,data=ScaledTS)  # ehm- spell10 positive...
summary(GaleLn02)  
vif(GaleLn02) #  REMOVE HIGH VIF - CORRELATED

GaleLn03<-lm(log(Yield0)~SeasPr+I(SeasPr^2)+AvgTemp  + Prec2m+CVPrec+Spell+Spell20 +MaxP
           + SDTemp + DDays + HWDays+MaxT  ,data=ScaledTS)  # max temperature is also positive. interesting
summary(GaleLn03)  