rm(list=ls())
setwd("foodSystems/dataFS") 
setwd("dataFS") # home
library(dplyr); library(tseries); library(plm); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)

load("Main/DaTS.RData")

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo


Shiv32B<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec
              + SDTemp  +(I(Prec2m^2)+Prec2m|ASAL),data=ScaledTS) 
summary(Shiv32B); vif(Shiv32B); 

plot(ACF(Shiv32B),alpha=0.05)
acf(summary(Shiv32B)$residuals)
pacf(summary(Shiv32B)$residuals)

#---------------------------------------------------------------------------------
ctrl <- lmeControl(opt='optim');

ShiARe00<-lme(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec
          + SDTemp , random= ~(I(Prec2m^2)+Prec2m) | ASAL, control=ctrl,na.action=na.exclude,correlation=corSymm ,
          data=ScaledTS); summary(ShiARe00)


#           TADY TO NEJDE DAAALL


plot(ACF(ShiARe00),alpha=0.05)
acf(summary(ShiARe00)$residuals)
pacf(summary(ShiARe00)$residuals)

