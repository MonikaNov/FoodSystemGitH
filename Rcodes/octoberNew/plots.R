rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

library(dplyr); library(tseries); library(plm); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)

load("Main/isdataTS.RData")
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

# 1. PRECIPITATION

plot(Yield~Year, data=isdataTS)

plot(Yield~SeasPr, data=isdataTS)
plot(Yield~I(SeasPr^2), data=isdataTS)
plot(Yield~SeasPr, data=isdataScTS)
plot(Yield~I(SeasPr^2), data=isdataScTS)

  plot(Yield~SeasPr, data=isdataScTS)
  lines(Yield~AvgTemp, data=isdataScTS,col=2,type='p')
  
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
plot(Yield~Prec2m, data=isdataScTS)
plot(Yield~I(Prec2m^2), data=isdataScTS)
  plot(Yield~SeasPr, data=isdataScTS,xlim=c(-2,4))
  lines(Yield~Prec2m, data=isdataScTS,col=2,type='p')
  lines(Yield~I(Prec2m^2), data=isdataScTS,col=3,type='p')
  
plot(Yield~CVPrec, data=isdataScTS)
plot(Yield~SDPrec, data=isdataScTS)
  plot(Yield~SeasPr, data=isdataScTS,xlim=c(-2,4))
  lines(Yield~CVPrec, data=isdataScTS,col=2,type='p')   #   N.I.C.E.
  
  
plot(Yield~Spell, data=isdataScTS)
cor.test(isdataScTS$CVPrec,isdataScTS$Spell)

plot(Yield~Spell10, data=isdataScTS)
plot(Yield~Spell20, data=isdataScTS)
cor.test(isdataScTS$Spell10,isdataScTS$Spell)

  plot(Yield~Spell20, data=isdataScTS)
  lines(Yield~CVPrec, data=isdataScTS,col=2,type='p') 

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
  
# 2. TEMPERATURE
  
plot(Yield~AvgTemp, data=isdataScTS)
plot(Yield~I(AvgTemp^2), data=isdataScTS)
  plot(Yield~AvgTemp, data=isdataScTS)
  lines(Yield~I(AvgTemp^2), data=isdataScTS,col=2,type='p') 
  
  plot(Yield~SeasPr, data=isdataScTS)
  lines(Yield~AvgTemp, data=isdataScTS,col=2,type='p') 
  lines(Yield~CVPrec, data=isdataScTS,col=3,type='p') 
  
plot(Yield~SDTemp, data=isdataScTS)
plot(Yield~I(SDTemp^2), data=isdataScTS)

plot(Yield~AvgTemp, data=isdataScTS)
lines(Yield~SDTemp, data=isdataScTS,col=2,type='p') 


plot(Yield~SDTemp, data=isdataScTS)


plot(Yield~DDays, data=isdataScTS)
plot(Yield~HWDays, data=isdataScTS)

cor.test(isdataScTS$Yield,isdataScTS$HWDays)

plot(Yield~MaxT, data=isdataScTS)

plot(Yield~I(AvgTemp*SeasPr), data=isdataScTS)

plot(Yield~I(AvgTemp*SeasPr), data=isdataTS)