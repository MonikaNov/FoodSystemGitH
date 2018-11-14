
rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)
library(plm)
library(lme4)
library(boot)
library(lmerTest)
library(itsadug) # for acf of lmer objects
library(lattice) # lme4 plots
load("Main/Phase24.RData")

Phase24ts<-pdata.frame(Phase24,index=c("CountyID","T"))

##ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# The best without weights is for now  Aye3 
##ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Aye3<-lmer(MUACn~PreMedZl1+PreMedZl2+PreMedZl3+PreMedZl4+PreMedZl5
           +PreMedZl6+TemMedZl6+(PreMedZl4+PreMedZl5+PreMedZl6|CountyID),data=Phase24ts)
summary(Aye3) 
plot(Aye3,xlab="Fitted",ylab="Residuals (Pearson)",main="No weights")
dotplot(ranef(Aye3))
##ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# The best without weights is for now  Aye12w 
##ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo


Aye12w<-lmer(MUACn~PreMedZl1+PreMedZl2+PreMedZl5+PreMedZl6+TemMedZl5+
                  (PreMedZl4+PreMedZl5+PreMedZl6 + TemMedZl1+TemMedZl5+TemMedZl6
                   |CountyID),data=Phase24ts,weights=Area2014/1000)
summary(Aye12w) 

plot(Aye12w,xlab="Fitted",ylab="Residuals (Pearson)",main="With weights (Area)")
dotplot(ranef(Aye12w))