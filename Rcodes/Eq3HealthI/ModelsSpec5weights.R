
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
load("~/foodSystems/Rcodes/Eq3HealthI/ModelsSpec5weights.RData")
Phase24ts<-pdata.frame(Phase24,index=c("CountyID","T"))

##ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# The best without weights for now is Aye3 
##ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
##ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
Ayelet1w<-lmer(MUACn~PreMedZ+PreMedZl1+PreMedZl2+PreMedZl3+PreMedZl4+PreMedZl5+PreMedZl6+
                TemMedZ+ TemMedZl1+TemMedZl2+TemMedZl3+TemMedZl4+TemMedZl5+TemMedZl6+
                (PreMedZ+PreMedZl1+PreMedZl2+PreMedZl3+PreMedZl4+PreMedZl5+PreMedZl6
                 +TemMedZ+ TemMedZl1+TemMedZl2+TemMedZl3+TemMedZl4+TemMedZl5+TemMedZl6
                 |CountyID),data=Phase24ts,weights=Area2014)
summary(Ayelet1w) 

##ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
Ayelet12w<-lmer(MUACn~PreMedZ+PreMedZl1+PreMedZl2+PreMedZl3+PreMedZl4+PreMedZl5+PreMedZl6+
                +TemMedZl4+TemMedZl5+TemMedZl6+
                 (PreMedZ+PreMedZl1+PreMedZl2+PreMedZl3+PreMedZl4+PreMedZl5+PreMedZl6+TemMedZl4+TemMedZl5+TemMedZl6
                  |CountyID),data=Phase24ts,weights=Area2014)
summary(Ayelet12w) 


Ayelet3w<-lmer(MUACn~PreMedZl1+PreMedZl2+PreMedZl3+PreMedZl4+PreMedZl5+PreMedZl6+
                  (PreMedZl4+PreMedZl5+PreMedZl6|CountyID),
                data=Phase24ts,weights=Area2014)
summary(Ayelet3w) 


Ayelet4w<-lmer(MUACn~PreMedZl1+PreMedZl2+PreMedZl3+PreMedZl4+PreMedZl5+PreMedZl6+
                 (PreMedZl4+PreMedZl5+PreMedZl6|CountyID),
               data=Phase24ts,weights=Area2014,control = lmerControl(optimizer="Nelder_Mead"))
summary(Ayelet4w) 
##ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
save.image("~/foodSystems/Rcodes/Eq3HealthI/ModelsSpec5weights.RData")