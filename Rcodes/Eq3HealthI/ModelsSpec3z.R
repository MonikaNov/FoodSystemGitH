
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
load("~/foodSystems/Rcodes/Eq3HealthI/ModelsSpec1.RData")
load("~/foodSystems/Rcodes/Eq3HealthI/squared.RData")
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Phase24ts<-pdata.frame(Phase24,index=c("CountyID","T"))

##ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
##ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

##ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# The best for now is Aye3 
##ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

Ayelet3<-lmer(MUACn~PreMedZl1+PreMedZl2+PreMedZl3+PreMedZl4+PreMedZl5
              +PreMedZl6+TemMedZl6+(PreMedZl1+PreMedZl2+PreMedZl3+PreMedZl4
                                    +PreMedZl5+PreMedZl6+TemMedZl6|CountyID),data=Phase24ts)
summary(Ayelet3) 



Ayelet3f<-lmer(MUACn~PreMedZl1+PreMedZl2+PreMedZl3+PreMedZl4+PreMedZl5
              +PreMedZl6+TemMedZl6+(PreMedZl1+PreMedZl2+PreMedZl3+PreMedZl4
                                    +PreMedZl5+TemMedZl6|CountyID),data=Phase24ts)
summary(Ayelet3f) 


Ayelet3s<-step(lmer(MUACn~PreMedZl1+PreMedZl2+PreMedZl3+PreMedZl4+PreMedZl5
              +PreMedZl6+TemMedZl6+(PreMedZl1+PreMedZl2+PreMedZl3+PreMedZl4
                                    +PreMedZl5+PreMedZl6+TemMedZl6|CountyID),data=Phase24ts))
summary(Ayelet3s) 
Ayelet3s[[1]]
Ayelet3s[[2]]
get_model(Ayelet3s)

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Aye3<-lmer(MUACn~PreMedZl1+PreMedZl2+PreMedZl3+PreMedZl4+PreMedZl5
                    +PreMedZl6+TemMedZl6+(PreMedZl4+PreMedZl5+PreMedZl6|CountyID),data=Phase24ts)
summary(Aye3) 
plot(Aye3)
dotplot(ranef(Aye3))

anova(Aye3,Ayelet3)
rand(Aye3)
ranova(Aye3)
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
AyeletSq5<-lmer(MUACn~PreMedZl1+PreMedZl2+PreMedZl3+PreMedZl4+PreMedZl5
                +PreMedZl6+TemMedZl6+(PreMedZl4+PreMedZl5+PreMedZl6+I(scale(PreMedZl4))^2+I(scale(PreMedZl5))^2
                                      +I(scale(PreMedZl6))^2|CountyID),data=Phase24ts)
summary(AyeletSq5) 
plot(AyeletSq5)
dotplot(ranef(AyeletSq5))
#-----------------------------------

Aye3w<-lmer(MUACn~PreMedZl1+PreMedZl2+PreMedZl3+PreMedZl4+PreMedZl5
           +PreMedZl6+TemMedZl6+(PreMedZl4+PreMedZl5+PreMedZl6|CountyID),
           data=Phase24ts,weights=Area2014,control = lmerControl(optimizer="Nelder_Mead"))
summary(Aye3w) 
plot(Aye3w)
dotplot(ranef(Aye3w))
anova(Aye3,Aye3w)
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


AyeletSq3<-lmer(MUACn~-PreMedZl1+PreMedZl2+PreMedZl3+PreMedZl4+PreMedZl5
           +PreMedZl6+TemMedZl6+I(PreMedZl1)^2+I(PreMedZl2)^2+I(PreMedZl3)^2+I(PreMedZl4)^2+I(PreMedZl5)^2
           +I(PreMedZl6)^2+I(TemMedZl6)^2+(PreMedZl4+PreMedZl5+PreMedZl6|CountyID),data=Phase24ts)
summary(AyeletSq3) 

AyeletSq4<-lmer(MUACn~I(PreMedZl1)^2+I(PreMedZl2)^2+I(PreMedZl3)^2+I(PreMedZl4)^2+I(PreMedZl5)^2
                +I(PreMedZl6)^2+I(TemMedZl6)^2+(PreMedZl4+PreMedZl5+PreMedZl6|CountyID),data=Phase24ts)
summary(AyeletSq4) 

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

AyeletSq3t<-lmer(MUACn~-1+PreMedZl1+PreMedZl2+PreMedZl3+PreMedZl4+PreMedZl5
                +PreMedZl6+TemMedZl6+I(PreMedZl1)^2+I(PreMedZl2)^2+I(PreMedZl3)^2+I(PreMedZl4)^2+I(PreMedZl5)^2
                +I(PreMedZl6)^2+I(TemMedZl6)^2+(PreMedZl4+PreMedZl5+PreMedZl6|CountyID),data=Phase24ts)
summary(AyeletSq3) 


##ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
AyeTest<-lmer(MUACn~PreMedZl1+PreMedZl2+PreMedZl3+PreMedZl4+PreMedZl5
           +PreMedZl6+TemMedZl6+(1|CountyID),data=Phase24ts)
summary(AyeTest) 
anova(Aye3,AyeTest)

AyeTest<-lmer(MUACn~PreMedZ+PreMedZl1+PreMedZl2+PreMedZl3+PreMedZl4+PreMedZl5
              +PreMedZl6+TemMedZl6+(PreMedZl4+PreMedZl5+PreMedZl6|CountyID),data=Phase24ts)
summary(AyeTest) 
anova(Aye3,AyeTest)

AyeTest<-lmer(MUACn~PreMedZl1+PreMedZl2+PreMedZl3+PreMedZl4+PreMedZl5
              +PreMedZl6+(PreMedZl4+PreMedZl5+PreMedZl6|CountyID),data=Phase24ts)
summary(AyeTest) 
anova(Aye3,AyeTest)


AyeTest<-lmer(MUACn~PreMedZl1+PreMedZl2+PreMedZl3+(PreMedZl4+PreMedZl5+PreMedZl6|CountyID),data=Phase24ts)
summary(AyeTest) 
anova(Aye3,AyeTest)
##ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# The best for now is Aye3 
##ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
save.image("~/foodSystems/Rcodes/Eq3HealthI/ModelsSpec3z.RData")
