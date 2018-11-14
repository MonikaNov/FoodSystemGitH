
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
load("~/foodSystems/Rcodes/Eq3HealthI/ModelsSpec7weights.RData")
##ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# The best without weights for now is Aye3 
##ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
##ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
Ayelet1w<-lmer(MUACn~PreMedZ+PreMedZl1+PreMedZl2+PreMedZl3+PreMedZl4+PreMedZl5+PreMedZl6+
                TemMedZ+ TemMedZl1+TemMedZl2+TemMedZl3+TemMedZl4+TemMedZl5+TemMedZl6+
                (PreMedZ+PreMedZl1+PreMedZl2+PreMedZl3+PreMedZl4+PreMedZl5+PreMedZl6
                 +TemMedZ+ TemMedZl1+TemMedZl2+TemMedZl3+TemMedZl4+TemMedZl5+TemMedZl6
                 |CountyID),data=Phase24ts,weights=Area2014/1000)
summary(Ayelet1w) 

##ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
Ayelet12w<-lmer(MUACn~PreMedZ+PreMedZl1+PreMedZl2+PreMedZl3+PreMedZl4+PreMedZl5+PreMedZl6+
                +TemMedZl4+TemMedZl5+TemMedZl6+
                 (PreMedZ+PreMedZl1+PreMedZl2+PreMedZl3+PreMedZl4+PreMedZl5+PreMedZl6+TemMedZl4+TemMedZl5+TemMedZl6
                  |CountyID),data=Phase24ts,weights=Area2014/1000)
summary(Ayelet12w) 


Ayelet3w<-lmer(MUACn~PreMedZl1+PreMedZl2+PreMedZl3+PreMedZl4+PreMedZl5+PreMedZl6+
                  (PreMedZl4+PreMedZl5+PreMedZl6|CountyID),
                data=Phase24ts,weights=Area2014/1000)
summary(Ayelet3w) 

plot(Ayelet3w)
dotplot(ranef(Ayelet3w))


Ayelet4w<-lmer(MUACn~PreMedZl1+PreMedZl2+PreMedZl3+PreMedZl4+PreMedZl5+PreMedZl6+
                 (PreMedZl4+PreMedZl5+PreMedZl6|CountyID),
               data=Phase24ts,weights=Area2014/1000,control = lmerControl(optimizer="Nelder_Mead"))
summary(Ayelet4w) 
#------------------------------------------------------------------------------------------------------------------------------------------

Ayelet1wS<-step(lmer(MUACn~PreMedZ+PreMedZl1+PreMedZl2+PreMedZl3+PreMedZl4+PreMedZl5+PreMedZl6+
                 TemMedZ+ TemMedZl1+TemMedZl2+TemMedZl3+TemMedZl4+TemMedZl5+TemMedZl6+
                 (PreMedZ+PreMedZl1+PreMedZl2+PreMedZl3+PreMedZl4+PreMedZl5+PreMedZl6
                  +TemMedZ+ TemMedZl1+TemMedZl2+TemMedZl3+TemMedZl4+TemMedZl5+TemMedZl6
                  |CountyID),data=Phase24ts,weights=Area2014/1000))
summary(Ayelet1wS) 
get_model(Ayelet1wS) 
#------------------------------------------------------------------------------------------------------------------------------------------
#based on the stepwise above:

Ayelet10w<-lmer(MUACn~PreMedZl1+PreMedZl2+PreMedZl4+PreMedZl5+PreMedZl6+TemMedZl5+
                       (PreMedZl2+PreMedZl4+PreMedZl5+PreMedZl6 +TemMedZ+ TemMedZl1+TemMedZl5+TemMedZl6
                        |CountyID),data=Phase24ts,weights=Area2014/1000)
summary(Ayelet10w) 

Ayelet11w<-lmer(MUACn~PreMedZl1+PreMedZl2+PreMedZl4+PreMedZl5+PreMedZl6+TemMedZl5+
                  (PreMedZl4+PreMedZl5+PreMedZl6 + TemMedZl1+TemMedZl5+TemMedZl6
                   |CountyID),data=Phase24ts,weights=Area2014/1000)
summary(Ayelet11w) 
##ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
##ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

Ayelet12w<-lmer(MUACn~PreMedZl1+PreMedZl2+PreMedZl5+PreMedZl6+TemMedZl5+
                  (PreMedZl4+PreMedZl5+PreMedZl6 + TemMedZl1+TemMedZl5+TemMedZl6
                   |CountyID),data=Phase24ts,weights=Area2014/1000)
summary(Ayelet12w) 

#so far the best Ayelet12w
##ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
##ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#------------------------------------------------------------------------------------------------------------------------------------------
#trying more steps

Ayelet10ws<-step(lmer(MUACn~PreMedZl1+PreMedZl2+PreMedZl4+PreMedZl5+PreMedZl6+TemMedZl5+
                  (PreMedZl2+PreMedZl4+PreMedZl5+PreMedZl6 +TemMedZ+ TemMedZl1+TemMedZl5+TemMedZl6
                   |CountyID),data=Phase24ts,weights=Area2014/1000))
summary(Ayelet10ws) 
get_model(Ayelet10ws) 


Ayelet11ws<-step(lmer(MUACn~PreMedZl1+PreMedZl2+PreMedZl4+PreMedZl5+PreMedZl6+TemMedZl5+
                        (PreMedZl2+PreMedZl4+PreMedZl5+PreMedZl6 +TemMedZ+ TemMedZl1+TemMedZl5+TemMedZl6
                         |CountyID),data=Phase24ts,weights=Area2014/1000))
summary(Ayelet11ws) 
get_model(Ayelet11ws) 

Ayelet14w<-lmer(MUACn~PreMedZl1+PreMedZl2+PreMedZl4+PreMedZl5+PreMedZl6+TemMedZl5+
                        (PreMedZl2+PreMedZl5+PreMedZl6 +TemMedZ+ TemMedZl1+TemMedZl5
                         |CountyID),data=Phase24ts,weights=Area2014/1000)
summary(Ayelet14w) 
get_model(Ayelet14w) 
##ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
save.image("~/foodSystems/Rcodes/Eq3HealthI/ModelsSpec7weights.RData")