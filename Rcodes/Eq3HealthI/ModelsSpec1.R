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
load("Main/Phase23.RData")
load("~/foodSystems/Rcodes/Eq3HealthI/ModelsSpec1.RData")
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


Phase23ts<-pdata.frame(Phase23,index=c("CountyID","T"))
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#
# Ayelet2 the best for now
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


Phase23=transform(Phase22,PreMeanZl4=c(rep(NA,4),head(PreMeanZ,-4)), PreMeanZl5=c(rep(NA,5),head(PreMeanZ,-5)),PreMeanZl6=c(rep(NA,6),head(PreMeanZ,-6)))
Phase23=transform(Phase23,PreMeanl4=c(rep(NA,4),head(PreMean,-4)), PreMeanl5=c(rep(NA,5),head(PreMean,-5)),PreMeanl6=c(rep(NA,6),head(PreMean,-6)))
Phase23=transform(Phase23,PreMedZl4=c(rep(NA,4),head(PreMedZ,-4)),PreMedZl5=c(rep(NA,5),head(PreMedZ,-5)),PreMedZl6=c(rep(NA,6),head(PreMedZ,-6)))
Phase23=transform(Phase23,PreMedl4=c(rep(NA,4),head(PreMed,-4)), PreMedl5=c(rep(NA,5),head(PreMed,-5)), PreMedl6=c(rep(NA,6),head(PreMed,-6)))

# b. temperature
Phase23=transform(Phase23,TemMeanZl4=c(rep(NA,4),head(TemMeanZ,-4)), TemMeanZl5=c(rep(NA,5),head(TemMeanZ,-5)), TemMeanZl6=c(rep(NA,6),head(TemMeanZ,-6)))
Phase23=transform(Phase23,TemMeanl4=c(rep(NA,4),head(TemMean,-4)), TemMeanl5=c(rep(NA,5),head(TemMean,-5)), TemMeanl6=c(rep(NA,6),head(TemMean,-6)))
Phase23=transform(Phase23,TemMedZl4=c(rep(NA,4),head(TemMedZ,-4)), TemMedZl5=c(rep(NA,5),head(TemMedZ,-5)),TemMedZl6=c(rep(NA,6),head(TemMedZ,-6)))
Phase23=transform(Phase23,TemMedl4=c(rep(NA,4),head(TemMed,-4)),  TemMedl5=c(rep(NA,5),head(TemMed,-5)),  TemMedl6=c(rep(NA,6),head(TemMed,-6)))
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Phase24<-Phase23
Phase24ts<-pdata.frame(Phase24,index=c("CountyID","T"))
##ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
rm(list=setdiff(ls(),"Phase24"  ))
save.image("Main/Phase24.RData")

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
##ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
Ayelet1<-lmer(MUACn~PreMedZ+PreMedZl1+PreMedZl2+PreMedZl3+PreMedZl4+PreMedZl5+PreMedZl6+
               TemMedZ+ TemMedZl1+TemMedZl2+TemMedZl3+TemMedZl4+TemMedZl5+TemMedZl6+
                (PreMedZ+PreMedZl1+PreMedZl2+PreMedZl3+PreMedZl4+PreMedZl5+PreMedZl6
                 +TemMedZ+ TemMedZl1+TemMedZl2+TemMedZl3+TemMedZl4+TemMedZl5+TemMedZl6
                    |CountyID),data=Phase24ts)
summary(Ayelet1) 
anova(Ayelet1)
drop1(Ayelet1)


Ayelet2<-lmer(MUACn~PreMedZl1+PreMedZl2+PreMedZl3+PreMedZl4+PreMedZl5
              +PreMedZl6+(PreMedZl1+PreMedZl2+PreMedZl3+PreMedZl4
                          +PreMedZl5+PreMedZl6|CountyID),data=Phase24ts)
summary(Ayelet2) 

summary(Ayelet2) 
anova(Ayelet2)
drop1(Ayelet2)

Ayelet21<-lmer(MUACn~PreMedZl1+PreMedZl2+PreMedZl3+PreMedZl4+PreMedZl5
              +PreMedZl6+(PreMedZl1+PreMedZl2+PreMedZl3+PreMedZl4
                          +PreMedZl5+PreMedZl6+
                          TemMedZl1+TemMedZl2+TemMedZl3+TemMedZl4
                          +TemMedZl5+TemMedZl6|CountyID),data=Phase24ts)
summary(Ayelet21) 

Ayelet22<-lmer(MUACn~PreMedZl1+PreMedZl2+PreMedZl3+PreMedZl4+PreMedZl5
               +PreMedZl6+(PreMedZl1+PreMedZl2+PreMedZl3+PreMedZl4
                           +PreMedZl5+PreMedZl6+TemMedZl6|CountyID),data=Phase24ts)
summary(Ayelet22) 





Ayelet3<-lmer(MUACn~PreMedZl1+PreMedZl2+PreMedZl3+PreMedZl4+PreMedZl5
              +PreMedZl6+TemMedZl6+(PreMedZl1+PreMedZl2+PreMedZl3+PreMedZl4
                                    +PreMedZl5+PreMedZl6+TemMedZl6|CountyID),data=Phase24ts)
summary(Ayelet3) 
##ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# just in case

Ayelet2t<-lmer(MUACn~TemMedZl1+TemMedZl2+TemMedZl3+TemMedZl4+TemMedZl5+TemMedZl6+(TemMedZl1+TemMedZl2+TemMedZl3+TemMedZl4+TemMedZl5+TemMedZl6|CountyID),data=Phase24ts)
summary(Ayelet2t) 

Ayelet2t2<-lmer(MUACn~TemMedZ+TemMedZl1+TemMedZl2+TemMedZl3+TemMedZl4+TemMedZl5
                +TemMedZl6+(TemMedZ+TemMedZl1+TemMedZl2+TemMedZl3+TemMedZl4+TemMedZl5+TemMedZl6|CountyID),data=Phase24ts)
summary(Ayelet2t2) 


Ayelet2t23<-lmer(MUACn~TemMedZ+(TemMedZ+TemMedZl1+TemMedZl2+TemMedZl3+TemMedZl4+TemMedZl5+TemMedZl6|CountyID),data=Phase24ts)
summary(Ayelet2t23) 
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Ayelet4<-lmer(MUACn~I(PreMedZ)^2+I(PreMedZl1)^2+I(PreMedZl2)^2+I(PreMedZl3)^2+I(PreMedZl4)^2+I(PreMedZl5)^2
              +I(PreMedZl6)^2+(PreMedZl1+PreMedZl2+PreMedZl3+PreMedZl4
    +PreMedZl5+PreMedZl6+I(PreMedZ)^2+I(PreMedZl1)^2+I(PreMedZl2)^2+I(PreMedZl3)^2+
      I(PreMedZl4)^2+I(PreMedZl5)^2
    +I(PreMedZl6)^2|CountyID),data=Phase24ts)
summary(Ayelet4) 


##ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
Ayelet1s<-step(lmer(MUACn~PreMedZ+PreMedZl1+PreMedZl2+PreMedZl3+PreMedZl4+PreMedZl5+PreMedZl6+
                TemMedZ+ TemMedZl1+TemMedZl2+TemMedZl3+TemMedZl4+TemMedZl5+TemMedZl6+
                (PreMed+PreMedZl1+PreMedZl2+PreMedZl3+PreMedZl4+PreMedZl5+PreMedZl6
                 +TemMedZ+ TemMedZl1+TemMedZl2+TemMedZl3+TemMedZl4+TemMedZl5+TemMedZl6
                 |CountyID),data=Phase24ts))
summary(Ayelet1s) 
get_model(Ayelet1s) 
##ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
save.image("~/foodSystems/Rcodes/Eq3HealthI/squared.RData")
save.image("~/foodSystems/Rcodes/Eq3HealthI/ModelsSpec6.RData")
##ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
