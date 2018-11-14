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
load("~/foodSystems/Rcodes/Eq3HealthI/ModelsSpec2Raw.RData")
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

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
##ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
Gordon<-lmer(MUACn~PreMed+PreMedl1+PreMedl2+PreMedl3+PreMedl4+PreMedl5+PreMedl6+
                TemMed+ TemMedl1+TemMedl2+TemMedl3+TemMedl4+TemMedl5+TemMedl6+
                (PreMed+PreMedl1+PreMedl2+PreMedl3+PreMedl4+PreMedl5+PreMedl6
                 +TemMed+ TemMedl1+TemMedl2+TemMedl3+TemMedl4+TemMedl5+TemMedl6
                 |CountyID),data=Phase24ts)
summary(Gordon) 
anova(Gordon)
drop1(Gordon)
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

GordonSc<-lmer(MUACn~scale(PreMed)+scale(PreMedl1)+scale(PreMedl2)+scale(PreMedl3)+scale(PreMedl4)+
          scale(PreMedl5)+scale(PreMedl6)+
            scale(TemMed)+ scale(TemMedl1)+scale(TemMedl2)+scale(TemMedl3)+scale(TemMedl4)
        +scale(TemMedl5)+scale(TemMedl6)+
               (scale(PreMed)+scale(PreMedl1)+scale(PreMedl2)+scale(PreMedl3)+scale(PreMedl4)+
          scale(PreMedl5)+scale(PreMedl6)
                +scale(TemMed)+ scale(TemMedl1)+scale(TemMedl2)+scale(TemMedl3)+
              scale(TemMedl4)+scale(TemMedl5)+scale(TemMedl6)|CountyID),data=Phase24ts)
summary(GordonSc) 
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



Gordon2<-lmer(MUACn~PreMed+PreMedl1+PreMedl2+PreMedl3+PreMedl4+PreMedl5+PreMedl6+
               (PreMed+PreMedl1+PreMedl2+PreMedl3+PreMedl4+PreMedl5+PreMedl6
                |CountyID),data=Phase24ts)
summary(Gordon2) 
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Gordon2<-lmer(MUACn~scale(PreMedl4)+(scale(PreMedl4)|CountyID),data=Phase24ts)
summary(Gordon2) 

Gordon2<-lmer(MUACn~PreMedl4+(PreMedl4|CountyID),data=Phase24ts,control = lmerControl(optimizer="Nelder_Mead"))
summary(Gordon2) 
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Gordon3<-step(lmer(MUACn~PreMed+PreMedl1+PreMedl2+PreMedl3+PreMedl4+PreMedl5+PreMedl6+
               TemMed+ TemMedl1+TemMedl2+TemMedl3+TemMedl4+TemMedl5+TemMedl6+
               (PreMed+PreMedl1+PreMedl2+PreMedl3+PreMedl4+PreMedl5+PreMedl6
                +TemMed+ TemMedl1+TemMedl2+TemMedl3+TemMedl4+TemMedl5+TemMedl6
                |CountyID),data=Phase24ts))
summary(Gordon3) 
##ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
save.image("~/foodSystems/Rcodes/Eq3HealthI/ModelsSpec2Raw3.RData")