rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)
library(plm)
library(lme4)
library(itsadug) # for acf of lmer objects
load("Main/Phase22.RData")

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

head(Phase22)
# getting lags
Phase23=transform(Phase22,PreMeanZl1=c(NA,head(PreMeanZ,-1)),
                  PreMeanZl2=c(rep(NA,2),head(PreMeanZ,-2)),
                  PreMeanZl3=c(rep(NA,3),head(PreMeanZ,-3)))



Phase23ts<-pdata.frame(Phase23,index=c("CountyID","T"))
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
MUAC1<-lmer(MUACn~PreMed+TemMed+(PreMed+TemMed|CountyID),data=Phase22ts)
summary(MUAC1)



#----------------------------------------------------------------------
#Z scores

MUAC1<-lmer(MUACn~PreMedZ+TemMedZ+(PreMedZ+TemMedZ|CountyID),data=Phase22ts)
summary(MUAC1)
acf_resid(MUAC1)


MUAC1<-lmer(MUACn~PreMedZ+TemMedZ+(PreMedZ+TemMedZ|CountyID),data=Phase22ts)
summary(MUAC1)
acf_resid(MUAC1)


MUAC1<-lmer(MUACn~PreMedZ+TemMedZ+I(PreMedZ^2)+I(TemMedZ^2)+(PreMedZ+TemMedZ+I(PreMedZ^2)+I(TemMedZ^2)|CountyID),data=Phase22ts)
summary(MUAC1)

#----------------------------------------------------------------------
#now mean

MUAC1<-lmer(MUACn~PreMeanZ+TemMeanZ+(PreMeanZ+TemMeanZ|CountyID),data=Phase23ts)
summary(MUAC1)

MUAC2<-lmer(MUACn~PreMeanZ+TemMeanZ+lag(PreMeanZ)+lag(TemMeanZ)
            +(PreMeanZ+TemMeanZ+lag(PreMeanZ)+lag(TemMeanZ)|CountyID),data=Phase22ts)
summary(MUAC2)

MUAC2<-lmer(MUACn~lag(PreMeanZ)+lag(TemMeanZ)+(lag(PreMeanZ)+lag(TemMeanZ)|CountyID),data=Phase22ts)
summary(MUAC2)



MUAC1<-lmer(MUACn~PreMedZ+TemMedZ+(PreMedZ+TemMedZ|CountyID),data=Phase22ts)
summary(MUAC1)
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# and lags


MUAC1<-lmer(MUACn~PreMeanZ+TemMeanZ+(PreMeanZ+TemMeanZ|CountyID),data=Phase23ts)
summary(MUAC1)

MUAC1<-lmer(MUACn~PreMeanZ+lag(PreMeanZ)+TemMeanZ+(PreMeanZ+TemMeanZ|CountyID),data=Phase23ts)
summary(MUAC1)

MUAC1<-lmer(MUACn~PreMeanZ+PreMeanZl1+PreMeanZl2+PreMeanZl3+TemMeanZ
            +(PreMeanZ+TemMeanZ+PreMeanZl1+PreMeanZl2+PreMeanZl3|CountyID),data=Phase23ts)
summary(MUAC1)