rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)
library(plm)
library(lme4)
library(lmerTest)
library(itsadug) # for acf of lmer objects
load("Main/Phase22.RData")

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

head(Phase22)
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# getting lags

# a. precipitation
Phase23=transform(Phase22,PreMeanZl1=c(NA,head(PreMeanZ,-1)), PreMeanZl2=c(rep(NA,2),head(PreMeanZ,-2)),PreMeanZl3=c(rep(NA,3),head(PreMeanZ,-3)))
Phase23=transform(Phase23,PreMeanl1=c(NA,head(PreMean,-1)), PreMeanl2=c(rep(NA,2),head(PreMean,-2)),PreMeanl3=c(rep(NA,3),head(PreMean,-3)))
Phase23=transform(Phase23,PreMedZl1=c(NA,head(PreMedZ,-1)),PreMedZl2=c(rep(NA,2),head(PreMedZ,-2)),PreMedZl3=c(rep(NA,3),head(PreMedZ,-3)))
Phase23=transform(Phase23,PreMedl1=c(NA,head(PreMed,-1)), PreMedl2=c(rep(NA,2),head(PreMed,-2)), PreMedl3=c(rep(NA,3),head(PreMed,-3)))

# b. temperature
Phase23=transform(Phase23,TemMeanZl1=c(NA,head(TemMeanZ,-1)), TemMeanZl2=c(rep(NA,2),head(TemMeanZ,-2)), TemMeanZl3=c(rep(NA,3),head(TemMeanZ,-3)))
Phase23=transform(Phase23,TemMeanl1=c(NA,head(TemMean,-1)), TemMeanl2=c(rep(NA,2),head(TemMean,-2)), TemMeanl3=c(rep(NA,3),head(TemMean,-3)))
Phase23=transform(Phase23,TemMedZl1=c(NA,head(TemMedZ,-1)), TemMedZl2=c(rep(NA,2),head(TemMedZ,-2)),TemMedZl3=c(rep(NA,3),head(TemMedZ,-3)))
Phase23=transform(Phase23,TemMedl1=c(NA,head(TemMed,-1)),  TemMedl2=c(rep(NA,2),head(TemMed,-2)),  TemMedl3=c(rep(NA,3),head(TemMed,-3)))

Phase23ts<-pdata.frame(Phase23,index=c("CountyID","T"))
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
MUAC1<-lmer(MUACn~PreMed+TemMed+(PreMed+TemMed|CountyID),data=Phase23ts)
summary(MUAC1)

MUAC1<-lmer(MUACn~PreMed+PreMedl1+PreMedl2+PreMedl3+
              TemMed+TemMedl1+TemMedl2+TemMedl3+
              (PreMed+PreMedl1+PreMedl2+PreMedl3+TemMed+TemMedl1+TemMedl2+TemMedl3|CountyID),data=Phase23ts)
summary(MUAC1)

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Z scores

MUAC4<-lmer(MUACn~PreMedZ+TemMedZ+(PreMedZ+TemMedZ|CountyID),data=Phase23ts)
summary(MUAC4)
confint(MUAC4,method="Wald")

# MUAC2<-lmer(MUACn~PreMedZ+PreMedZl1+PreMedZl2+PreMedZl3+ TemMedZ+TemMedZl1+TemMedZl2+TemMedZl3+  (PreMedZ+PreMedZl1+PreMedZl2+PreMedZl3+TemMedZ+TemMedZl1+TemMedZl2+TemMedZl3|CountyID),data=Phase23ts)
# takes a bit long - better do not estimate again
summary(MUAC2)

MUAC5<-lmer(MUACn~PreMedZl1+PreMedZl2+PreMedZl3+(PreMedZl1+PreMedZl2+PreMedZl3|CountyID),data=Phase23ts)
summary(MUAC5) #cool,quite nice :-)
confint(MUAC5,method="Wald")
as.function(MUAC5)
# MUAC5prf<-profile(MUAC5) # takes too long

test1<-simulate(MUAC5)

mySumm2 <- function(.) {
  c(beta=fixef(.),sigma=sigma(.), sig01=sqrt(unlist(VarCorr(.))))
}
bootMer(MUAC5)

set.seed(101)
## 3.8s (on a 5600 MIPS 64bit fast(year 2009) desktop "AMD Phenom(tm) II X4 925"):
system.time( boo01 <- bootMer(MUAC5, mySumm2, nsim = 100) )

summary(boo01)
coef(MUAC5)


mySumm <- function(.) { s <- sigma(.)
c(beta =getME(., "beta"), sigma = s, sig01 = unname(s * getME(., "theta"))) }
(t0 <- mySumm(MUAC5)) 
system.time( boo02 <- bootMer(MUAC5, mySumm, nsim = 100) )


# now I will try to simulate p-values:
mySumm3<-function(.)   summary(as_lmerModLmerTest(.))[[10]][,5]
boo04 <- bootMer(MUAC5, mySumm3, nsim = 100)
summary(boo04)
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
save.image("~/foodSystems/Rcodes/Eq3HealthI/mixedModels3.RData")