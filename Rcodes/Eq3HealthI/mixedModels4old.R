rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)
library(plm)
library(lme4)
library(lmerTest)
library(itsadug) # for acf of lmer objects
library(lattice) # lme4 plots
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
# save game
#  rm(list=setdiff(ls(), "Phase23"))
# save.image("Main/Phase23.RData")
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

# learnign and training the lme4 package. so far MUAC5 seems to be the best

MUAC5<-lmer(MUACn~PreMedZl1+PreMedZl2+PreMedZl3+(PreMedZl1+PreMedZl2+PreMedZl3|CountyID),data=Phase23ts)
summary(MUAC5) 
logLik(MUAC5)
REMLcrit(MUAC5) # = -2* loglik
plot(MUAC5)
plot(MUAC5, resid(., type = "pearson") ~ fitted(.)| round(model.frame(.)$CountyID,-1) )
terms(MUAC5)
logLik(MUAC5)
ranef(MUAC5)

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# going to do the chapter 5.2 of Bates et al. (2015)

MUAC5<-lmer(MUACn~PreMedZl1+PreMedZl2+PreMedZl3+(PreMedZl1+PreMedZl2+PreMedZl3|CountyID),data=Phase23ts)
summary(MUAC5) 

fm1<-lmer(Reaction~Days+(Days|Subject),sleepstudy)
summary(fm1)
VarCorr(fm1)
as.data.frame(VarCorr(fm1))
ranef(fm1)
dotplot(ranef(fm1))
dotplot(ranef(MUAC5))
dotplot(ranef(MUAC5),condVar=TRUE)
vcov(fm1)




sleepstudy$Group<-as.factor(sample(c(1,2,3,4),size=180,replace=TRUE,prob=c(0.15,0.30,0.2,0.35)))
fm1<-lmer(Reaction~Days+(Days|Subject),sleepstudy)
fmt<-lmer(Reaction~Days+(Days|Subject)+(Days|Group),sleepstudy)
summary(fmt)
VarCorr(fmt)
as.data.frame(VarCorr(fmt))
ranef(fmt)
cor.test(ranef(fmt)$Group[,1],ranef(fmt)$Group[,2])

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# diagnostic plots

plot(fm1,type=c("p","smooth"))
plot(fm1)
plot(fm1,sqrt(abs(resid(.)))~fitted(.)|Group, type=c("p","smooth") )
plot(fm1,sqrt(abs(resid(.)))~fitted(.), type=c("p","smooth") )
plot(MUAC5,sqrt(abs(resid(.)))~fitted(.), type=c("p","smooth") )
plot(MUAC5,sqrt(abs(resid(.)))~fitted(.)|cntis, type=c("p","smooth") )
plot(MUAC5,sqrt(abs(resid(MUAC5)))~fitted(MUAC5)|CountyID, type=c("p","smooth") )

MUAC5b<-lmer(MUACn~PreMedZl1+PreMedZl2+PreMedZl3+(PreMedZl1+PreMedZl2+PreMedZl3|CountyID),data=Phase23ts[rownames(model.frame(MUAC5)),])
summary(MUAC5b)
plot(MUAC5b,sqrt(abs(resid(.)))~fitted(.)|CountyID, type=c("p","smooth") )
plot(MUAC5b,sqrt(abs(resid(.)))~fitted(.), type=c("p","smooth") )
plot(MUAC5b)
plot(MUAC5b,resid(.)~fitted(.), type=c("p","smooth") )

#plots for counties separately and just some of them
table(Phase23ts$CountyID)

mymap<-function (x) {x[as.numeric(as.character(x))>13] <-NA; return(x)}
plot(MUAC5b,resid(.)~fitted(.)|mymap(CountyID), type=c("p","smooth") )

mymap<-function (x) {x[as.numeric(as.character(x))<14 |  as.numeric(as.character(x))>22] <-NA; return(x)}
plot(MUAC5b,resid(.)~fitted(.)|mymap(CountyID), type=c("p","smooth") )

mymap<-function (x) {x[as.numeric(as.character(x))<23 |  as.numeric(as.character(x))>47] <-NA; return(x)}
plot(MUAC5b,resid(.)~fitted(.)|mymap(CountyID), type=c("p","smooth") )

mymap<-function (x) {x[as.numeric(as.character(x))<47 ] <-NA; return(x)}
plot(MUAC5b,resid(.)~fitted(.)|mymap(CountyID), type=c("p","smooth") )




mymap<-function (x) {x[as.numeric(as.character(x))>13] <-NA; return(x)}
plot(MUAC5b,sqrt(abs(resid(.)))~fitted(.)|mymap(CountyID), type=c("p","smooth") )

mymap<-function (x) {x[as.numeric(as.character(x))<14 |  as.numeric(as.character(x))>22] <-NA; return(x)}
plot(MUAC5b,sqrt(abs(resid(.)))~fitted(.)|mymap(CountyID), type=c("p","smooth") )

mymap<-function (x) {x[as.numeric(as.character(x))<23 |  as.numeric(as.character(x))>47] <-NA; return(x)}
plot(MUAC5b,sqrt(abs(resid(.)))~fitted(.)|mymap(CountyID), type=c("p","smooth") )

mymap<-function (x) {x[as.numeric(as.character(x))<47 ] <-NA; return(x)}
plot(MUAC5b,sqrt(abs(resid(.)))~fitted(.)|mymap(CountyID), type=c("p","smooth") )
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# heteroscedasticity in residual accross counties..some literature suggests to specify variance structure = weights..
MUAC5<-lmer(MUACn~PreMedZl1+PreMedZl2+PreMedZl3+(PreMedZl1+PreMedZl2+PreMedZl3|CountyID),data=Phase23ts)
summary(MUAC5) 

MUAC5w<-lmer(MUACn~PreMedZl1+PreMedZl2+PreMedZl3+(PreMedZl1+PreMedZl2+PreMedZl3|CountyID),data=Phase23ts,weights=Area2014)
summary(MUAC5w) 

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  qq plot from lattice

qqmath(fm1,id=0.05)

qqmath(fm1,id=0.05)