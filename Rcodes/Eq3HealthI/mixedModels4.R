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
load("Main/Phase23.RData")

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


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

MUAC5w<-lmer(MUACn~PreMedZl1+PreMedZl2+PreMedZl3+(PreMedZl1+PreMedZl2+PreMedZl3|CountyID),data=Phase23ts,weights=Area2014/1000)
summary(MUAC5w) 


plot(MUAC5w)
plot(MUAC5)  # ehm. much nicer with PROXIMATE weights

plot(MUAC5w,sqrt(abs(residuals(.)))~fitted(.), type=c("p","smooth") )
plot(MUAC5w,sqrt(abs(resid(.)))~fitted(.)|CountyID, type=c("p","smooth") )

#----------------------------------------
# actually, which residuals am I getting?

plot(MUAC5)
plot(MUAC5,residuals(.)~fitted(.), type=c("p","smooth") )
residuals(MUAC5)
head(model.frame(MUAC5))
ranef(MUAC5)
fixef(MUAC5)
#Formula: MUACn ~ PreMedZl1 + PreMedZl2 + PreMedZl3 + (PreMedZl1 + PreMedZl2 +      PreMedZl3 | CountyID)
fit1= 11.5707    -0.7224*-0.74302783    -0.6953* 0.81588770     -0.5289387 *-0.95631822     -9.5359478 + 0.54933834* -0.74302783 +  0.75935296*0.81588770 +  0.61866683* -0.95631822  
res1=2.60-fit1  # okk

fit6= 11.5707    -0.7224* -0.15339467    -0.6953* 0.02012081      -0.5289387 * -0.05533213      -9.5359478 + 0.54933834*  -0.15339467 +  0.75935296* 0.02012081  +  0.61866683* -0.05533213    
res6=0.9-fit6  # okk, it generates the good residuals..
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  qq plot from lattice

qqmath(fm1,id=0.05)

qqmath(MUAC5,id=0.05)
qqmath(MUAC5w,id=0.05)


#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# posterior predictive simulations

iqrvec<-sapply(simulate(fm1,1000),IQR)
obsval<-IQR(sleepstudy$Reaction)
post.pred.p<-mean(obsval>=c(obsval,iqrvec))
post.pred.p