rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma
# load("~/foodSystems/Rcodes/octoberNew/Models1.RData")

library(dplyr); library(tseries); library(plm); library(lme4); library(lattice); library(car); library(lmerTest); library(sandwich); library(lmtest)
setwd(WDhome)
setwd(WDuni)
load("Main/isdataTS.RData")

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
AmBEST<-lmer(log(isdataTS$Yield)~SeasPr + I(SeasPr^2) + I(SeasPr * AvgTemp) + 
               AvgTemp + CVPrec + SDTemp + (1 | ID1),data=isdataScTS)
summary(AmBEST)
rem<-c("27-1995",names(which(resid(AmBEST2,type="pearson")<(-2))))
# so this model, AmBEST3 is essentially the same specifiction as the AmBEST, the difference is that I got rid of the residual outliers..

AmBEST3<-lmer(log(isdataTS$Yield[!(rownames(isdataScTS) %in% rem)])~SeasPr + I(SeasPr^2) + I(SeasPr * AvgTemp) + 
                AvgTemp + CVPrec + SDTemp + (1 | ID1),data=isdataScTS[!(rownames(isdataScTS) %in% rem),])

pbsytest(log(isdataTS$Yield)~SeasPr + I(SeasPr^2) + I(SeasPr * AvgTemp) + AvgTemp + CVPrec + SDTemp +ID1,data=isdataScTS,test="J")

pbsytest(log(isdataTS$Yield[!(rownames(isdataScTS) %in% rem)])~SeasPr + I(SeasPr^2) + I(SeasPr * AvgTemp) +
           AvgTemp + CVPrec + SDTemp ,data=isdataScTS[!(rownames(isdataScTS) %in% rem),],test="J")
pbsytest(log(isdataTS$Yield[!(rownames(isdataScTS) %in% rem)])~SeasPr + I(SeasPr^2) + I(SeasPr * AvgTemp) +
           AvgTemp + CVPrec + SDTemp,data=isdataScTS[!(rownames(isdataScTS) %in% rem),])

pbsytest(log(isdataTS$Yield)~SeasPr + I(SeasPr^2) + I(SeasPr * AvgTemp) +
           AvgTemp + CVPrec + SDTemp,data=isdataScTS)

pbsytest(log(isdataTS$Yield[!(rownames(isdataScTS) %in% rem)])~SeasPr + I(SeasPr^2) + I(SeasPr * AvgTemp) +
           AvgTemp + CVPrec + SDTemp +ID1,data=isdataScTS[!(rownames(isdataScTS) %in% rem),])


pbltest(log(Yield+2)~SeasPr + I(SeasPr^2) + I(SeasPr * AvgTemp) +AvgTemp + CVPrec + SDTemp,data=isdataScTS)

# test of AR(1) and MA(1) errors
pbltest(log(Yield+2)~SeasPr + I(SeasPr^2) + I(SeasPr * AvgTemp) +AvgTemp + CVPrec + SDTemp,data=isdataScTS,alternative='onesided')
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# in the paper, I could also show the following varieties:
