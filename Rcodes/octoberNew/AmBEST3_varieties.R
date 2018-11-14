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

rem<-c("27-1995",names(which(resid(AmBEST2,type="pearson")<(-2))))
# so this model, AmBEST3 is essentially the same specifiction as the AmBEST, the difference is that I got rid of the residual outliers..

AmBEST3<-lmer(log(isdataTS$Yield[!(rownames(isdataScTS) %in% rem)])~SeasPr + I(SeasPr^2) + I(SeasPr * AvgTemp) + 
                AvgTemp + CVPrec + SDTemp + (1 | ID1),data=isdataScTS[!(rownames(isdataScTS) %in% rem),])

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# in the paper, I could also show the following varieties:

AmBEST3_lm<-lm(log(isdataTS$Yield[!(rownames(isdataScTS) %in% rem)])~SeasPr + I(SeasPr^2) + I(SeasPr * AvgTemp) + 
            +                   AvgTemp + CVPrec + SDTemp +ID1,data=isdataScTS[!(rownames(isdataScTS) %in% rem),])
summary(AmBEST3_lm) 


AmBEST31<-lmer(log(isdataTS$Yield[!(rownames(isdataScTS) %in% rem)])~SeasPr + I(SeasPr^2) + I(SeasPr * AvgTemp) + 
                AvgTemp + CVPrec + SDTemp + (1+SeasPr+AvgTemp | ID1),data=isdataScTS[!(rownames(isdataScTS) %in% rem),])

summary(AmBEST31)  # cool, very similar estimates to AmBEST3

anova(AmBEST31,AmBEST3)
ranova(AmBEST31)    # based on these tests, it says that the prec and temp should NOT be in the RE

iqrvec<-sapply(simulate(AmBEST31,10000),IQR)
obsval<-IQR(log(isdataTS$Yield[!(rownames(isdataScTS) %in% rem)]),na.rm=TRUE)
post.pred.p<-mean(obsval>=c(obsval,iqrvec))
post.pred.p # I would say its relatively ok............ # good.but the simulation probably does not use RE. does it?


plot(AmBEST31)
plot(AmBEST31,type=c("p","smooth")) # maybe remove the extreme resid obervation???
plot(AmBEST31,sqrt(abs(resid(.)))~fitted(.), type=c("p","smooth"))
qqmath(AmBEST31,id=0.05)  # so the plots are almost equaly bad as those for AmBEST3, without prec and temp in RE


AmBEST01<-lmer(log(isdataTS$Yield)~SeasPr + I(SeasPr^2) + I(SeasPr * AvgTemp) + 
                 AvgTemp + CVPrec + SDTemp + (1+SeasPr+AvgTemp | ID1),data=isdataScTS)
summary(AmBEST01)  

iqrvec<-sapply(simulate(AmBEST01,10000),IQR)
obsval<-IQR(log(isdataTS$Yield),na.rm=TRUE)
post.pred.p<-mean(obsval>=c(obsval,iqrvec))
post.pred.p 


AmBEST02<-lmer(log(isdataTS$Yield)~SeasPr + I(SeasPr^2) + I(SeasPr * AvgTemp) + 
                 AvgTemp + CVPrec + SDTemp + (1+SDTemp| ID1),data=isdataScTS)
summary(AmBEST02)  
anova(AmBEST ,AmBEST02)  