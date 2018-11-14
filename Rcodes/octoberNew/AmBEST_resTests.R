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
# Amelie41ln<-lmer(log(isdataTS$Yield)~SeasPr+I(SeasPr^2)+ I(SeasPr*AvgTemp)+AvgTemp+       
  #                 Prec2m +CVPrec+Spell+Spell10+Spell20+MaxP+
  #                 SDTemp + DDays + HWDays + (1|ID1),data=isdataScTS)
# Am41lnstep<-step(Amelie41ln, keep=attr(terms(Amelie41ln), "term.labels")[1:4])  
# AmBEST<-get_model(Am41lnstep)                    # the best so far (1.11.2018), no to run. get it simply as follows:

AmBEST<-lmer(log(isdataTS$Yield)~SeasPr + I(SeasPr^2) + I(SeasPr * AvgTemp) + 
               AvgTemp + CVPrec + SDTemp + (1 | ID1),data=isdataScTS)
summary(AmBEST)
extractAIC(AmBEST)
anova(AmBEST)

xyplot(profile(AmBEST)) #looks linear/quadratic>>good ?? not sure about interpretation of this, though...
coef(AmBEST)
vif(AmBEST)
ranef(AmBEST)
getME(AmBEST)

plot(AmBEST)
plot(AmBEST,type=c("p","smooth")) # maybe remove the extreme resid obervation???
plot(AmBEST,sqrt(abs(resid(.)))~fitted(.), type=c("p","smooth"))
sqrt(abs(resid(.)))
qqmath(AmBEST,id=0.05)

plot(resid(AmBEST,type="pearson"))
which(resid(AmBEST,type="pearson")<(-6))

isdataTS[which(resid(AmBEST,type="pearson")<(-6)),]
isdataScTS[which(resid(AmBEST,type="pearson")<(-6)),]

#------------------------------------------------------------------------------------------------------------------------------------------------------
# ok, now I will try to re-estimate the AmBEST without the dodgy pearson resid..
which(resid(AmBEST,type="pearson")<(-6))

AmBEST2<-lmer(log(isdataTS$Yield[(!(rownames(isdataTS)=="27-1995"))])~SeasPr + I(SeasPr^2) + I(SeasPr * AvgTemp) + 
               AvgTemp + CVPrec + SDTemp + (1 | ID1),data=isdataScTS[(!(rownames(isdataScTS)=="27-1995")),])
summary(AmBEST2) # cool, looks similar as AmBEST 

plot(AmBEST2)
plot(AmBEST2,type=c("p","smooth")) # maybe remove the extreme resid obervation???
plot(AmBEST2,sqrt(abs(resid(.)))~fitted(.), type=c("p","smooth"))
sqrt(abs(resid(.)))
qqmath(AmBEST2,id=0.05) #ehm, maybe remove all residuals smaller than about -2??>>YES
#------------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------------

rem<-c("27-1995",names(which(resid(AmBEST2,type="pearson")<(-2))))
isdataScTS[rem,]
isdataScTS[rem,c("Yield",attr(terms(AmBEST), "term.labels")[c(1,4,5,6)] )]

test1<-isdataScTS[!(rownames(isdataScTS) %in% rem),]
setdiff(rownames(isdataScTS),rownames(test1))
which(! (rownames(isdataScTS) %in% rownames(test1)))
all.equal(test1,isdataScTS[-which(! (rownames(isdataScTS) %in% rownames(test1))),])


AmBEST3<-lmer(log(isdataTS$Yield[!(rownames(isdataScTS) %in% rem)])~SeasPr + I(SeasPr^2) + I(SeasPr * AvgTemp) + 
                AvgTemp + CVPrec + SDTemp + (1 | ID1),data=isdataScTS[!(rownames(isdataScTS) %in% rem),])
summary(AmBEST3) # cool, STILL looks similar as AmBEST 

plot(AmBEST3)
plot(AmBEST3,type=c("p","smooth")) # maybe remove the extreme resid obervation???
plot(AmBEST3,sqrt(abs(resid(.)))~fitted(.), type=c("p","smooth"))
sqrt(abs(resid(.)))

# still seem to be a bit heteroscedastic>>robust standard errors??
coeftest(AmBEST3,vcovHC(AmBEST3))

plot(sqrt(abs(resid(AmBEST))))
plot(sqrt(abs(resid(AmBEST2))))

plot(resid(AmBEST))
plot(resid(AmBEST2))
plot(resid(AmBEST3))
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# plots of random effects

dotplot(ranef(AmBEST3))
qqmath(ranef(AmBEST3))
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#posetrior predictive simulation to evaluate if the model is good:

iqrvec<-sapply(simulate(AmBEST,10000),IQR)
obsval<-IQR(log(isdataTS$Yield),na.rm=TRUE)
post.pred.p<-mean(obsval>=c(obsval,iqrvec))
post.pred.p # I would say its relatively ok............
anova(AmBEST)#


iqrvec<-sapply(simulate(AmBEST3,10000),IQR)
obsval<-IQR(log(isdataTS$Yield[!(rownames(isdataScTS) %in% rem)]),na.rm=TRUE)
post.pred.p<-mean(obsval>=c(obsval,iqrvec))
post.pred.p # I would say its relatively ok............
anova(AmBEST3)#

# I should also use anova to compare this, without the outliers with the original specification that also had the MaxP
Am40ln<-lmer(log(isdataTS$Yield[!(rownames(isdataScTS) %in% rem)])~ SeasPr + I(SeasPr^2) 
             + I(SeasPr * AvgTemp) +      AvgTemp + CVPrec + SDTemp + MaxT 
             + (1 | ID1),data=isdataScTS[!(rownames(isdataScTS) %in% rem),])
summary(Am40ln)
vif(Am40ln)
anova(Am40ln,AmBEST3) # ehm
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# and plots of effects

summary(AmBEST3)
hist(isdataScTS$SeasPr,40)
hist(isdataTS$Yield,40)

x<-0.2
exp(0.13108*x)*exp(-0.07478*(x)^2)

plot(function(x) exp(0.13108*x)*exp(-0.07478*(x)^2),-1.6,2.60    )
dotplot(ranef(AmBEST3))


#-------------------------------------------------------------------------------------------------------------------------------------------------------
# now the variant which only have the basic climate variables:

AmBEST00<-lmer(log(isdataTS$Yield)~SeasPr + I(SeasPr^2)  + 
               AvgTemp+ (1 | ID1),data=isdataScTS)
summary(AmBEST00)
extractAIC(AmBEST00)
anova(AmBEST00)
# and the one which has all of them, to write up the results

AmBEST99<-lmer(log(isdataTS$Yield)~SeasPr + I(SeasPr^2) + AvgTemp + I(SeasPr * AvgTemp) + 
                CVPrec + SDTemp + Prec2m+Spell+Spell10+MaxP+DDays+ HWDays + MaxT+ (1 | ID1),data=isdataScTS)
summary(AmBEST99)
extractAIC(AmBEST99)
anova(AmBEST99)
drop1(AmBEST99)