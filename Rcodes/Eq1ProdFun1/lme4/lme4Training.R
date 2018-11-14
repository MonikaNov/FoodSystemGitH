
library(lme4)
library(plm)
data("Wages",package="Ecdat")


fm1 <- lmer(Yield ~ 1 + (1|Batch), Dyestuff)
print(fm1)
summary(fm1)

(fm1ML<-lmer(Yield ~ 1 + (1|Batch), Dyestuff,REML=FALSE,verbose=TRUE))
fm1ML<-lmer(Yield ~  1|Batch, Dyestuff,REML=FALSE,verbose=TRUE)
fm1ML <- lmer(Yield ~ 1|Batch, Dyestuff, REML = FALSE, verbose = 2.4)
fm1ML <- lmer(Yield ~ 1|Batch, Dyestuff, REML = FALSE, verbose = 1.4)
(fm1ML <- lmer(Yield ~ 1|Batch, Dyestuff, REML = FALSE, verbose = 0.4))

fm3 <- lmer(Yield ~ 1 +Batch+ (1|Batch), Dyestuff)
print(fm3)
summary(fm3)

(fm2 <- lmer(Yield ~ 1 + (1|Batch), Dyestuff2))
(fm2ML <- update(fm2, REML="FALSE"))

summary(fm2ML)
(fm2OLS<-lm(Yield~1,Dyestuff2))
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------












fm2<- lm(Yield ~ Batch, data=Dyestuff)
print(fm2)
summary(fm2)

fm2<- pvcm(Yield ~ 1, data=Dyestuff,index=5,model="random",effect="individual")
print(fm2)
summary(fm2)