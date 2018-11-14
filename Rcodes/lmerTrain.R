rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)
library(plm)
library(lme4)

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
RShowDoc("lmerperf",package="lme4")

str(sleepstudy)

fm1<-lmer(Reaction~Days+(Days|Subject),sleepstudy)
summary(fm1)
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

parsedFormula<-lFormula(formula=Reaction~Days+(Days|Subject),data=sleepstudy)
parsedFormula

devianceFunction<-do.call(mkLmerDevfun,parsedFormula)
devianceFunction

optimizerOutput<-optimizeLmer(devianceFunction)
optimizerOutput

mkMerMod(rho=environment(devianceFunction),
         opt=optimizerOutput,
         reTrms=parsedFormula$reTrms,
         fr=parsedFormula$fr)

summary(mkMerMod(rho=environment(devianceFunction),
         opt=optimizerOutput,
         reTrms=parsedFormula$reTrms,
         fr=parsedFormula$fr))

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

fm1<-lmer(Reaction~Days+(Days|Subject),sleepstudy)
summary(fm1)

fm2<-lmer(Reaction~Days+(Days||Subject),sleepstudy)
summary(fm2)
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fm3<-lmer(Reaction~I(10+Days)+(I(10+Days)|Subject),sleepstudy)
summary(fm3)

fm4<-lmer(Reaction~I(100+Days)+(I(100+Days)||Subject),sleepstudy)
summary(fm4)
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

fm1<-lmer(Reaction~Days+(Days|Subject),sleepstudy)
summary(fm1)

fm2<-lm(Reaction~Days+Subject+Days:Subject,sleepstudy)
summary(fm2)

sleepstudy$Group<-as.factor(sample(c(1,2,3,4),size=180,replace=TRUE,prob=c(0.15,0.30,0.2,0.35)))
sleepstudy$gender<-as.factor(sample(c(1,0),size=180,replace=TRUE,prob=c(0.4,0.6)))

fm2<-lm(Reaction~Days+Days:gender,sleepstudy)
summary(fm2)

fm2<-lmer(Reaction~Days+gender+(Days|gender),sleepstudy)
summary(fm2)


fm2<-lmer(Reaction~Days+(Days|gender),sleepstudy)
summary(fm2)


fm2<-lm(Reaction~Days+I(Days*as.numeric(gender)),sleepstudy)
summary(fm2)


fm3<-lmer(Reaction~1+Days+(Days|Group),sleepstudy)
summary(fm3)

fm2<-lm(Reaction~Days+Group+(Days:Group),sleepstudy)
summary(fm2)