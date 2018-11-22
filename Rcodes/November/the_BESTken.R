rm(list=ls())
setwd("~/FoodSystemGitH/dataFS") 
setwd("dataFS") # home
library(dplyr); library(tseries); library(plm); library(nlme); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)

load("Main/DaTS.RData")
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

# arma(11) and ar(2) the best. AIC a bit better for 11>> claiming KenARe11 the best !!!
KenARe11<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
              +AvgTemp + SDTemp  + HWDays, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
              data=ScaledTS,na.action=na.exclude)

acf(summary(KenARe11)$resid)
pacf(summary(KenARe11)$resid)

summary(KenARe11)

#----------------------------------------------------------------
Ken112<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
              +AvgTemp   + HWDays, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
              data=ScaledTS,na.action=na.exclude)
summary(Ken112)

curve(exp(summary(Ken112)$coef[[1]]["SeasPr"]*x)*exp(summary(Ken112)$coef[[1]]["I(SeasPr^2)"]*x^2 ),-2,3)
# v. nice


#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
Ken113<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
            +AvgTemp   + HWDays, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
            data=ScaledTS[ScaledTS$ASAL==1,],na.action=na.exclude)
summary(Ken113)

Ken113<-lme(log(Yield0)~SeasPr+CVPrec+Spell+Spell4
            +AvgTemp   + HWDays, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
            data=ScaledTS[ScaledTS$ASAL==1,],na.action=na.exclude)
summary(Ken113)


Ken113<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+Spell+Spell4
            +AvgTemp   + HWDays, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
            data=ScaledTS[ScaledTS$ASAL==1,],na.action=na.exclude)
summary(Ken113)


#-------------------------------------------------------------------------------------------------------------------------
Ken114<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
            +AvgTemp   , random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
            data=ScaledTS[ScaledTS$ASAL==0,],na.action=na.exclude)
summary(Ken114)
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#plots..

xyplot(profile(Ken112)) 

plot(Ken112)
plot(Ken112,type=c("p","smooth")) 
plot(Ken112,sqrt(abs(resid(.)))~fitted(.), type=c("p","smooth"))
sqrt(abs(resid(.)))
qqmath(Ken112,id=0.05)

plot(resid(Ken112,type="pearson"))
which(resid(Ken112,type="pearson")<(-6))


#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
