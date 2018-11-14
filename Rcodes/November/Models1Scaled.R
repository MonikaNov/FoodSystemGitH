rm(list=ls())
setwd("foodSystems/dataFS") 
# load("~/foodSystems/Rcodes/octoberNew/Models1.RData")

library(dplyr); library(tseries); library(plm); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)

load("Main/DaTS.RData")
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

Emmit0<-lm(Yield~SeasPr+AvgTemp,data=ScaledTS)
summary(Emmit0) 

Emmit0ln<-lm(log(Yield0)~SeasPr+AvgTemp,data=ScaledTS)
summary(Emmit0ln) 

Emmit1<-lmer(Yield~SeasPr+AvgTemp+(SeasPr+AvgTemp|ID1),data=ScaledTS)
summary(Emmit1) 
anova(Emmit1, Emmit0)
ranova(Emmit1)
ranova(Emmit1,reduce.terms=FALSE)
        Emmit15<-lmer(Yield~SeasPr+AvgTemp+(SeasPr+AvgTemp|ID1),data=ScaledTS[ScaledTS$ASAL==1,])
        summary(Emmit15)
        Emmit16<-lmer(Yield~SeasPr+AvgTemp+(SeasPr+AvgTemp|ID1),data=ScaledTS[ScaledTS$ASAL==0,])
        summary(Emmit16)

Emmit1ln<-lmer(log(Yield0)~SeasPr+AvgTemp+(SeasPr+AvgTemp|ID1),data=ScaledTS)
summary(Emmit1ln)
ranova(Emmit1ln) # interesting. Without the log of yield, seems that the random effects are actually needed. But not when log..

Emmit11ln<-lmer(log(Yield0)~SeasPr+AvgTemp+(0+SeasPr+AvgTemp|ID1),data=ScaledTS)
summary(Emmit11ln)
ranova(Emmit11ln)
ranova(Emmit11ln,reduce.terms = FALSE)
anova(Emmit11ln,Emmit1ln)

Emmit12ln<-lmer(log(Yield0)~SeasPr+AvgTemp+(1|ID1),data=ScaledTS) #, ok maybe just random intercept the best for now??
summary(Emmit12ln)
ranova(Emmit12ln)
anova(Emmit11ln,Emmit12ln)
anova(Emmit1ln,Emmit12ln)
        Emmit17ln<-lmer(log(Yield0)~SeasPr+AvgTemp+(1|ID1),data=ScaledTS[ScaledTS$ASAL==1,])
        summary(Emmit17ln)
        Emmit18ln<-lmer(log(Yield0)~SeasPr+AvgTemp+(1|ID1),data=ScaledTS[ScaledTS$ASAL==0,])
        summary(Emmit18ln)
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------

Emmit04<-lm(Yield~SeasPr+AvgTemp+I(SeasPr^2)+I(AvgTemp^2),data=ScaledTS)
summary(Emmit04)
    Emmit041<-lm(Yield~SeasPr+AvgTemp+I(SeasPr^2),data=ScaledTS)
    summary(Emmit041)

Emmit04ln<-lm(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+I(AvgTemp^2),data=ScaledTS)
summary(Emmit04ln) 
    Emmit041ln<-lm(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2),data=ScaledTS)
    summary(Emmit041ln) 

Emmit4<-lmer(Yield~SeasPr+AvgTemp+I(SeasPr^2)+I(AvgTemp^2)+(SeasPr+AvgTemp|ID1),data=ScaledTS)
summary(Emmit4) 
ranova(Emmit4);ranova(Emmit4,reduce.terms=FALSE)

Emmit5<-lmer(Yield~SeasPr+AvgTemp+I(SeasPr^2)+(SeasPr+AvgTemp|ID1),data=ScaledTS)
summary(Emmit5) 
ranova(Emmit5); ranova(Emmit5, reduce.terms = FALSE)


Emmit6<-lmer(Yield~SeasPr+AvgTemp+I(SeasPr^2)+(SeasPr|ID1),data=ScaledTS)
summary(Emmit6) 
ranova(Emmit6); ranova(Emmit6, reduce.terms = FALSE)
anova(Emmit6,Emmit5)
      Emmit6b<-lmer(Yield~SeasPr+AvgTemp+I(SeasPr^2)+(0+SeasPr|ID1),data=ScaledTS)
      summary(Emmit6b)
      anova(Emmit6,Emmit6b)

Emmit7<-lmer(Yield~SeasPr+AvgTemp+I(SeasPr^2)+(1|ID1),data=ScaledTS)
summary(Emmit7) 
ranova(Emmit7); ranova(Emmit7, reduce.terms = FALSE)
anova(Emmit6,Emmit7)  # ok, it seems that precipitation should be in the random effects here. But what if ln yield instead of yield??
                      # interesting is, that without scaling it used to be temperature which used to stay in the random effects.

# so for the moment, Emmit6 seem sto be the best...
Emmit6<-lmer(Yield~SeasPr+AvgTemp+I(SeasPr^2)+(SeasPr|ID1),data=ScaledTS)
summary(Emmit6)
    Emmit61<-lmer(Yield~SeasPr+AvgTemp+I(SeasPr^2)+(SeasPr|ID1),data=ScaledTS[ScaledTS$ASAL==1,])
    summary(Emmit61) 
       Emmit61<-lmer(Yield~SeasPr+AvgTemp+(SeasPr|ID1),data=ScaledTS[ScaledTS$ASAL==1,])
       summary(Emmit61) 
    Emmit62<-lmer(Yield~SeasPr+AvgTemp+I(SeasPr^2)+(SeasPr|ID1),data=ScaledTS[ScaledTS$ASAL==0,])
    summary(Emmit62)  # interesting
        FA<-function(x) {0.50052*x-(0.19307)*(x^2)}
        plot(FA((-2):2),type='l')
            hist(ScaledTS$SeasPr,40)
            
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# back to the log
Emmit12ln<-lmer(log(Yield0)~SeasPr+AvgTemp+(1|ID1),data=ScaledTS) #, ok maybe just random intercept the best for now??
summary(Emmit12ln)
ranova(Emmit12ln)

Emmit122ln<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+I(AvgTemp^2)+(1|ID1),data=ScaledTS) #
summary(Emmit122ln)

Emmit122ln<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+(1|ID1),data=ScaledTS) 
summary(Emmit122ln)
ranova(Emmit122ln)

Emmit123ln<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+(1|ID1),data=ScaledTS) 
summary(Emmit123ln)
ranova(Emmit123ln)  # ok, this one seems to be the best for now...
    Emmit124ln<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+(1|ID1),data=ScaledTS[ScaledTS$ASAL==1,]) 
    summary(Emmit124ln)
    ranova(Emmit124ln)
        Emmit124ln<-lmer(log(Yield0)~SeasPr+AvgTemp+(1|ID1),data=ScaledTS[ScaledTS$ASAL==1,]) 
        summary(Emmit124ln)
        ranova(Emmit124ln)
    Emmit125ln<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+(1|ID1),data=ScaledTS[ScaledTS$ASAL==0,]) 
    summary(Emmit125ln) 
    ranova(Emmit125ln)
        Emmit125ln<-lmer(log(Yield0)~SeasPr+I(SeasPr^2)+(1|ID1),data=ScaledTS[ScaledTS$ASAL==0,]) 
        summary(Emmit125ln) 
        ranova(Emmit125ln)
    
    #ok, it seems that for the counties that are not ASAL, precip is much more important...
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

# now trying to add an interaction of prec and temperature

# 1. Linear    

Emmit6<-lmer(Yield~SeasPr+AvgTemp+I(SeasPr^2)+(SeasPr|ID1),data=ScaledTS)
summary(Emmit6) 

Emmit7<-lmer(Yield~SeasPr*AvgTemp+(SeasPr|ID1),data=ScaledTS)
summary(Emmit7) 

Emmit70<-lmer(Yield~SeasPr*AvgTemp+(SeasPr*AvgTemp|ID1),data=ScaledTS)
summary(Emmit70) 
ranova(Emmit70)
    Emmit71<-lmer(Yield~SeasPr*AvgTemp+(SeasPr+AvgTemp+I(SeasPr*AvgTemp)|ID1),data=ScaledTS)
    summary(Emmit71) 
    ranova(Emmit71)
Emmit71<-lmer(Yield~SeasPr*AvgTemp+I(SeasPr^2)+I(AvgTemp^2)+(SeasPr|ID1),data=ScaledTS)
summary(Emmit71) 
ranova(Emmit71) 

Emmit72<-lmer(Yield~SeasPr*AvgTemp+I(SeasPr^2)+(SeasPr|ID1),data=ScaledTS)
summary(Emmit72) 
ranova(Emmit72) 

Emmit73<-lmer(Yield~SeasPr*AvgTemp+I(SeasPr^2)+(1|ID1),data=ScaledTS)
summary(Emmit73) 
ranova(Emmit73) 
# 1. Log-Linear   --------------------------------------------------------------------------------------------------------------------------------------------------------

Emmit9ln<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+I(AvgTemp^2)+I(SeasPr*AvgTemp)+(1|ID1),data=ScaledTS) #
summary(Emmit9ln)
ranova(Emmit9ln)

Emmit91ln<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+I(AvgTemp^2)+I(SeasPr*AvgTemp)+(SeasPr+AvgTemp|ID1),data=ScaledTS) #
summary(Emmit91ln)
ranova(Emmit91ln)

Emmit92ln<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+I(AvgTemp^2)+I(SeasPr*AvgTemp)
                +(SeasPr+AvgTemp+I(SeasPr^2)+I(AvgTemp^2)+I(SeasPr*AvgTemp)|ID1),data=ScaledTS) #
summary(Emmit92ln)
ranova(Emmit92ln)
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Emmit8ln<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+I(SeasPr*AvgTemp)+(1|ID1),data=ScaledTS) #
summary(Emmit8ln)
ranova(Emmit8ln)
              FB = function(x) {exp(0.19159*x)*exp(  -0.05195*x^2)}
              curve(exp(0.19159*x)*exp(  -0.05195*x^2),-3,3 )
Emmit81ln<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+I(AvgTemp^2)+I(SeasPr*AvgTemp)+(SeasPr+AvgTemp|ID1),data=ScaledTS) #
summary(Emmit81ln)
ranova(Emmit81ln)

Emmit82ln<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+I(AvgTemp^2)+I(SeasPr*AvgTemp)
                +(SeasPr+AvgTemp+I(SeasPr^2)+I(AvgTemp^2)+I(SeasPr*AvgTemp)|ID1),data=ScaledTS) #
summary(Emmit82ln)
ranova(Emmit82ln)

Emmit83ln<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+I(SeasPr*AvgTemp)
                +(SeasPr+AvgTemp+I(SeasPr^2)+I(SeasPr*AvgTemp)|ID1),data=ScaledTS) #
summary(Emmit83ln)
ranova(Emmit83ln)
      curve(exp(  0.17433*x)*exp(   -0.06023*x^2),-3,3 )
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# save.image("../Rcodes/November/Models1Scaled.RData")