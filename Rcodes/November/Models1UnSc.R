rm(list=ls())
setwd("foodSystems/dataFS") 
library(dplyr); library(tseries); library(plm); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)

load("Main/Da.RData")
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

Varga0<-lm(Yield~SeasPr+AvgTemp,data=DaTS)
summary(Varga0) 

Varga0ln<-lm(log(Yield)~SeasPr+AvgTemp,data=DaTS)
summary(Varga0ln) 
    Varga04ln<-lm(log(Yield)~SeasPr+AvgTemp,data=DaTS[DaTS$ASAL==1,])
    summary(Varga04ln)
    Varga04<-lm(Yield~SeasPr+AvgTemp+I(SeasPr^2),data=DaTS[DaTS$ASAL==1,])
    summary(Varga04)
    Varga04ln<-lm(log(Yield)~SeasPr+AvgTemp+I(SeasPr^2),data=DaTS[DaTS$ASAL==1,])
    summary(Varga04ln)
        hist(DaTS$SeasPr[DaTS$ASAL==1],40)
        hist(DaTS$SeasPr[DaTS$ASAL==0],40)
            myF<-function(x) {x=exp(0.0005834*x)*exp(-3.846e-07*x^2) }
            plot(myF(0:1500),type='l')
            
    Varga04ln<-lm(log(Yield)~SeasPr+AvgTemp,data=DaTS[DaTS$ASAL==0,])
    summary(Varga04ln)
    Varga04ln<-lm(log(Yield)~SeasPr+AvgTemp+I(SeasPr^2)+I(AvgTemp^2),data=DaTS[DaTS$ASAL==0,])
    summary(Varga04ln)
    
Varga01<-lm(Yield~SeasPr+AvgTemp+I(SeasPr^2)+I(AvgTemp^2),data=DaTS)
summary(Varga01) 

Varga01ln<-lm(log(Yield)~SeasPr+AvgTemp+I(SeasPr^2)+I(AvgTemp^2),data=DaTS)
summary(Varga01ln) 

Varga02<-lm(Yield~SeasPr+AvgTemp+I(SeasPr^2)+I(AvgTemp^2)+I(AvgTemp*SeasPr),data=DaTS)
summary(Varga02) # ehm.. interesting
    Varga03<-lm(Yield~SeasPr+AvgTemp+I(SeasPr^2)+I(AvgTemp^2)+I(AvgTemp*SeasPr),data=DaTS[DaTS$ASAL==TRUE,])
    summary(Varga03) 

    Varga02ln<-lm(log(Yield)~SeasPr+AvgTemp+I(SeasPr^2)+I(AvgTemp^2)+I(AvgTemp*SeasPr),data=DaTS)
summary(Varga02ln) 
#--------------------------
Varga1<-lmer(Yield~SeasPr+AvgTemp+(SeasPr+AvgTemp|ID1),data=DaTS)   # ehm..immediately fails to converge.. scaling will be necesary
summary(Varga1) 
ranova(Varga1)   # seems that R. effects shouldn't be there
    Varga1b<-lmer(Yield~SeasPr+AvgTemp+(0+SeasPr+AvgTemp|ID1),data=DaTS)
    summary(Varga1b) 

Varga1ln<-lmer(log(Yield)~SeasPr+AvgTemp+(SeasPr+AvgTemp|ID1),data=DaTS)
summary(Varga1ln)
ranova(Varga1ln)  # again... # seems that R. effects shouldn't be there.. so...
    Varga1bln<-lmer(log(Yield)~SeasPr+AvgTemp+(0+SeasPr+AvgTemp|ID1),data=DaTS)
    summary(Varga1bln)
#--------------------------
Varga2<-lmer(Yield~SeasPr+AvgTemp+(1|ID1),data=DaTS)   
summary(Varga2) 

Varga2ln<-lmer(log(Yield)~SeasPr+AvgTemp+(1|ID1),data=DaTS)
summary(Varga2ln)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Varga3<-lmer(Yield~SeasPr+AvgTemp+I(SeasPr^2)+I(AvgTemp^2)+(1|ID1),data=DaTS)   
summary(Varga3) 
      1.450e-03*x+ -6.040e-07 *x^2
      1.450e-03*1000+ -6.040e-07 *(1000^2)
      aa=function(x) {1.450e-03*x+ -6.040e-07 *(x^2)}
      plot(aa(0:1000),type="l") # nice..
Varga3ln<-lmer(log(Yield)~SeasPr+AvgTemp+I(SeasPr^2)+I(AvgTemp^2)+(1|ID1),data=DaTS)
summary(Varga3ln)

Varga4<-lmer(Yield~SeasPr+AvgTemp+I(SeasPr^2)+I(AvgTemp^2)+(SeasPr+AvgTemp+I(SeasPr^2)+I(AvgTemp^2)|ID1),data=DaTS)   
summary(Varga4) 
ranova(Varga4)


Varga41<-lmer(Yield~SeasPr+AvgTemp+I(SeasPr^2)+I(AvgTemp^2)+(AvgTemp|ID1),data=DaTS)   
summary(Varga41) 
ranova(Varga41)

    Varga42<-lmer(Yield~SeasPr+AvgTemp+I(SeasPr^2)+(AvgTemp|ID1),data=DaTS)   
    summary(Varga42) 
    ranova(Varga42)  # NICE.vGOOD
            Varga43<-lmer(Yield~SeasPr+AvgTemp+(AvgTemp|ID1),data=DaTS)   
            summary(Varga43) 
            ranova(Varga43) 
                  Varga44<-lmer(Yield~SeasPr+AvgTemp+I(SeasPr^2)+(AvgTemp|ID1),data=DaTS[DaTS$ASAL==TRUE,])
                  summary(Varga44)
                  nobs(Varga44)
                  ranova(Varga44)
                  Varga45<-lmer(Yield~SeasPr+AvgTemp+(0+AvgTemp|ID1),data=DaTS[DaTS$ASAL==TRUE,])
                  summary(Varga45)
                  nobs(Varga45)
                  ranova(Varga45,reduce=FALSE) 
                  Varga45ln<-lmer(log(Yield)~SeasPr+I(SeasPr^2)+AvgTemp+(AvgTemp|ID1),data=DaTS[DaTS$ASAL==TRUE,])
                  summary(Varga45ln)
                  nobs(Varga45ln)
                  ranova(Varga45ln,reduce=TRUE) 
                  
Varga4ln<-lmer(log(Yield)~SeasPr+AvgTemp+I(SeasPr^2)+I(AvgTemp^2)+(SeasPr+AvgTemp+I(SeasPr^2)+I(AvgTemp^2)|ID1),data=DaTS)   
summary(Varga4ln) 
ranova(Varga4ln)

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

# save.image("../Rcodes/November/Models1UnSc.RData")

