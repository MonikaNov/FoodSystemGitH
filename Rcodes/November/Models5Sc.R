rm(list=ls())
setwd("foodSystems/dataFS") 
# load("../Rcodes/octoberNew/Models1.RData")

library(dplyr); library(tseries); library(plm); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)

load("Main/DaTS.RData")
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# ok, so far Kendalln3 the best.. try to replace seas pr. with 2 months.. or something..

Kendalln3<-lmer(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
                +AvgTemp + SDTemp  + HWDays+(1|ID1),data=ScaledTS) 
summary(Kendalln3)
vif(Kendalln3)
anova(Kendalln3)
#-----------------------------------------------------------------------------------------------------
Kendalln31<-lmer(log(Yield0)~SeasPr+I(SeasPr^2)+Prec2m+CVPrec+Spell+Spell4
                +AvgTemp + SDTemp  + HWDays+(1|ID1),data=ScaledTS) 
summary(Kendalln31)
vif(Kendalln31)

Kendalln31<-lmer(log(Yield0)~Prec2m+I(Prec2m^2)+CVPrec+Spell+Spell4
                 +AvgTemp + SDTemp  + HWDays+(1|ID1),data=ScaledTS) 
summary(Kendalln31)
      Kendalln31<-lmer(log(Yield0)~Prec2m+CVPrec+Spell+Spell4
                 +AvgTemp + SDTemp  + HWDays+(1|ID1),data=ScaledTS) 
      summary(Kendalln31) 
      # ok prec2m no good at all.

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# ok, so far Kendalln3 the best.. 
      #   but I think that may not be very robust..try various varieties..:-)
      
Kendalln3<-lmer(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
                      +AvgTemp + SDTemp  + HWDays+(1|ID1),data=ScaledTS) 
summary(Kendalln3)
vif(Kendalln3)
    hist(ScaledTS$SeasPr,40)
    curve(exp(0.06198*x)*exp(-0.02927*(x^2)),-2,3 )

Kendalln32<-lmer(Yield~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
                +AvgTemp + SDTemp  + HWDays+(1|ID1),data=ScaledTS) 
summary(Kendalln32) #ok, this one seems relatively robust  NICE
vif(Kendalln32)
    hist(ScaledTS$SeasPr,40)
    hist(ScaledTS$Yield,40)
    curve(0.0677*x+-0.02988*(x^2),-2,3 )

#-----------------------------------------------------------
Kendalln3<-lmer(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
                    +AvgTemp + SDTemp  + HWDays+(1|ID1),data=ScaledTS) 
summary(Kendalln3)
ranova(Kendalln3)
ranova(Kendalln3,random.terms=FALSE)

    Kendalln33<-lmer(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
                +AvgTemp + SDTemp  + HWDays+(SeasPr+AvgTemp|ID1),data=ScaledTS) 
    summary(Kendalln33)
    ranova(Kendalln33); anova(Kendalln33,Kendalln3)
    
    Kendalln33<-lmer(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
                     +AvgTemp + SDTemp  + HWDays+(SeasPr+I(SeasPr^2)|ID1),data=ScaledTS) 
    summary(Kendalln33)
    ranova(Kendalln33); anova(Kendalln33,Kendalln3)
    
# ok so it seems that the random intercept is still the best..
#----------------------------------------------------------------------
# now ASAL and non-ASAL separately
    
Kendalln3<-lmer(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
                    +AvgTemp + SDTemp  + HWDays+(1|ID1),data=ScaledTS) 
summary(Kendalln3); ranova(Kendalln3); nobs(Kendalln3)


Kendalln34<-lmer(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
              +AvgTemp + SDTemp  + HWDays+(1|ID1),data=ScaledTS[ScaledTS$ASAL==1,]) 
summary(Kendalln34);  ranova(Kendalln34)
#ehm.seas pr. stopped being significant for the ASAL!!

Kendalln35<-lmer(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
                 +AvgTemp + SDTemp  + HWDays+(1|ID1),data=ScaledTS[ScaledTS$ASAL==0,]) 
summary(Kendalln35);  ranova(Kendalln35)
    # rank defficient..
        summary(ScaledTS$HWDays[ScaledTS$ASAL==0])
        summary(DaTS$HWDays[DaTS$ASAL==0])
        summary(DaTS["HWDays"]);   sd(DaTS$HWDays,na.rm=TRUE)  
#----------------------------------------------------------------------
# now look more into Kendalln34<=> why seas. pr not significant here. maybe also try different random effects?

Kendalln34<-lmer(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
                 +AvgTemp + SDTemp  + HWDays+(1|ID1),data=ScaledTS[ScaledTS$ASAL==1,]) 
summary(Kendalln34);  ranova(Kendalln34)

  Kendalln341<-lmer(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
                 +AvgTemp + SDTemp  + HWDays+(SeasPr+I(SeasPr^2)|ID1),data=ScaledTS[ScaledTS$ASAL==1,]) 
  summary(Kendalln341);  ranova(Kendalln341);  anova(Kendalln341,Kendalln34)
  
  Kendalln342<-lmer(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4+AvgTemp + SDTemp  + HWDays
                    +(SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4 +AvgTemp + SDTemp  + HWDays|ID1),
                    data=ScaledTS[ScaledTS$ASAL==1,]) # this way no...
  summary(Kendalln342);  ranova(Kendalln342);  anova(Kendalln342,Kendalln34)
  
#-  - - - - - - - - - - -
  
Kendalln34<-lmer(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
                   +AvgTemp + SDTemp  + HWDays+(1|ID1),data=ScaledTS[ScaledTS$ASAL==1,]) 
summary(Kendalln34);  ranova(Kendalln34)
vif(Kendalln34)
  Kendalln343<-lmer(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
                   +AvgTemp   + HWDays+(1|ID1),data=ScaledTS[ScaledTS$ASAL==1,]) 
  summary(Kendalln343);  ranova(Kendalln343)
  
  Kendalln343<-lmer(log(Yield0)~SeasPr+CVPrec+Spell+Spell4
                    +AvgTemp   + HWDays+(1|ID1),data=ScaledTS[ScaledTS$ASAL==1,]) 
  summary(Kendalln343);  ranova(Kendalln343)
  
  Kendalln343<-lmer(log(Yield0)~SeasPr+Spell+Spell4
                    +AvgTemp   + HWDays+(1|ID1),data=ScaledTS[ScaledTS$ASAL==1,]) 
  summary(Kendalln343);  ranova(Kendalln343)  
  
  Kendalln343<-lmer(log(Yield0)~Prec2m+Spell+Spell4
                    +AvgTemp   + HWDays+(1|ID1),data=ScaledTS[ScaledTS$ASAL==1,]) 
  summary(Kendalln343);  ranova(Kendalln343)  ;vif(Kendalln343)
  
  Kendalln343<-lmer(log(Yield0)~I(SeasPr-Prec2m)+Spell+Spell4
                    +AvgTemp   + HWDays+(1|ID1),data=ScaledTS[ScaledTS$ASAL==1,]) 
  summary(Kendalln343);  ranova(Kendalln343)  
  
  Kendalln343<-lmer(log(Yield0)~SeasPr+CVPrec+Spell+Spell4
                    +AvgTemp   + HWDays+(1|ID1),data=ScaledTS[ScaledTS$ASAL==1,]) 
  summary(Kendalln343);  ranova(Kendalln343)  ;  ranova(Kendalln343,reduce.terms=FALSE)
  
  Kendalln343<-lmer(log(Yield0)~SeasPr+I(SeasPre^2)+CVPrec+Spell+Spell4
                    +AvgTemp   +(1|ID1),data=ScaledTS[ScaledTS$ASAL==1,]) 
  summary(Kendalln343);  ranova(Kendalln343)  ;  ranova(Kendalln343,reduce.terms=FALSE)
  
  Kendalln344<-lmer(log(Yield0)~CVPrec+Spell+Spell4
                    +AvgTemp   + HWDays+(SeasPr+1|ID1),data=ScaledTS[ScaledTS$ASAL==1,]) 
  summary(Kendalln344);  ranova(Kendalln344)  
  
#----------------------------------------------------------------------------------------------------------------------------------
  
  # ok, so the best ones for the ASAL/non ASAL
# asi nejlepsi bude prezentovat Kendalln3 = subset pro ASAL/non-ASAL tak jak to je....
  
  
  
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
  
# save.image("../Rcodes/November/Models5Sc.RData")  