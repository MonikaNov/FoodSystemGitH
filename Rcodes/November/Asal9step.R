rm(list=ls())
setwd("foodSystems/dataFS") 
setwd("dataFS") # home
library(dplyr); library(tseries); library(plm); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)

load("Main/DaTS.RData")
# load("../Rcodes/octoberNew/Models1.RData")
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# spoiler alert: the result is that the best could be Shiv32B or Shiv4..
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

            #as discovered below, the best model from this category is probably Shiv32>>rename the particular one to Shiv32B:
                    Shiv32B<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec
                              + SDTemp  +(I(Prec2m^2)+Prec2m|ASAL),data=ScaledTS) 
                    summary(Shiv32B); vif(Shiv32B); ranova(Shiv32B)
                    # actually, Shiv14 seems to be better according to anova #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                    anova(Shiv14,Shiv32B)
                    Shiv14<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec+Spell+Spell4 +MaxP
                             + SDTemp + DDays + HWDays+MaxT+(I(Prec2m^2)+Prec2m|ASAL),data=ScaledTS) 
                    summary(Shiv14); vif(Shiv14); ranova(Shiv14)
            #but worse vif fo Shiv14...maybe search continues here
                      #from Asal8step.R script :
                            Shs3<-lmer(log(Yield0) ~ SeasPr + AvgTemp + Prec2m + CVPrec +
                                         (Prec2m + Spell4 | ASAL),data=ScaledTS)
                            summary(Shs3);ranova(Shs3);vif(Shs3)
                      anova(Shs3,Shiv32B)
                      anova(Shs3,Shiv4)
                      
                      
                      #just in case:
                      Shiv12s<-lmer(log(Yield0) ~ SeasPr + AvgTemp + Prec2m + CVPrec + DDays + MaxT + 
                                      (I(SeasPr^2) + Prec2m + Spell4 | ASAL),data=ScaledTS)# ehm..does not converge
                      anova(Shiv12s,Shiv32B)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Shiv32B<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec
              + SDTemp  +(I(Prec2m^2)+Prec2m|ASAL),data=ScaledTS) 
summary(Shiv32B); vif(Shiv32B); ranova(Shiv32B)
                    
Shiv4<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec
+ SDTemp  +HWDays+I(HWDays^2)+(I(Prec2m^2)+Prec2m|ASAL),data=ScaledTS) 
summary(Shiv4); vif(Shiv4); ranova(Shiv4)

Shiv4<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec
            + SDTemp  +MaxT+(I(Prec2m^2)+Prec2m|ASAL),data=ScaledTS) 
summary(Shiv4); vif(Shiv4); ranova(Shiv4)


Shiv4<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec
            + SDTemp  +Spell20+I(Spell20^2) +(I(Prec2m^2)+Prec2m|ASAL),data=ScaledTS) 
summary(Shiv4); vif(Shiv4); ranova(Shiv4); anova(Shiv32B,Shiv14,Shiv4) # anova not quite as I whish
hist(ScaledTS$Spell20,50)
curve(exp(summary(Shiv4)$coef["Spell20",1]*x)*exp(summary(Shiv4)$coef["I(Spell20^2)",1]*x^2 ),-1,3)

      
      Shiv41<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec
                  + SDTemp  +Spell4+I(Spell4^2) +(I(Prec2m^2)+Prec2m|ASAL),data=ScaledTS) 
      summary(Shiv41); vif(Shiv41); ranova(Shiv41); anova(Shiv32B,Shiv14,Shiv41) # anova not quite as I whish
      hist(ScaledTS$Spell4,50)
      curve(exp(summary(Shiv41)$coef["Spell4",1]*x)*exp(summary(Shiv41)$coef["I(Spell4^2)",1]*x^2 ),-3,2.5)

      
      Shiv42<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec
                   + SDTemp  +Spell4+I(sign(Spell4)*(Spell4^2)) +(I(Prec2m^2)+Prec2m|ASAL),data=ScaledTS) 
      summary(Shiv42); vif(Shiv42); ranova(Shiv42); anova(Shiv32B,Shiv14,Shiv42) # anova not quite as I whish
      hist(ScaledTS$Spell4,50)  #NOPE
      curve(exp(summary(Shiv42)$coef["Spell4",1]*x)*exp(summary(Shiv42)$coef["I(sign(Spell4) * (Spell4^2))",1]*x^2 ),-3,2)
      
      
      Shiv43<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec #N.O.P.E
                   + SDTemp  +Spell10+I(sign(Spell10)*(Spell10^2)) +(I(Prec2m^2)+Prec2m|ASAL),data=ScaledTS) 
      summary(Shiv43); vif(Shiv43); ranova(Shiv43); anova(Shiv32B,Shiv14,Shiv43) # anova not quite as I whish
      hist(ScaledTS$Spell10,50)  
      curve(exp(summary(Shiv43)$coef["Spell10",1]*x)*exp(summary(Shiv43)$coef["I(sign(Spell10) * (Spell10^2))",1]*x^2 ),-3,2)
      
      
      Shiv43<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec #N.O.P.E
                   + SDTemp  +Spell10 +(I(Prec2m^2)+Prec2m|ASAL),data=ScaledTS) 
      summary(Shiv43); vif(Shiv43); ranova(Shiv43); anova(Shiv32B,Shiv14,Shiv43) # anova not quite as I whish
      hist(ScaledTS$Spell10,50)  # NOOOOOO
   