rm(list=ls())
setwd("foodSystems/dataFS") 
setwd("dataFS") # home
library(dplyr); library(tseries); library(plm); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)

load("Main/DaTS.RData")
# load("../Rcodes/octoberNew/Models1.RData")

    #as discovered below, the best model from this category is probably Shiv32>>rename the particular one to Shiv32B:
          Shiv32B<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec
                        + SDTemp  +(I(Prec2m^2)+Prec2m|ASAL),data=ScaledTS) 
          summary(Shiv32B); vif(Shiv32B); ranova(Shiv32B)
          # actually, Shiv14 seems to be better according to anova #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                    anova(Shiv14,Shiv32B)
           Shiv14<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec+Spell+Spell4 +MaxP
                       + SDTemp + DDays + HWDays+MaxT+(I(Prec2m^2)+Prec2m|ASAL),data=ScaledTS) 
          summary(Shiv14); vif(Shiv14); ranova(Shiv14)
                          #but worse vif fo Shiv14...maybe search continues in another script..
          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# ok, so far Kendalln3 the best.. 

# HERE I WILL TRY TO ESTIMATE THE MODELS>> FIND THE BEST SUBSET BUT USING GROUPING INTO ASAL AND NON-ASAL INSTEAD OF HTE COUNTIES

Kendalln0<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec+Spell+Spell4 +MaxP
                + SDTemp + DDays + HWDays+MaxT+(1|ID1),data=ScaledTS) # if without log, the random effects equation would be different, I think SeasPr would be there
summary(Kendalln0)
vif(Kendalln0)

Kendalln3<-lmer(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
                +AvgTemp + SDTemp  + HWDays+(1|ID1),data=ScaledTS) 
summary(Kendalln3)
vif(Kendalln3)

#----------------------------------------------------------------------------------------------------------------------------------------------
#

Shiv0<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec+Spell+Spell4 +MaxP
                + SDTemp + DDays + HWDays+MaxT+(1|ASAL),data=ScaledTS) 
summary(Shiv0)
vif(Shiv0)


Shiv03<-lmer(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
                +AvgTemp + SDTemp  + HWDays+(1|ASAL),data=ScaledTS) 
summary(Shiv03)
vif(Shiv03)
#----------------------------------------------------------------------------------------------------------------------------------------------
#  first focus on Shiv0 and its varieties..they will actually be Shiv1

Shiv0<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec+Spell+Spell4 +MaxP
            + SDTemp + DDays + HWDays+MaxT+(1|ASAL),data=ScaledTS) 
summary(Shiv0); vif(Shiv0); ranova(Shiv0); ranova(Shiv0, reduce.terms=FALSE)# ehm..random intercept does not seem to be needed
 

Shiv1<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec+Spell+Spell4 +MaxP
            + SDTemp + DDays + HWDays+MaxT+(SeasPr+AvgTemp|ASAL),data=ScaledTS) 
summary(Shiv1); vif(Shiv1); ranova(Shiv1)

Shiv1<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec+Spell+Spell4 +MaxP
            + SDTemp + DDays + HWDays+MaxT+(SeasPr+Prec2m|ASAL),data=ScaledTS) 
summary(Shiv1); vif(Shiv1); ranova(Shiv1)

Shiv11<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec+Spell+Spell4 +MaxP
             + SDTemp + DDays + HWDays+MaxT+(SeasPr+I(SeasPr^2)+Prec2m|ASAL),data=ScaledTS) 
summary(Shiv11); vif(Shiv11); ranova(Shiv11)

Shiv11<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec+Spell+Spell4 +MaxP
             + SDTemp + DDays + HWDays+MaxT+(Prec2m|ASAL),data=ScaledTS) 
summary(Shiv11); vif(Shiv11); ranova(Shiv11); ranova(Shiv11,reduce.terms=FALSE) 
#wellp, prec2m seems to be the most important RE

Shiv11<-lmer(log(Yield0)~SeasPr+AvgTemp+ Prec2m+CVPrec+Spell+Spell4 +MaxP
            + SDTemp + DDays + HWDays+MaxT+(SeasPr+Prec2m|ASAL),data=ScaledTS) 
summary(Shiv11); vif(Shiv11); ranova(Shiv11)

# interesting, Shiv14 seems to be the best for now
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Shiv14<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec+Spell+Spell4 +MaxP
             + SDTemp + DDays + HWDays+MaxT+(I(Prec2m^2)+Prec2m|ASAL),data=ScaledTS) 
summary(Shiv14); vif(Shiv14); ranova(Shiv14)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


Shiv12<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec+Spell+Spell4 +MaxP
             + SDTemp + DDays + HWDays+MaxT+(SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec+Spell+Spell4 +MaxP
    + SDTemp + DDays + HWDays+MaxT+I(SeasPr^2)+Prec2m|ASAL),data=ScaledTS) 
summary(Shiv12); vif(Shiv12); ranova(Shiv12) #

# this model took long time. None RE seem to be significant here. The closest to significant are:
#                Spell4 SeasPr^2 Prec2m and SeasPr 

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo


# interesting, Shiv14 seems to be the best for now
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Shiv14<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec+Spell+Spell4 +MaxP
             + SDTemp + DDays + HWDays+MaxT+(I(Prec2m^2)+Prec2m|ASAL),data=ScaledTS) 
summary(Shiv14); vif(Shiv14); ranova(Shiv14)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Shiv3<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec+Spell+Spell4 +MaxP
             + SDTemp + DDays + HWDays+MaxT+(I(Prec2m^2)+Prec2m+Spell4|ASAL),data=ScaledTS) 
summary(Shiv3); vif(Shiv3); ranova(Shiv3)

      Shiv3<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec+Spell+Spell4 +MaxP
            + SDTemp + DDays + HWDays+MaxT+(I(Prec2m^2)+Prec2m+Spell4+SeasPr|ASAL),data=ScaledTS) 
      summary(Shiv3); vif(Shiv3); ranova(Shiv3)
      #   NOPE

      # ehm..spell 20 actually is significant here with the correct sign
      # BUT FAIL TO CONVERGE
Shiv31<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec+Spell+Spell20 +MaxP
               + SDTemp + DDays + HWDays+MaxT+(I(Prec2m^2)+Prec2m+Spell4|ASAL),data=ScaledTS) 
summary(Shiv31); vif(Shiv31); ranova(Shiv31)

Shiv32<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec+Spell+Spell20 +MaxP
             + SDTemp + DDays + HWDays+(I(Prec2m^2)+Prec2m+Spell4|ASAL),data=ScaledTS) 
summary(Shiv32); vif(Shiv32); ranova(Shiv32)

Shiv32<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec+Spell+Spell20 
             + SDTemp  + HWDays+(I(Prec2m^2)+Prec2m|ASAL),data=ScaledTS) 
summary(Shiv32); vif(Shiv32); ranova(Shiv32)   


Shiv32<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec+Spell20 
             + SDTemp  + HWDays+(I(Prec2m^2)+Prec2m+Spell20|ASAL),data=ScaledTS) # nope..
summary(Shiv32); vif(Shiv32); ranova(Shiv32)   

Shiv32<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec+Spell20
             + SDTemp  +(I(Prec2m^2)+Prec2m|ASAL),data=ScaledTS) 
summary(Shiv32); vif(Shiv32); ranova(Shiv32)

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# now shiv32 seems to be the best from what I have, I can try some experiments on it..

Shiv32<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec
             + SDTemp  +(I(Prec2m^2)+Prec2m|ASAL),data=ScaledTS) 
summary(Shiv32); vif(Shiv32); ranova(Shiv32)
# the best model from this category is probably Shiv32>>rename the particular one to Shiv32B
Shiv32B<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec
             + SDTemp  +(I(Prec2m^2)+Prec2m|ASAL),data=ScaledTS) 
summary(Shiv32B); vif(Shiv32B); ranova(Shiv32B)
anova(Shiv14,Shiv32B )

summary(Shiv32)$coef["SeasPr",1]
summary(Shiv32)$coef["I(SeasPr^2)",1]

exp(summary(Shiv32)$coef["SeasPr",1]*x)*exp(summary(Shiv32)$coef["I(SeasPr^2)",1]*x^2 )
curve(exp(summary(Shiv32)$coef["SeasPr",1]*x)*exp(summary(Shiv32)$coef["I(SeasPr^2)",1]*x^2 ),-2,3)
hist(ScaledTS$SeasPr,50)

# now shiv32 seems to be the best from what I have, I can try some experiments on it..

Shiv32<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+ Prec2m+CVPrec
             + SDTemp  +(I(Prec2m^2)+Prec2m|ASAL),data=ScaledTS) 
summary(Shiv32); vif(Shiv32); ranova(Shiv32)
      Shiv321<-lmer(log(Yield0)~SeasPr+AvgTemp+I(SeasPr^2)+CVPrec
             + SDTemp  +(I(Prec2m^2)+Prec2m|ASAL),data=ScaledTS) 
      summary(Shiv321); vif(Shiv321); ranova(Shiv321) # failed to converge

      Shiv322<-lmer(log(Yield0)~SeasPr+I(SeasPr^2)+AvgTemp+Prec2m+I(Prec2m^2)+CVPrec
                    + SDTemp  +(I(Prec2m^2)+Prec2m|ASAL),data=ScaledTS) 
      summary(Shiv322); vif(Shiv322); ranova(Shiv322)#nope
      
      Shiv323<-lmer(log(Yield0)~I(SeasPr-Prec2m)+I((SeasPr-Prec2m)^2)+AvgTemp+Prec2m+I(Prec2m^2)+CVPrec
                    + SDTemp  +(I(Prec2m^2)+Prec2m|ASAL),data=ScaledTS) 
      summary(Shiv323); vif(Shiv323); ranova(Shiv323)
      
      Shiv323<-lmer(log(Yield0)~I(SeasPr-Prec2m)+I((SeasPr-Prec2m)^2)+AvgTemp+Prec2m+I(Prec2m^2)+CVPrec
                    + SDTemp  +(Prec2m|ASAL),data=ScaledTS) 
      summary(Shiv323); vif(Shiv323); ranova(Shiv323)
      
      
      
      Shiv323<-lmer(log(Yield0)~I(SeasPr-Prec2m)+AvgTemp+Prec2m+I(Prec2m^2)+CVPrec
                    + SDTemp  +(Prec2m|ASAL),data=ScaledTS) 
      summary(Shiv323); vif(Shiv323); ranova(Shiv323)
      hist(ScaledTS$Prec2m,50)
      curve(exp(summary(Shiv323)$coef["Prec2m",1]*x)*exp(summary(Shiv323)$coef["I(Prec2m^2)",1]*x^2 ),-2,3)
     # ehm..divny
      curve(exp(summary(Shiv323)$coef["SeasPr",1]*x)*exp(summary(Shiv323)$coef["I(SeasPr^2)",1]*x^2 ),-2,3)
      
          # remove the spare
          Shiv323<-lmer(log(Yield0)~I(SeasPr-Prec2m)+AvgTemp+Prec2m+I(Prec2m^2)+CVPrec
                   +(Prec2m|ASAL),data=ScaledTS) 
          summary(Shiv323); vif(Shiv323); ranova(Shiv323)
          
          Shiv323<-lmer(log(Yield0)~I(SeasPr-Prec2m)+I((SeasPr-Prec2m)^2)+AvgTemp+Prec2m+I(Prec2m^2)+CVPrec
                        +(Prec2m|ASAL),data=ScaledTS)   #nope
          summary(Shiv323); vif(Shiv323); ranova(Shiv323)
          
          Shiv323<-lmer(log(Yield0)~I(SeasPr-Prec2m)+AvgTemp+Prec2m+I(Prec2m^2)+CVPrec
                        +(Prec2m|ASAL),data=ScaledTS)   #nope
          summary(Shiv323); vif(Shiv323); ranova(Shiv323)
      
      
      
      Shiv323<-lmer(log(Yield0)~I(SeasPr-Prec2m)+I((SeasPr-Prec2m)^2)+AvgTemp+Prec2m+CVPrec
                    + SDTemp  +(I(Prec2m^2)+Prec2m|ASAL),data=ScaledTS) 
      summary(Shiv323); vif(Shiv323); ranova(Shiv323)
      
      Shiv323<-lmer(log(Yield0)~I(SeasPr-Prec2m)+AvgTemp+Prec2m+CVPrec
                    + SDTemp  +(I(Prec2m^2)+Prec2m|ASAL),data=ScaledTS) 
      summary(Shiv323); vif(Shiv323); ranova(Shiv323)
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

# save.image("../Rcodes/November/Models7.RData")