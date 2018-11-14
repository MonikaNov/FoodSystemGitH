rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

load("Main/CrMaize8.RData")
library(nlme)
library(lme4)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------------------------------------
#

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
#
Kyle012<-lme(Yield~PrecZscore +TempZscore,random= ~1+PrecZscore+TempZscore|ID ,data=CrMaize8,control = lmeControl(opt = "optim"))
summary(Kyle012) 

Kyle12<-lme(Yield~1+PrecZscore +TempZscore,random= ~PrecZscore+TempZscore|ID 
            ,data=CrMaize8,control = lmeControl(opt = "optim"),correlation=corAR1(0,form= ~ as.numeric(Year)|ID))
summary(Kyle12) 
anova(Kyle12)

coef(Kyle12$modelStruct$corStruct, unconstrained = FALSE)

coef(Kyle12$modelStruct$corStruct, unconstrained = FALSE)

Kyle212<-lme(Yield~1+PrecZscore +TempZscore,random= ~PrecZscore+TempZscore|ID 
            ,data=CrMaize9,control = lmeControl(opt = "optim"),correlation=corARMA(form = ~ as.numeric(Year)|ID, p=0,q=1))
summary(Kyle212) 

Kyle213<-lme(Yield~1+PrecZscore +TempZscore,random= ~PrecZscore+TempZscore|ID 
             ,data=CrMaize8,control = lmeControl(opt = "optim"),correlation=corARMA(form = ~ as.numeric(Year)|ID, p=0,q=3))
summary(Kyle213) 
anova(Kyle213)
anova(Kyle12)

coef(Kyle213$modelStruct$corStruct, unconstrained = FALSE)

anova(Kyle212,Kyle213)

anova(Kyle12,Kyle213)
anova(Kyle12,Kyle212)

plot(Kyle2s)
plot(Kyle2s,type=c("p","smooth"))
plot(Kyle2s,sqrt(abs(resid(.)))~fitted(.),type=c("p","smooth"))
qqmath(Kyle2s,id=0.05)

plot(Kyle12,type=c("p","smooth"))
plot(Kyle12,sqrt(abs(resid(.)))~fitted(.),type=c("p","smooth"))
qqmath(Kyle12,id=0.05)

plot(Kyle213,type=c("p","smooth"))
plot(Kyle213,sqrt(abs(resid(.)))~fitted(.),type=c("p","smooth"))
qqmath(Kyle213,id=0.05)
#      !!!!!!!!!!!!!           THE BEST Kyle213          !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# more tests:

Kyle129<-gls(Yield~1+PrecZscore +TempZscore ,data=CrMaize9,control = lmeControl(opt = "optim"))
summary(Kyle129) 

anova(Kyle213,Kyle129)

Kyle1296<-gls(Yield~1+PrecZscore +TempZscore ,data=CrMaize9,control = lmeControl(opt = "optim")
              ,correlation=corARMA(form = ~ as.numeric(Year)|ID, p=0,q=3))
summary(Kyle1296) 


anova(Kyle213,Kyle1296)

Kyle012<-lme(Yield~PrecZscore +TempZscore,random= ~1+PrecZscore+TempZscore|ID ,data=CrMaize8,control = lmeControl(opt = "optim"))
summary(Kyle012)
anova(Kyle213,Kyle012)

Kyle2138<-lme(Yield~1+PrecZscore +TempZscore,random= ~PrecZscore+TempZscore|ID 
             ,data=CrMaize9,control = lmeControl(opt = "optim"),correlation=corARMA(form = ~ as.numeric(Year)|ID, p=0,q=1))
summary(Kyle2138) 
anova(Kyle213,Kyle2138)

Kyle2139<-lme(Yield~1+PrecZscore +TempZscore,random= ~PrecZscore+TempZscore|ID 
              ,data=CrMaize9,control = lmeControl(opt = "optim"),correlation=corARMA(form = ~ as.numeric(Year)|ID, p=0,q=2))
summary(Kyle2139) 
anova(Kyle213,Kyle2139)

#  OK KYLE 213 REALLY THE BEST:::...  but maybe better keep Kyle12 as the best for parsimony

# Kyle213<-lme(Yield~1+PrecZscore +TempZscore,random= ~PrecZscore+TempZscore|ID ,data=CrMaize9,control = lmeControl(opt = "optim"),correlation=corARMA(form = ~ as.numeric(Year)|ID, p=0,q=3))


Kyle2137<-lme(Yield~1+PrecZscore +TempZscore,random= ~PrecZscore+TempZscore|ID 
              ,data=CrMaize9,control = lmeControl(opt = "optim"),correlation=corARMA(form = ~ as.numeric(Year)|ID, p=0,q=6))
summary(Kyle2137) 

anova(Kyle213,Kyle2137)

summary(Kyle12)
summary(Kyle213)
summary(Kyle2137)  # good estimates look extremely similar. So maybe the best would be to use Kyle12
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# now the weighted version


Kyle051w<-lme(Yield~PrecZscore +PrecZonVar,random= ~1+PrecZscore+TempZscore|ID,weights=~AreaSc ,data=CrMaize8,control = lmeControl(opt = "optim"))
summary(Kyle051w) 

Kyle151w<-lme(Yield~1+PrecZscore +PrecZonVar,random= ~1+PrecZscore+TempZscore|ID,weights=~AreaSc  ,data=CrMaize8,
              correlation=corAR1(0,form= ~ as.numeric(Year)|ID))
summary(Kyle151w) # exactly the same as Kyle152w with different optimizer

KyW<-lme(Yield~1+PrecZscore +PrecZonVar,random= ~1+PrecZscore+TempZscore|ID,weights=~AreaSc  ,data=CrMaize8,
         correlation=corARMA(form = ~ as.numeric(Year)|ID, p=0,q=1))
summary(KyW) 


KyW2<-lme(Yield~1+PrecZscore +PrecZonVar,random= ~1+PrecZscore+TempZscore|ID,weights=~AreaSc  ,data=CrMaize8,
         correlation=corARMA(form = ~ as.numeric(Year)|ID, p=1,q=1))
summary(KyW2) 

KyW22<-lme(Yield~1+PrecZscore +PrecZonVar,random= ~1+PrecZscore+TempZscore|ID,weights=~AreaSc  ,data=CrMaize8,
          correlation=corARMA(form = ~ as.numeric(Year)|ID, p=2,q=2))
summary(KyW22) 
anova(KyW22)

KyW02<-lme(Yield~1+PrecZscore +PrecZonVar,random= ~1+PrecZscore+TempZscore|ID,weights=~AreaSc  ,data=CrMaize8,
           correlation=corARMA(form = ~ as.numeric(Year)|ID, p=0,q=2))
summary(KyW02) 


anova(Kyle051w,KyW2)
anova(KyW,KyW2)
anova(Kyle051w,KyW)
anova(Kyle051w,KyW4)

KyW3<-lme(Yield~1+PrecZscore +PrecZonVar,random= ~1+PrecZscore+TempZscore|ID,weights=~AreaSc  ,data=CrMaize8,
          correlation=corARMA(form = ~ as.numeric(Year)|ID, p=1,q=2))
summary(KyW3) 


KyW4<-lme(Yield~1+PrecZscore +PrecZonVar,random= ~1+PrecZscore+TempZscore|ID,weights=~AreaSc  ,data=CrMaize8,
          correlation=corARMA(form = ~ as.numeric(Year)|ID, p=3,q=1))
summary(KyW4) 

anova(KyW,KyW4) # KyW4 probably the best for now..


KyW5<-lme(Yield~1+PrecZscore +PrecZonVar,random= ~1+PrecZscore+TempZscore|ID,weights=~AreaSc  ,data=CrMaize8,
          correlation=corARMA(form = ~ as.numeric(Year)|ID, p=3,q=2))

anova(Kyle051w,KyW,KyW2,KyW3,KyW4,KyW5)

KyW6<-lme(Yield~1+PrecZscore +PrecZonVar,random= ~1+PrecZscore+TempZscore|ID,weights=~AreaSc  ,data=CrMaize8,
          correlation=corARMA(form = ~ as.numeric(Year)|ID, p=3,q=3),control = lmeControl(opt = "optim")) #doesnt go

anova(Kyle051w,KyW,KyW2,KyW3,KyW4,KyW5,KyW6)
#---------------------------------------------------------------------------------------------------------------------------



Kyle151w<-lme(Yield~1+PrecZscore +PrecZonVar,random= ~1+PrecZscore+TempZscore|ID,weights=~AreaSc  ,data=CrMaize8,
              correlation=corAR1(0,form= ~ as.numeric(Year)|ID))
summary(Kyle151w) # exactly the same as Kyle152w with different optimizer


KyW2<-lme(Yield~1+PrecZscore +PrecZonVar,random= ~1+PrecZscore+TempZscore|ID,weights=~AreaSc  ,data=CrMaize8,
          correlation=corARMA(form = ~ as.numeric(Year)|ID, p=1,q=1))
summary(KyW2) 

anova(Kyle151w,KyW2)



#---------------------------------------------------------------------------------------------------------------------------
#now zero q
summary(Kyle151w)

KyW7<-lme(Yield~1+PrecZscore +PrecZonVar,random= ~1+PrecZscore+TempZscore|ID,weights=~AreaSc  ,data=CrMaize8,
          correlation=corARMA(form = ~ as.numeric(Year)|ID, p=2,q=0))

anova(Kyle151w,KyW7)

KyW8<-lme(Yield~1+PrecZscore +PrecZonVar,random= ~1+PrecZscore+TempZscore|ID,weights=~AreaSc  ,data=CrMaize8,
          correlation=corARMA(form = ~ as.numeric(Year)|ID, p=3,q=0))
summary(KyW8)
anova(Kyle151w,KyW8)

KyW9<-lme(Yield~1+PrecZscore +PrecZonVar,random= ~1+PrecZscore+TempZscore|ID,weights=~AreaSc  ,data=CrMaize8,
          correlation=corARMA(form = ~ as.numeric(Year)|ID, p=4,q=0))
summary(KyW9)
anova(Kyle151w,KyW9)

KyW10<-lme(Yield~1+PrecZscore +PrecZonVar,random= ~1+PrecZscore+TempZscore|ID,weights=~AreaSc  ,data=CrMaize8,
          correlation=corARMA(form = ~ as.numeric(Year)|ID, p=5,q=0))
summary(KyW10)
anova(Kyle151w,KyW10)

KyW10<-lme(Yield~1+PrecZscore +PrecZonVar,random= ~1+PrecZscore+TempZscore|ID,weights=~AreaSc  ,data=CrMaize8,
           correlation=corARMA(form = ~ as.numeric(Year)|ID, p=8,q=0))
summary(KyW10)
anova(Kyle151w,KyW10)

anova(Kyle151w,KyW2)
anova(KyW2,KyW10)
anova(KyW4,KyW2)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Kyw22 better than all lower. what about upper?

KyW22<-lme(Yield~1+PrecZscore +PrecZonVar,random= ~1+PrecZscore+TempZscore|ID,weights=~AreaSc  ,data=CrMaize8,
           correlation=corARMA(form = ~ as.numeric(Year)|ID, p=2,q=2))
summary(KyW22) 

KyW23<-lme(Yield~1+PrecZscore +PrecZonVar,random= ~1+PrecZscore+TempZscore|ID,weights=~AreaSc  ,data=CrMaize8,
           correlation=corARMA(form = ~ as.numeric(Year)|ID, p=2,q=3))

KyW32<-lme(Yield~1+PrecZscore +PrecZonVar,random= ~1+PrecZscore+TempZscore|ID,weights=~AreaSc  ,data=CrMaize8,
           correlation=corARMA(form = ~ as.numeric(Year)|ID, p=3,q=2))

KyW42<-lme(Yield~1+PrecZscore +PrecZonVar,random= ~1+PrecZscore+TempZscore|ID,weights=~AreaSc  ,data=CrMaize8,
           correlation=corARMA(form = ~ as.numeric(Year)|ID, p=4,q=2))

anova(KyW22,KyW33)

# okkkkkkkkkkk  Kyw22 SEEMS TO BE THE BEST FOR NOW
anova(KyW22,KyW32)


###################################################################################################################################################################################
#

#just to compare with plm

Kyle12<-lme(Yield~1+PrecZscore +TempZscore,random= ~PrecZscore+TempZscore|ID 
            ,data=CrMaize8,control = lmeControl(opt = "optim"),correlation=corAR1(0,form= ~ as.numeric(Year)|ID))
summary(Kyle12) 


Kimmy30<-pvcm(Yield~PrecZscore+TempZscore,index=c("ID","Year"),model=c("random"), data=CrMaize8)
summary(Kimmy30)

Kimmy30<-pvcm(Yield~PrecZscore+TempZscore,index=c("ID","Year"),model=c("within"), data=CrMaize8)
summary(Kimmy30)