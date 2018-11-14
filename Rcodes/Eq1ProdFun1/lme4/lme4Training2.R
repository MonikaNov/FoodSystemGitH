
library(lme4)
library(plm)
fm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
summary(fm1)

fm5 <- lmer(Reaction ~ Days + (1+Days | Subject), sleepstudy)
summary(fm5)

fm6 <- lmer(Reaction ~ Days + (Days-1 | Subject), sleepstudy)
summary(fm6)


VarCorr(fm1)

fm1 <- lmer(Reaction ~ 1 + (1 | g), sleepstudy)
summary(fm1)


fm2 <- lmer(Reaction ~ Days +offset(as.numeric(Subject)-100)+ (Days | Subject), sleepstudy)
summary(fm2)


#ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
# my case

Dale401<-pvcm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"),weights=Area, model='random', data=CrMaize8)
summary(Dale401)

Dale401<-pvcm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"), model='random', data=CrMaize8)
summary(Dale401)

Dale451<-pvcm(Yield~PrecZones+TempZones,index=c("ID","Year"), model='random', data=CrMaize8)
summary(Dale451)

Emily<-lmer(Yield~1+PrecZones+PrecZonVar+TempZones+TempZonVar+( 1+PrecZones+PrecZonVar+TempZones+TempZonVar|ID),  data=CrMaize8)
summary(Emily)


Emily2<-lmer(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar+(PrecZones+PrecZonVar+TempZones+TempZonVar|ID),data=CrMaize8)
summary(Emily2)

Emily20<-lmer(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar+(PrecZones+PrecZonVar+TempZones+TempZonVar|ID),weights=Area,  data=CrMaize8)
summary(Emily20)

# !!!!!!!!!!!!!!!!!vahy - DOST DIVNY !!!!!!!!!!!!!!!!!!!
Emily20<-lmer(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar+(PrecZones+PrecZonVar+TempZones+TempZonVar|ID),weights=Area,  data=CrMaize8)
summary(Emily20)

Emily21<-lmer(Yield~PrecZones+TempZones+(PrecZones+PrecZonVar+TempZones+TempZonVar|ID),  data=CrMaize8)
summary(Emily21)


Emily22<-lmer(Yield~PrecZones+TempZones+(PrecZones+TempZones|ID),  data=CrMaize8)
summary(Emily22)
#-----------        try different optimizer   ---------------------------------         -----------------
Emily22NM <- update(Emily22,control=lmerControl(optimizer="Nelder_Mead"))
summary(Emily22NM)

Emily220<-lmer(Yield~PrecZones+TempZones+(PrecZones+TempZones|ID),weights=Area,  data=CrMaize8)
summary(Emily220)


Emily220exp<-lmer(Yield~PrecZones+TempZones+(1|ID)+(0+TempZones|ID)+(0+PrecZones|ID),weights=Area,  data=CrMaize8)
summary(Emily220exp)
 
# seems that prec not very much correlated ??( 0.07) try to model it as uncorrelated?
# NOW I AM READING THAT THERE IS DRAWBACK ASSOCIATED WITH THESE TYPES OF MODELS>>> DO NOT USE THEEEEEEEESE MODELS WITH UNCRORRELATED RANDOM EFFECTS
# ACTUALLY, I PROBABLY CAN USE IT HERE AS IT SEEMS TO BE OK FOR THE VARIABLES WHERE ZERO MEANINGFUL (WHICH YIELDS IS)
Emily22<-lmer(Yield~PrecZones+TempZones+(PrecZones+TempZones|ID),  data=CrMaize8)
summary(Emily22)

Emily22u<-lmer(Yield~PrecZones+TempZones+(1+TempZones|ID)+(0+PrecZones|ID),  data=CrMaize8)
summary(Emily22u) #ted si to nestezuje...?


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

bbb<-lmer(Yield~scale(PrecZones, center=FALSE) +scale(TempZones, center=FALSE)
             +  (scale(PrecZones, center=FALSE)|ID)+(scale(TempZones, center=FALSE)|ID),data=CrMaize8)
summary(bbb)

bbbd<-lm(Yield~scale(PrecZones, center=FALSE) +scale(TempZones, center=FALSE)+factor(ID),data=CrMaize8)
summary(bbbd)