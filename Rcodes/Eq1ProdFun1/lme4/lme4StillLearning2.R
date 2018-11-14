rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

load("~/foodSystems/dataFS/Main/CrMaize8.RData")
library(plm)
library(lme4)
#---------------------------------------------------------------------------------------------------------------------------------------------------------------

Emily2<-lmer(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar+(PrecZones+PrecZonVar+TempZones+TempZonVar|ID),data=CrMaize8)
summary(Emily2)

Emily2s<-lmer(Yield~scale(PrecZones, center=FALSE)+PrecZonVar
             +scale(TempZones, center=FALSE)+TempZonVar+(scale(PrecZones, center=FALSE)+PrecZonVar
      +scale(TempZones, center=FALSE)+TempZonVar|ID),data=CrMaize8)
summary(Emily2s)


Emily20<-lmer(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar+(PrecZones+PrecZonVar+TempZones+TempZonVar|ID),
              weights=Area,  data=CrMaize8)
summary(Emily20)

Emily200 <- update(Emily20,control=lmerControl(optimizer="Nelder_Mead"))
summary(Emily200)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
Kyle1<-lmer(Yield~PrecZones+TempZones+(PrecZones+TempZones|ID),data=CrMaize8)
summary(Kyle1)

# EVEN THE ORDER HERE GIVES DIFFERENT RESULTS
Kyle1<-lmer(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar+(TempZones+PrecZones|ID),data=CrMaize8)


# not the same!!! Kyle1<-lmer(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar+(PrecZones|ID)+(TempZones|ID),data=CrMaize8)


Kyle1w<-lmer(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar+(PrecZones+TempZones|ID),weights=Area,  data=CrMaize8)
summary(Kyle1w)

# scaling
#---------------------------------------------------------------------------------------------------------------------------------------------------------------
Kyle1s<-lmer(Yield~scale(PrecZones, center=FALSE) +PrecZonVar+scale(TempZones, center=FALSE)
             +TempZonVar+  (scale(PrecZones, center=FALSE)+scale(TempZones, center=FALSE)|ID),data=CrMaize8)
summary(Kyle1s)
CIKyle1s<-confint(Kyle1s)

fixef(Kyle1s)
ranef(Kyle1s)
anova(Kyle1s)
quantile(residuals(Kyle1s,"pearson",scaled="TRUE"))
library("lattice")
qqmath(Kyle1s,id=0.05)
qqmath(ranef(Kyle1s)[[1]][,2] ,id=0.2)

iqrvec <- sapply(simulate(Kyle1s, 1000), IQR)
obsval <- IQR(CrMaize8$Yield)
post.pred.p <- mean(obsval >= c(obsval, iqrvec))  # V. GOOOD  GOOD


iqrvec <- sapply(simulate(fm1, 1000), IQR)
obsval <- IQR(sleepstudy$Reaction)
post.pred.p <- mean(obsval >= c(obsval, iqrvec))

anova(Emily2s, Kyle1s,Kyle2s)                            

#THIS IS EXTREMLY GOOD >> IT SHOWS THAT THE VARS ARE PROBABLY NOT NEEDED

plot(Kyle1sw,type=c("p","smooth"))
plot(Kyle1s,sqrt(abs(resid(.))) ~ fitted(.), type=c("p","smooth"))


plot(CrMaize8$PrecZones[1:10],type=c("p","smooth"))
plot(Kyle1s)


Kyle1s0 <- update(Kyle1s,control=lmerControl(optimizer="Nelder_Mead")) # robustness
summary(Kyle1s0) #cool, seems to be exactly the same


Kyle1sw<-lmer(Yield~scale(PrecZones, center=FALSE) +PrecZonVar+scale(TempZones, center=FALSE)
              +TempZonVar+  (scale(PrecZones, center=FALSE)+scale(TempZones, center=FALSE)|ID),
              weights=AreaSc,data=CrMaize8)
summary(Kyle1sw)

Kyle1sw0 <- update(Kyle1sw,control=lmerControl(optimizer="Nelder_Mead")) # robustness
summary(Kyle1sw0) #cool, seems to be exactly the same
#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# now try to remove the var variables? they don't seem to be very significant...

Kyle2s<-lmer(Yield~scale(PrecZones, center=FALSE) +scale(TempZones, center=FALSE)
             +  (scale(PrecZones, center=FALSE)+scale(TempZones, center=FALSE)|ID),data=CrMaize8)
summary(Kyle2s)  # very similar to Kyle1s which is good
anova(Kyle2s)



Kyle2sw<-lmer(Yield~scale(PrecZones, center=FALSE) +scale(TempZones, center=FALSE)
              +  (scale(PrecZones, center=FALSE)+scale(TempZones, center=FALSE)|ID),
              weights=AreaSc,data=CrMaize8)
summary(Kyle2sw)  # very similar to Kyle2s which is good
