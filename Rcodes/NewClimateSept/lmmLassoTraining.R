rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)

library('dplyr')
library('tseries')
library(plm)
library(lme4)
library(lattice)
library(car)
library(lmerTest)
library(optimx)
library(lmmlasso)
library(lmmen)
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# I may have to scale evrything, otherwise it will never converge..


load("Main/MaizeClimate.RData")
MaizeClimateTS<-pdata.frame(MaizeClimate,index=c("ID1","Year"))
MaizeClimateScTS<-MaizeClimateTS
MaizeClimateScTS[,-c(1:5,32:34,38)]<-scale(MaizeClimateTS[,-c(1:5,32:34,38)])

xyz2<-MaizeClimate[c("Yield","ID1","SeasRain_MAM","SeasRain_OND","DrSpell20_MAM","DrSpell20_OND","PrecCoefVar_MAM","PrecCoefVar_OND")]
xyz2<-xyz2[complete.cases(xyz2),]
xyz<-cbind(seq(1),xyz2)
xyz<-as.matrix(xyz)

#scaling
MaizeClimateSc<-MaizeClimate
MaizeClimateSc[,-c(1:5,32:34,38)]<-scale(MaizeClimate[,-c(1:5,32:34,38)])

xyz2<-MaizeClimateSc[c("Yield","ID1","SeasRain_MAM","SeasRain_OND","DrSpell20_MAM","DrSpell20_OND","PrecCoefVar_MAM","PrecCoefVar_OND")]
xyz2<-xyz2[complete.cases(xyz2),]
xyz<-cbind(seq(1),xyz2)
xyz<-as.matrix(xyz)
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

l1=lmmlasso(x=xyz[,c(1,4:8)], 
            y=xyz[,2], grp=xyz[,3],lambda=10)

summary(l1)


l2=lmmlasso(x=xyz[,c(1,4:8)], 
            y=xyz[,2], grp=xyz[,3],lambda=0.000001)

summary(l2)

xyz2<-MaizeClimateSc[c("Yield","ID1","SeasRain_MAM","SeasRain_OND","DrSpell20_MAM","DrSpell20_OND","PrecCoefVar_MAM","PrecCoefVar_OND")]
xyz2<-xyz2[complete.cases(xyz2),]
xyz<-cbind(seq(1),xyz2)
xyz<-as.matrix(xyz)

colnames(xyz)<-c("X","y","grp","X","X","X","X","X","Z")


cvl<-cv.glmmLasso(xyz, xyz[,2]~xyz[,4], form.rnd = xyz[,2]~xyz[,2], lambda = seq(500, 0,
                                                                   by = -5), family = stats::gaussian(link = "identity"))

cvl<-cv.lmmlasso(l2,lambda = seq(0, 500, 5))


cvl=cv.lmmlasso(dat=xyz,lambda = seq(0, 500, 5))

summary(l2)

Cassey<-lmer(Yield~SeasRain_MAM +SeasRain_OND  + DrSpell20_MAM+DrSpell20_OND +PrecCoefVar_MAM+PrecCoefVar_OND
             +(SeasRain_MAM +SeasRain_OND  + DrSpell20_MAM+DrSpell20_OND|ID1),data=MaizeClimateScTS)
summary(Cassey) 

Cassey<-lmer(Yield~SeasRain_MAM +SeasRain_OND  + DrSpell20_MAM+DrSpell20_OND +PrecCoefVar_MAM+PrecCoefVar_OND
             +(SeasRain_MAM +SeasRain_OND  + DrSpell20_MAM+DrSpell20_OND|ID1),
             data=MaizeClimateScTS[complete.cases(MaizeClimateScTS==TRUE),])
summary(Cassey) 


Setppie<-step(Cassey)
summary(Setppie) 
summary(get_model(Setppie))




Cassey<-lmer(Yield~scale(SeasRain_MAM) +scale(SeasRain_OND)  
             +(scale(SeasRain_MAM) +scale(SeasRain_OND)|ID1),data=MaizeClimateTS)
summary(Cassey) 




Cassey<-lmer(log(Yield)~SeasRain_MAM +SeasRain_OND  + DrSpell20_MAM+DrSpell20_OND +PrecStDev_MAM+PrecStDev_OND
             +(SeasRain_MAM +SeasRain_OND  + DrSpell20_MAM+DrSpell20_OND|ID1),data=MaizeClimateTS,
             control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-nlminb-B')))
summary(Cassey) 

Cassey<-lmer(log(Yield)~SeasRain_MAM +SeasRain_OND  + DrSpell20_MAM+DrSpell20_OND +PrecStDev_MAM+PrecStDev_OND
             +(SeasRain_MAM +SeasRain_OND  + DrSpell20_MAM+DrSpell20_OND|ID1),data=MaizeClimateTS)
summary(Cassey) 

Cassey<-lmer(Yield~SeasRain_MAM +SeasRain_OND
             +(SeasRain_MAM +SeasRain_OND|ID1),data=MaizeClimateTS)
summary(Cassey) 



Cassey<-lmer(log(Yield)~SeasRain_MAM +SeasRain_OND 
             +(1|ID1),data=MaizeClimateTS)
summary(Cassey) 

Cassey2<-lmer(Yield~SeasRain_OND +SeasRain_MAM 
             +(1|ID1),data=MaizeClimateTS)
summary(Cassey2) 

Cassey3<-lmer(Yield~.-ID1+(1|ID1),data=MaizeClimateTS[c(1,21,29,36)])
summary(Cassey3) 


lm(y ~ ., data = MaizeClimateTS[c(1,21,29,36])

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# first models (a suggestion from email 20.5.2018): lmer(yield~rain+temp+cv_rain+cv_temp+(1+rain+temp|counties))
