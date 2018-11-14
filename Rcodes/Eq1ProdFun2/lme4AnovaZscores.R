rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

load("~/foodSystems/dataFS/Main/CrMaize8.RData")
library(plm)
library(lme4)

# >>>>>>>>          NOTICED THAT WITH ZSCORES A BIT SMALLER P_VALS FOR PRECIPITATION THAN RAW SCALED>..  <<<<<<<<<<

# just trying to formalize what I have been doing in lme4StillLearning2
# Pedram suggested to use z-score to avoid the convergence problems
# Area weights need to be scaled too. Just got rid of the zero weight (one case) and devided by 10000 

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# I tried also Emilies1 but convergence problems if Variation coefficients also in random effects

Kyle1s<-lmer(Yield~1+PrecZscore+PrecZonVar+TempZscore+TempZonVar+
              (PrecZscore+TempZscore|ID), data=CrMaize8) 
summary(Kyle1s)

Kyle1sw<-lmer(Yield~PrecZscore+PrecZonVar+TempZscore+TempZonVar+
                (PrecZscore+TempZscore|ID),weights=AreaSc, data=CrMaize8) 
summary(Kyle1sw)    #  !!!!!!!       here temp really small t-value, probably switched from significant to insignificant    !!!!!!!  coef. value is much smaller if weighted, the SE quite similar
# But SE little bit bigger with weights. PrecVar seems to be more significant

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------------------------------------
Kyle2s<-lmer(Yield~PrecZscore +TempZscore  +  (PrecZscore+TempZscore|ID),data=CrMaize8)
summary(Kyle2s) # quite similar to Kyle 1s >>good

Kyle2sw<-lmer(Yield~PrecZscore +TempZscore  +  (PrecZscore+TempZscore|ID),  weights=AreaSc,data=CrMaize8)
summary(Kyle2sw)  #  !!!!!!!       here temp really small t-value, probably switched from significant to insignificant    !!!!!!!  coef. value is much smaller if weighted, the SE quite similar
# otherwise relatively similar. but for the weighted version REML criterion much higher, also the residuals quite big range
#---------------------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------------------------------------
anova(Kyle1s)
anova(Kyle1sw)
anova(Kyle2s)
anova(Kyle2sw)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------------------------------------
anova(Kyle1s,Kyle2s)
anova(Kyle1sw,Kyle2sw)

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# now even simpler models. For weighted, probably Prec and it Var and not Temp
# various combinations

Kyle21s<-lmer(Yield~ (PrecZscore+TempZscore|ID),data=CrMaize8)
summary(Kyle21s)
anova(Kyle21s,Kyle2s)

Kyle22s<-lmer(Yield~PrecZscore +TempZscore+(1|ID) ,data=CrMaize8)
summary(Kyle22s)
anova(Kyle22s,Kyle2s)


Kyle21s<-lmer(Yield~ TempZscore+(PrecZscore+TempZscore|ID),data=CrMaize8)
summary(Kyle21s)
anova(Kyle21s,Kyle2s)

Kyle22s<-lmer(Yield~PrecZscore +TempZscore+(1+TempZscore|ID) ,data=CrMaize8)
summary(Kyle22s)
anova(Kyle22s,Kyle2s)



Kyle0<-lmer(Yield~(1|ID) ,data=CrMaize8)
summary(Kyle0)

anova(Kyle2s,Kyle0)

Kyle00<-lm(Yield~1 ,data=CrMaize8)
summary(Kyle00)


#----  COOL KYLE  2 SEEEMS TO BE THE BEST FOR NON-WEIGHTED     ----------   ####    --------  ####   ----------   ####   ----------   ####   ----------   ####   ----------   ####   ----------   ####  

Kyle3sw<-lmer(Yield~PrecZscore+PrecZonVar+TempZscore+
                (PrecZscore+TempZscore|ID),weights=AreaSc, data=CrMaize8) 
summary(Kyle3sw)

anova(Kyle3sw,Kyle2sw,Kyle1sw)

Kyle2sw<-lmer(Yield~PrecZscore +TempZscore  +  (PrecZscore+TempZscore|ID),  weights=AreaSc,data=CrMaize8)
summary(Kyle2sw) 

# so far Kyle3sw is the best 


Kyle4sw<-lmer(Yield~PrecZscore+PrecZonVar+TempZscore+
                (PrecZscore|ID),weights=AreaSc, data=CrMaize8) 
summary(Kyle4sw)

anova(Kyle4sw,Kyle3sw)

Kyle51sw<-lmer(Yield~PrecZscore+PrecZonVar+
                (PrecZscore+TempZscore|ID),weights=AreaSc, data=CrMaize8) 
summary(Kyle51sw)

Kyle3sw<-lmer(Yield~PrecZscore+PrecZonVar+TempZscore+
                (PrecZscore+TempZscore|ID),weights=AreaSc, data=CrMaize8) 
summary(Kyle3sw)


Kyle50sw<-lmer(Yield~PrecZscore+PrecZonVar+
                 (PrecZscore|ID),weights=AreaSc, data=CrMaize8) 
summary(Kyle50sw)

anova(Kyle3sw,Kyle50sw,Kyle51sw)
anova(Kyle3sw,Kyle51sw)


# now Kyle51sw is the best 

Kyle51sw<-lmer(Yield~PrecZscore+PrecZonVar+
                 (PrecZscore+TempZscore|ID),weights=AreaSc, data=CrMaize8) 
summary(Kyle51sw)

Kyle511sw<-lmer(Yield~
                 (PrecZscore+TempZscore|ID),weights=AreaSc, data=CrMaize8) 
summary(Kyle511sw)

Kyle512sw<-lmer(Yield~PrecZscore+PrecZonVar+
                  (1|ID),weights=AreaSc, data=CrMaize8) 
summary(Kyle512sw)

anova(Kyle51sw,Kyle511sw)
anova(Kyle51sw,Kyle512sw)

# still Kyle51sw is the best -----------------------------------   -   - - - ------ -- -   - - - ------- -- 
# now I will try to remove one by one coef..

Kyle51sw<-lmer(Yield~PrecZscore+PrecZonVar+
                 (PrecZscore+TempZscore|ID),weights=AreaSc, data=CrMaize8) 
summary(Kyle51sw)

Kyle511sw<-lmer(Yield~PrecZscore+PrecZonVar+ (PrecZscore|ID),weights=AreaSc, data=CrMaize8) 
Kyle512sw<-lmer(Yield~PrecZscore+PrecZonVar+(TempZscore|ID),weights=AreaSc, data=CrMaize8) 
Kyle513sw<-lmer(Yield~PrecZscore+(PrecZscore+TempZscore|ID),weights=AreaSc, data=CrMaize8) 
Kyle514sw<-lmer(Yield~PrecZonVar+(PrecZscore+TempZscore|ID),weights=AreaSc, data=CrMaize8) 

anova(Kyle51sw,Kyle511sw)
anova(Kyle51sw,Kyle512sw)
anova(Kyle51sw,Kyle513sw)
anova(Kyle51sw,Kyle514sw)

anova(Kyle51sw,Kyle00,Kyle0)

###### ---  FOR WEIGHTED KYLE 51 THE BEST PROBABLY------########     -------------          ################# ----- #########

anova(Kyle51sw,Kyle2s)