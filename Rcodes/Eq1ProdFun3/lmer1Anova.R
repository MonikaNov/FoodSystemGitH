rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

load("~/foodSystems/dataFS/Main/CrMaize8.RData")
library(plm)
library(lme4)       # >>>>>>>>        WITH ZSCORES A BIT SMALLER P_VALS FOR PRECIPITATION THAN RAW SCALED>..  <<<<<<<<<<

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
#

Kyle1s<-lmer(Yield~1+PrecZscore+PrecZonVar+TempZscore+TempZonVar+
              (PrecZscore+TempZscore|ID), data=CrMaize8) 
summary(Kyle1s)

Kyle1sw<-lmer(Yield~PrecZscore+PrecZonVar+TempZscore+TempZonVar+
                (PrecZscore+TempZscore|ID),weights=AreaSc, data=CrMaize8) 
summary(Kyle1sw)    #  !!!!!!!       here temp really small t-value, probably switched from significant to insignificant    !!!!!!!  coef. value is much smaller if weighted, the SE quite similar

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Kyle2s<-lmer(Yield~PrecZscore +TempZscore  +  (PrecZscore+TempZscore|ID),data=CrMaize8)
summary(Kyle2s) # quite similar to Kyle 1s >>good

Kyle2sw<-lmer(Yield~PrecZscore +TempZscore  +  (PrecZscore+TempZscore|ID),  weights=AreaSc,data=CrMaize8)
summary(Kyle2sw) 
#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

anova(Kyle1s)
anova(Kyle1sw)
anova(Kyle2s)
anova(Kyle2sw)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
anova(Kyle1s,Kyle2s)
anova(Kyle1sw,Kyle2sw)

# for unweighted both variance coefs insignificant. I will have to do more tests for the weighted

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

# 1 Unweighted

# starting

Kyle2s<-lmer(Yield~PrecZscore +TempZscore  +  (PrecZscore+TempZscore|ID),data=CrMaize8)
summary(Kyle2s) 

#---------------------------------------------  ---------------  ---------------  ---------------   ---------------   ---------------  


Kyle21s<-lmer(Yield~ TempZscore+(PrecZscore+TempZscore|ID),data=CrMaize8)
anova(Kyle21s,Kyle2s)

Kyle22s<-lmer(Yield~ PrecZscore+(PrecZscore+TempZscore|ID),data=CrMaize8)
anova(Kyle22s,Kyle2s)

Kyle23s<-lmer(Yield~PrecZscore +TempZscore+(1+TempZscore|ID) ,data=CrMaize8)
anova(Kyle23s,Kyle2s)

Kyle24s<-lmer(Yield~PrecZscore +TempZscore+(1+PrecZscore|ID) ,data=CrMaize8)
anova(Kyle24s,Kyle2s)

# cann't leave out anything, Kyle2s the best

Kyle0<-lmer(Yield~(1|ID) ,data=CrMaize8)
summary(Kyle0)

anova(Kyle2s,Kyle0)

Kyle00<-lm(Yield~1 ,data=CrMaize8)
summary(Kyle00)

# the best: Kyle2s------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Kyle2s<-lmer(Yield~PrecZscore +TempZscore  +  (PrecZscore+TempZscore|ID),data=CrMaize8)
summary(Kyle2s) 

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
#   2 Weighted

# starting:

Kyle1sw<-lmer(Yield~PrecZscore+PrecZonVar+TempZscore+TempZonVar+
                (PrecZscore+TempZscore|ID),weights=AreaSc, data=CrMaize8) 
summary(Kyle1sw)  

#---------------------------------------------  ---------------  ---------------  ---------------   ---------------   ---------------  

Kyle3sw<-lmer(Yield~PrecZscore+PrecZonVar+TempZscore+
                (PrecZscore+TempZscore|ID),weights=AreaSc, data=CrMaize8) 
summary(Kyle3sw)

anova(Kyle3sw,Kyle1sw) # can continue with Kyle3sw. try leave out something more

#  Kyle4sw<-lmer(Yield~PrecZscore+PrecZonVar+TempZscore+
                (PrecZscore|ID),weights=AreaSc, data=CrMaize8) 
#  summary(Kyle4sw)

#  anova(Kyle4sw,Kyle3sw) # no

Kyle51sw<-lmer(Yield~PrecZscore+PrecZonVar+(PrecZscore+TempZscore|ID),weights=AreaSc, data=CrMaize8) 
summary(Kyle51sw)
anova(Kyle3sw,Kyle51sw)

# now Kyle51sw is the best. trying to leave out more parameters.  one by one coef..

Kyle511sw<-lmer(Yield~PrecZscore+PrecZonVar+ (PrecZscore|ID),weights=AreaSc, data=CrMaize8) 
Kyle512sw<-lmer(Yield~PrecZscore+PrecZonVar+(TempZscore|ID),weights=AreaSc, data=CrMaize8) 
Kyle513sw<-lmer(Yield~PrecZscore+(PrecZscore+TempZscore|ID),weights=AreaSc, data=CrMaize8) 
Kyle514sw<-lmer(Yield~PrecZonVar+(PrecZscore+TempZscore|ID),weights=AreaSc, data=CrMaize8) 

anova(Kyle51sw,Kyle511sw)
anova(Kyle51sw,Kyle512sw)
anova(Kyle51sw,Kyle513sw)
anova(Kyle51sw,Kyle514sw)

anova(Kyle51sw,Kyle00,Kyle0)

# the best: Kyle51sw------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Kyle51sw<-lmer(Yield~PrecZscore+PrecZonVar+(PrecZscore+TempZscore|ID),weights=AreaSc, data=CrMaize8) 
summary(Kyle51sw)

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
# so for unweighted Kyle2s the best. For weighted Kyle51sw the best