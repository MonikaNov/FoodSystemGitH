rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

load("Main/CrMaize8.RData")
load("~/foodSystems/Rcodes/Eq1ProdFun3/lme4ZPvalsMore.RData") # confidence intervals take too long to compute


library(plm)
library(lme4)
library(lattice)
library(car)
library(lmerTest)
library(afex)
#.....................................................................................................................................................................................
#---------------------------------------------------------------------------------------------------------------------------------------------------------------
#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

Kyle2s<-lmer(Yield~PrecZscore +TempZscore  +  (PrecZscore+TempZscore|ID),data=CrMaize8)
summary(Kyle2s) # quite similar to Kyle 1s >>good

Kyle51sw<-lmer(Yield~PrecZscore+PrecZonVar+(PrecZscore+TempZscore|ID),weights=AreaSc, data=CrMaize8) 
summary(Kyle51sw)

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
#---------------------------------------------------------------------------------------------------------------------------------------------------------------
#.....................................................................................................................................................................................

Kyle2sCI<-confint(Kyle2s) # computational problem !!!
Kyle2sCIb<-confint(Kyle2s,method = c( "boot"))
Kyle2sCIw<-confint(Kyle2s,method = c( "Wald"))

# takes very long-> save the results  
Kyle51swCI<-confint(Kyle51sw) # ALL GOOD 

Kyle51swCIb<-confint(Kyle51sw,method = c( "boot")) # ZonVar - significance a bit questionable here??
Kyle51swCIw<-confint(Kyle51sw,method = c( "Wald"))
#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
# now some of the previous models

Kyle1sCI<-confint(Kyle1s)
Kyle1swCI<-confint(Kyle1sw) #this suggests that all what in fixed effects significant. BUt I don't think so, really small p-va. also bootstrapped not significant..
Kyle2swCI<-confint(Kyle2sw) # also here too  much significance

Kyle1sCIb<-confint(Kyle1s,method = c( "boot"))  # seems to be fine- in favor of Kyle2s. Var goes through zero, the other two don't
Kyle1swCIb<-confint(Kyle1sw,method = c( "boot"))
Kyle2swCIb<-confint(Kyle2sw,method = c( "boot"))

Kyle1sCIw<-confint(Kyle1s,method = c( "Wald")) 
Kyle1swCIw<-confint(Kyle1sw,method = c( "Wald")) #okkk. just as it should be
Kyle2swCIw<-confint(Kyle2sw,method = c( "Wald")) #okkk. just as it should be

# so in general it seems that the Wald intervals are more similar to the boots than to the profiling. wouldnt trust the profiles too much

# the profiling CI seem to put everything significant. But when I try some of the models which is not for Z-score, but for 
# scaled raw data, they show the significance correctly I think. ..INteresting
#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

# ------    ------------     other testssss  ------   ------    ------    ------   ------

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
#---------------------------------------------------------------------------------------------------------------------------------------------------------------
library(afex) # probably the same way of testing as lmerT

Kyle2s<-lmer(Yield~PrecZscore +TempZscore  +  (PrecZscore+TempZscore|ID),data=CrMaize8)
summary(Kyle2s) # quite similar to Kyle 1s >>good


Kyle51sw<-lmer(Yield~PrecZscore+PrecZonVar+
                 (PrecZscore+TempZscore|ID),weights=AreaSc, data=CrMaize8) 
summary(Kyle51sw)


#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
#---------------------------------------------------------------------------------------------------------------------------------------------------------------
#.....................................................................................................................................................................................

Kyle2s_alt<-lmer_alt(Yield~PrecZscore +TempZscore  +  (PrecZscore+TempZscore|ID),data=CrMaize8)
summary(Kyle2s_alt)

Kyle2s_mixed<-mixed(Yield~PrecZscore +TempZscore  +  (PrecZscore+TempZscore|ID),data=CrMaize8)
summary(Kyle2s_mixed)

Kyle2s_mixedPB<-mixed(Yield~PrecZscore +TempZscore  +  (PrecZscore+TempZscore|ID),method = "PB",data=CrMaize8)
summary(Kyle2s_mixedPB)  #nice, it looks similar

Kyle2s_mixedLRT<-mixed(Yield~PrecZscore +TempZscore  +  (PrecZscore+TempZscore|ID),method = "LRT",data=CrMaize8)
summary(Kyle2s_mixedLRT)

#.....................................................................................................................................................................................
Kyle51sw_alt<-lmer_alt(Yield~PrecZscore+PrecZonVar+
                 (PrecZscore+TempZscore|ID),weights=AreaSc, data=CrMaize8) 
summary(Kyle51sw_alt)

Kyle51sw_mixed<-mixed(Yield~PrecZscore+PrecZonVar+
                         (PrecZscore+TempZscore|ID),weights=AreaSc, data=CrMaize8) 
summary(Kyle51sw_mixed)

Kyle51sw_mixedPB<-mixed(Yield~PrecZscore+PrecZonVar+
                        (PrecZscore+TempZscore|ID),weights=AreaSc,method = "PB", data=CrMaize8) 
summary(Kyle51sw_mixedPB)

Kyle51sw_mixedLRT<-mixed(Yield~PrecZscore+PrecZonVar+
                          (PrecZscore+TempZscore|ID),weights=AreaSc,method = "LRT", data=CrMaize8) 
summary(Kyle51sw_mixedLRT)


# scaled raw data, the they show the significance correctly I think. ..INteresting
save.image("~/foodSystems/Rcodes/Eq1ProdFun3/lmer2Inferrence.RData")