rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

load("~/foodSystems/dataFS/Main/CrMaize8.RData")
library(plm)

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

# Dale30 and Dale 40 are the models which I choose as a base for now and I will use them to go throw the specification testss...


Dale30<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"), data=CrMaize8)
summary(Dale30)

Dale40<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"),
            weights = Area, data=CrMaize8)
summary(Dale40)

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww


plmtest(Dale30)
plmtest(Dale30,effect="time")
plmtest(Dale30,effect="twoway")


plmtest(Dale30,type="kw")
plmtest(Dale30,effect="time",type="kw")
plmtest(Dale30,effect="twoway",type="kw")

plmtest(Dale40)
plmtest(Dale40,effect="time")
plmtest(Dale40,effect="twoway")

plmtest(Dale40,type="ghm")
plmtest(Dale40,effect="time",type="ghm")
plmtest(Dale40,effect="twoway",type="ghm")
#--- -  ---- -   --------    ---------   -------    - - - - - - - --- -  ----      ------- -----      -----------        ---- ---     -----------
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------

Dale31<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"), effect="twoway",data=CrMaize8)
summary(Dale31)
Dale3ols<-lm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar, data=CrMaize8)
summary(Dale3ols)

pFtest(Dale31,Dale3ols)
pFtest(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar, data=CrMaize9, effect="twoway")

Dale41<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"), weights=Area, effect="twoway",data=CrMaize8)
summary(Dale41)
Dale4ols<-lm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar, weights=Area,data=CrMaize8)
summary(Dale4ols)

pFtest(Dale41,Dale4ols)

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#testing for time fixed effect
Dale34<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar+factor(Year),index=c("ID","Year"), data=CrMaize8)
summary(Dale34)

pFtest(Dale34,Dale30)


Dale44<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar+factor(Year),index=c("ID","Year"),
            weights = Area, data=CrMaize8)
summary(Dale44)
pFtest(Dale44,Dale40)

# YES to time effects

#testing for individual fixed effect
pFtest(Dale30,Dale3ols)
pFtest(Dale40,Dale4ols)

# 
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------
# within versus random
Dale31<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"), effect="twoway",data=CrMaize8)
summary(Dale31)

Dale32<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"), effect="twoway",model="random",data=CrMaize8)
summary(Dale32)

phtest(Dale31,Dale32)
#robust version towards autocorrelation #EHM ASI NEFUNGUJE pro twoway effeck
CrMaize9<-c(CrMaize8[c(3,6, 1,2,4,5,7:23)])
            
phtest(Dale31,Dale32,data = CrMaize9,model=c("within","random"),
                   method = "aux", vcov = function(Dale31) vcovHC(Dale31))
    
phtest(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar+factor(Year),data = CrMaize9,model=c("within","random"),
       method = "aux", vcov = function(x) vcovHC(x)) 
# NEPANIKARIT, JE TO ONE WAY EFFECT (PRO KTERY SEM VEDELA ZE H0 ZAMITNUTA UZ PODLE NE-ROBUST.TWOWAY EFFECT ASI NEJDE)
#-----------------------   -------------------
#just in case one way

Dale311<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"), effect="time",data=CrMaize8)
summary(Dale311)

Dale322<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"), effect="time",model="random",data=CrMaize8)
summary(Dale322)

phtest(Dale311,Dale322)

Dale33<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"),model="random",data=CrMaize8)
summary(Dale33)
phtest(Dale30,Dale33)
summary(Dale30)
#-----------------------   -------------------
# Between? does not extend to twoway, unfortunatelly.
Dale35<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"),model="between",data=CrMaize8)
summary(Dale35)
phtest(Dale35,Dale30)
phtest(Dale35,Dale33)

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------
Dale41<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"), weights=Area, effect="twoway",data=CrMaize8)
summary(Dale41)

Dale42<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"), weights=Area, effect="twoway",model="random",data=CrMaize8)
summary(Dale42)

phtest(Dale41,Dale42)

#--------------------------------------------------------------------------------------------------------------------------------------

Dale43<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"),weights=Area,model="random",data=CrMaize8)
summary(Dale43)
phtest(Dale40,Dale43)
summary(Dale40)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# if Dale 40 with random effects, do I need twoway??
Dale424<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"), weights=Area, model="random",data=CrMaize8)
summary(Dale424)

pFtest(Dale42,Dale424)

########################################################################################################################################################################################
#the bests are:

#Dale30 or 32 or 42 or 40
Dale3poo<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"), model="pooling",data=CrMaize8)
pwtest(Dale3poo)
pbltest(Dale32)
pbsytest(Dale3poo)
pbsytest(Dale3poo,test="j")
pbsytest(Dale3poo,test="re")
pbgtest(Dale32)

Dale4poo<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar, model="pooling",index=c("ID","Year"),
            weights = Area, data=CrMaize8)
pwtest(Dale4poo)
pbltest(Dale3poo)
pbsytest(Dale4poo)

pbgtest(Dale42)

#it seems there is serial correlation>> use robust vcov matrix??
vcovHC(Dale40)
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#pwartest = specification test between first difference and fixed effects

Dale32fd<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"),model="fd",data=CrMaize8)
summary(Dale32fd)

pwfdtest(Dale32fd,h0="fe")
pwfdtest(Dale32fd,h0="fd")
########################################################################################################################################################################################
#the bests are:

#Dale30 or 32 or 42 or 40

#40 stops being significant if autocorrelation!!!

Dale400<-pvcm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"),weights=Area, model='within', data=CrMaize8)
summary(Dale400)


Dale401<-pvcm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"),weights=Area, model='random', data=CrMaize8)
summary(Dale401)  #autocorrelation???

#SPATIAL CORRELATION - NOOOOOOOOOOO . BUT ESTIMATES STILL CAN BE CONSISTENT, JUST INEFFICIENT
pcdtest(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,data=CrMaize9)
pcdtest(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,data=CrMaize9,effect="individual")

pcdtest(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,data=CrMaize9,model="random",effect="twoways")
pcdtest(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,data=CrMaize9,model="random",effect="twoways")
pcdtest(Area~PrecZones+PrecZonVar+TempZones+TempZonVar,data=CrMaize9,model="random",effect="twoways")
pcdtest(MT~PrecZones+PrecZonVar+TempZones+TempZonVar,data=CrMaize9,model="random",effect="twoways")