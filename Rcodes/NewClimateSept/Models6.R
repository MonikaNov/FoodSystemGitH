rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDuni2<-"\\\\its-home.uscs.susx.ac.uk/home/mn301/foodSystems/dataFS"
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma


setwd(WDuni)
setwd(WDuni2)
setwd(WDhome)

library('dplyr')
library('tseries')
library(plm)
library(lme4)
library(lattice)
library(car)
library(lmerTest)
library(optimx)

source("/its/home/mn301/foodSystems/Rcodes/NewClimateSept/Models3GettingFrames.R")
source("\\\\its-home.uscs.susx.ac.uk/home/mn301/foodSystems/Rcodes/NewClimateSept/Models3GettingFrames.R")

load("~/foodSystems/Rcodes/NewClimateSept/Models6.RData")
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo


# unfortunatelly, I have to write the models as follows (foob is the same as foobar) as there is a bug in the Step function...:
n<-names(baseP)
xx<-paste(n[!n %in% c("Yield","ID1","ADM2_NAME","Year","ASAL","Area","MT","west1") ], collapse = " + ")

f <- as.formula(   paste("Yield ~", xx ,"+(1+",  xx,"|ID1)"   )      )   
baz<-lmer(f,data=baseP )
summary(baz)
#----------------------------------------------------------
#now scaled:

bazSc<-lmer(f,data=basePsc )
summary(bazSc)


foobarStep<-step(bazSc)
summary(get_model(foobarStep))  # very good !!!

#now log:
f <- as.formula(   paste("log(Yield) ~", xx ,"+(1+",  xx,"|ID1)"   )      )   
bazLn<-lmer(f,data=baseP)
summary(bazLn)
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# ok, based on the results of the step, I am going to adjust the models here.

bazSc0<-lmer(Yield ~ MaxRain_OND + Prec2Months_OND + DrSpell10_MAM + DrSpell20_MAM + 
               MaxRain_MAM + SeasRain_MAM + Prec2Months_MAM + (DrSpell10_OND + DrySpell_OND + DrySpell_MAM | ID1),data=basePsc )
summary(bazSc0)
nobs(bazSc0)


bazSc2<-lmer(Yield ~ MaxRain_OND + Prec2Months_OND + DrSpell10_MAM + DrSpell20_MAM + 
               MaxRain_MAM + SeasRain_MAM + Prec2Months_MAM +SeasRain_OND+ (DrSpell10_OND + DrySpell_OND + DrySpell_MAM +SeasRain_OND| ID1),data=MaizeClimateScTS )
summary(bazSc2)
nobs(bazSc2)


bazSc3<-lmer(Yield ~ MaxRain_OND + Prec2Months_OND + DrSpell10_MAM + DrSpell20_MAM + 
           MaxRain_MAM + SeasRain_MAM + Prec2Months_MAM +SeasRain_OND+ MaxTemp_OctMar+MaxTemp_MarSep+ 
             (DrSpell10_OND + DrySpell_OND + DrySpell_MAM +SeasRain_OND+MaxTemp_OctMar+MaxTemp_MarSep| ID1),data=MaizeClimateScTS)

summary(bazSc3)
nobs(bazSc3)

bazSc31<-lmer(Yield ~ MaxRain_OND + Prec2Months_OND + DrSpell10_MAM + DrSpell20_MAM + 
               MaxRain_MAM + SeasRain_MAM + Prec2Months_MAM +SeasRain_OND+ I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep*west1)+ 
               (DrSpell10_OND + DrySpell_OND + DrySpell_MAM +SeasRain_OND| ID1),data=MaizeClimateScTS)

summary(bazSc31)  # bad, not many observations

bazSc32<-lmer(Yield ~ MaxRain_OND + Prec2Months_OND + DrSpell10_MAM + DrSpell20_MAM + 
                MaxRain_MAM + SeasRain_MAM + Prec2Months_MAM +SeasRain_OND+ I(MaxTemp_OctMar*(ASAL))+I(MaxTemp_MarSep*(1-ASAL))+ 
                (DrSpell10_OND + DrySpell_OND + DrySpell_MAM +SeasRain_OND| ID1),data=MaizeClimateScTS)

summary(bazSc32)  


#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# ok, lets try all the temperatture vars.

bazSc4<-lmer(Yield ~ MaxRain_OND + Prec2Months_OND + DrSpell10_MAM + DrSpell20_MAM + 
                MaxRain_MAM + SeasRain_MAM + Prec2Months_MAM +SeasRain_OND+ I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep*west1)+ 
               I(CumDD_OctMar*(1-west1))+I(CumDD_MarSep*west1)+    I(HeatWDays_OctMar*(1-west1))+I(HeatWDays_MarSep*west1)+  
                I(MaxTemp_OctMar*(1-west1))+I(MaxTemp_MarSep*west1)+
               (DrSpell10_OND + DrySpell_OND + DrySpell_MAM +SeasRain_OND+ I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep*west1)+ 
                  I(CumDD_OctMar*(1-west1))+I(CumDD_MarSep*west1)+    I(HeatWDays_OctMar*(1-west1))+I(HeatWDays_MarSep*west1)+  
                  I(MaxTemp_OctMar*(1-west1))+I(MaxTemp_MarSep*west1)| ID1),data=MaizeClimateScTS)

summary(bazSc4)  


bazSc41<-lmer(Yield ~ Prec2Months_OND +     MaxRain_MAM + SeasRain_MAM + Prec2Months_MAM +SeasRain_OND+ I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep*west1)+ 
               I(CumDD_OctMar*(1-west1))+I(CumDD_MarSep*west1)+    I(HeatWDays_OctMar*(1-west1))+I(HeatWDays_MarSep*west1)+  
               I(MaxTemp_OctMar*(1-west1))+I(MaxTemp_MarSep*west1)+( DrSpell10_OND + DrySpell_OND  +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep*west1)+ 
                  I(CumDD_OctMar*(1-west1))+I(CumDD_MarSep*west1)+    I(HeatWDays_OctMar*(1-west1))+I(HeatWDays_MarSep*west1)+  
                  I(MaxTemp_OctMar*(1-west1))+I(MaxTemp_MarSep*west1)| ID1),data=MaizeClimateScTS)

summary(bazSc41)  

bazSc42<-lmer(Yield ~ Prec2Months_OND +     MaxRain_MAM + SeasRain_MAM + Prec2Months_MAM +SeasRain_OND+ I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep*west1)+ 
                I(CumDD_OctMar*(1-west1))+I(CumDD_MarSep*west1)+ 
                I(MaxTemp_OctMar*(1-west1))+I(MaxTemp_MarSep*west1)+( DrSpell10_OND + DrySpell_OND  +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep*west1)+ 
                     I(CumDD_OctMar*(1-west1))+I(CumDD_MarSep*west1)| ID1),data=MaizeClimateScTS)

summary(bazSc42)  


bazSc43<-lmer(log(Yield) ~ Prec2Months_OND +     MaxRain_MAM + SeasRain_MAM + Prec2Months_MAM +SeasRain_OND+ I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep*west1)+ 
                I(CumDD_OctMar*(1-west1))+I(CumDD_MarSep*west1)+ 
                I(MaxTemp_OctMar*(1-west1))+I(MaxTemp_MarSep*west1)+( DrSpell10_OND + DrySpell_OND  +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep*west1)+ 
                                                                        I(CumDD_OctMar*(1-west1))+I(CumDD_MarSep*west1)| ID1),data=MaizeClimateScTS)

summary(bazSc43)  


#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
basicME<-lmer(Yield ~ SeasRain_MAM +SeasRain_OND+ I(AvgTemp_OctMar*(ASAL))+I(AvgTemp_MarSep*(1-ASAL))+ 
                (SeasRain_MAM +SeasRain_OND+ I(AvgTemp_OctMar*(ASAL))+I(AvgTemp_MarSep*(1-ASAL))| ID1),data=MaizeClimateScTS)

summary(basicME) 

basicME<-lmer(Yield ~ SeasRain_MAM +SeasRain_OND+ AvgTemp_OctMar+AvgTemp_MarSep+ 
                (SeasRain_MAM +SeasRain_OND+ AvgTemp_OctMar+AvgTemp_MarSep| ID1),data=MaizeClimateScTS)

summary(basicME) # proste divny tady, ZNAMINKA VYCHAZI SPATNE...
nobs(basicME)



basicME<-lmer(log(Yield) ~ SeasRain_MAM +SeasRain_OND+ I(AvgTemp_OctMar*(ASAL))+I(AvgTemp_MarSep*(1-ASAL))+ 
                (SeasRain_MAM +SeasRain_OND+ I(AvgTemp_OctMar*(ASAL))+I(AvgTemp_MarSep*(1-ASAL))| ID1),data=MaizeClimateScTS)

summary(basicME) 




#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

save.image("~/foodSystems/Rcodes/NewClimateSept/Models6.RData")
save.image("\\\\its-home.uscs.susx.ac.uk/home/mn301/foodSystems/Rcodes/NewClimateSept/Models6.RData")