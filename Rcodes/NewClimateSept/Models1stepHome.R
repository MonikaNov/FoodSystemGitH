rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

# setwd(WDuni)
setwd(WDhome)

library('dplyr')
library('tseries')
library(plm)
library(lme4)
library(lattice)
library(car)
library(lmerTest)
library(optimx)

load("Main/data.RData")
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

laggy0<-lm(Yield ~ I(SeasRain_OND_L1*(1-west1))+I(SeasRain_MAM*(1-west1)) +I(SeasRain_MAM*(west1)) 
           +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1),data=dataScTS )
summary(laggy0)     

attr(terms(laggy0),"term.label")



#--------------------------------------------------------------------------------------------------------------------
n<-names(dataScTS)[c(6,7:20,22:29,30:31,65:70,72:79,81:89)]
xx<-paste(n, collapse = " + ")


f <- as.formula(   paste("Yield ~", xx ,"+(1+I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1)|ID1)"   )      )   
luggy<-lmer(f,data=dataScTS )
summary(luggy)  # interesting. quite nice, actually


# but I have to do a different one:

f <- as.formula(   paste("Yield ~ I(SeasRain_OND_L1*(1-west1)) +I(SeasRain_MAM*(west1)) +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1)+",
                         xx ,"+(1+I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1)|ID1)"   )      )   
luggy<-lmer(f,data=dataScTS )
nobs(luggy)

f <- as.formula(   paste("Yield ~ I(SeasRain_OND_L1*(1-west1)) +I(SeasRain_MAM*(west1)) +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1)+
                         MaxTemp_OctMar + AvgTemp_OctMar + CumDD_OctMar + HeatWDays_OctMar + SDtemp_OctMar + MaxTemp_MarSep + AvgTemp_MarSep +
                         CumDD_MarSep + HeatWDays_MarSep + SDtemp_MarSep + PrecCoefVar_OND + DrSpell10_OND + DrSpell20_OND + DrySpell_OND +
                         MaxRain_OND + PrecStDev_OND + Prec2Months_OND + PrecCoefVar_MAM + DrSpell10_MAM + DrSpell20_MAM + DrySpell_MAM + 
                         MaxRain_MAM + SeasRain_MAM + PrecStDev_MAM + Prec2Months_MAM + MaxTemp_OctMar_L1 + AvgTemp_OctMar_L1 + CumDD_OctMar_L1 + 
                         HeatWDays_OctMar_L1 + SDtemp_OctMar_L1 + MaxTemp_MarSep_L1 + CumDD_MarSep_L1 + HeatWDays_MarSep_L1 + SDtemp_MarSep_L1 + 
                         PrecCoefVar_OND_L1 + DrSpell10_OND_L1 + DrSpell20_OND_L1 + DrySpell_OND_L1 + MaxRain_OND_L1 + PrecStDev_OND_L1 +
                         Prec2Months_OND_L1 + PrecCoefVar_MAM_L1 + DrSpell10_MAM_L1 + DrSpell20_MAM_L1 + DrySpell_MAM_L1 + MaxRain_MAM_L1 +
                         SeasRain_MAM_L1 + PrecStDev_MAM_L1+(1+I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1)|ID1)"   )      )   
luggyS<-lmer(f,data=dataScTS )
summary(luggyS) 
nobs(luggyS) 

frame<-model.frame(luggyS,drop.unused.levels = TRUE)
aa=dataScTS[rownames(frame ),]

nobs(luggyS2) 

bb<-dataScTS[complete.cases(dataScTS[,c("Yield","SeasRain_OND_L1","west1","SeasRain_MAM","AvgTemp_OctMar","AvgTemp_MarSep_L1",
  "MaxTemp_OctMar","AvgTemp_OctMar","CumDD_OctMar","HeatWDays_OctMar","SDtemp_OctMar","MaxTemp_MarSep","AvgTemp_MarSep",
"CumDD_MarSep","HeatWDays_MarSep","SDtemp_MarSep","PrecCoefVar_OND","DrSpell10_OND","DrSpell20_OND","DrySpell_OND",
"MaxRain_OND","PrecStDev_OND","Prec2Months_OND","PrecCoefVar_MAM","DrSpell10_MAM","DrSpell20_MAM","DrySpell_MAM",
"MaxRain_MAM","SeasRain_MAM","PrecStDev_MAM","Prec2Months_MAM","MaxTemp_OctMar_L1","AvgTemp_OctMar_L1","CumDD_OctMar_L1",
"HeatWDays_OctMar_L1","SDtemp_OctMar_L1","MaxTemp_MarSep_L1","CumDD_MarSep_L1","HeatWDays_MarSep_L1","SDtemp_MarSep_L1",
"PrecCoefVar_OND_L1","DrSpell10_OND_L1","DrSpell20_OND_L1","DrySpell_OND_L1","MaxRain_OND_L1","PrecStDev_OND_L1",
"Prec2Months_OND_L1","PrecCoefVar_MAM_L1","DrSpell10_MAM_L1","DrSpell20_MAM_L1","DrySpell_MAM_L1","MaxRain_MAM_L1",
"SeasRain_MAM_L1","PrecStDev_MAM_L1","ID1")]),]


luggyS2<-lmer(Yield ~ I(SeasRain_OND_L1*(1-west1)) +I(SeasRain_MAM*(west1)) +I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1)+
                MaxTemp_OctMar + AvgTemp_OctMar + CumDD_OctMar + HeatWDays_OctMar +HeatWDays_MarSep_L1 + SDtemp_MarSep_L1 + 
                PrecCoefVar_OND_L1 + DrSpell10_OND_L1 + DrSpell20_OND_L1 + DrySpell_OND_L1 + MaxRain_OND_L1 + PrecStDev_OND_L1 +
                Prec2Months_OND_L1 + PrecCoefVar_MAM_L1 + DrSpell10_MAM_L1 + DrSpell20_MAM_L1 + DrySpell_MAM_L1 + MaxRain_MAM_L1 +
                SeasRain_MAM_L1 + PrecStDev_MAM_L1+(1+I(AvgTemp_OctMar*(1-west1))+I(AvgTemp_MarSep_L1*west1)|ID1),data=bb)
summary(luggyS2) 
attr(terms(luggyS),"term.label")
rm(luggyStep)
luggyStep<-step(luggyS2,keep=c("I(SeasRain_OND_L1 * (1 - west1))", "I(SeasRain_MAM * (west1))", "I(AvgTemp_OctMar * (1 - west1))" ,"I(AvgTemp_MarSep_L1 * west1)" ))
summary(get_model(luggyStep))
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
save.image("~/foodSystems/Rcodes/lag/Models1stepHome.RData")