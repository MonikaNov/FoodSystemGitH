rm(list=ls())

library('dplyr')
library('purrr')
load("dataFS/Main/DaTS.RData")
library(dplyr); library(tseries); library(plm); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)
testSc<-ScaledTS;
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

# "AvgTemp" "SDTemp"  "DDays"    "HWDays"   "MaxT" "SeasPr"   "Prec2m"   "SDPrec"   "CVPrec"               
# "Spell"   "Spell4" "Spell10"  "Spell20" "MaxP" "cum90""cum95"   "cum99"  "days90"  "days95"  "days99"   "PrecFirstM"           



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

DaTS$AvgTemp_ond[DaTS$ASAL==TRUE]<- DaTS$AvgTemp_OctDec_L1[DaTS$ASAL==TRUE]
DaTS$SDTemp_ond[DaTS$ASAL==TRUE]<-DaTS$SDTemp_OctDec_L1[DaTS$ASAL==TRUE]
DaTS$DDays_ond[DaTS$ASAL==TRUE]<-DaTS$Cum_DD_OctDec_L1[DaTS$ASAL==TRUE]
DaTS$HWDays_ond[DaTS$ASAL==TRUE]<-DaTS$HeatWDays_OctDec_L1[DaTS$ASAL==TRUE]
DaTS$MaxT_ond[DaTS$ASAL==TRUE]<-DaTS$MaxTemp_OctDec_L1[DaTS$ASAL==TRUE]

DaTS$SeasPr_ond[DaTS$ASAL==TRUE]<-DaTS$SeasRain_OctDec_L1[DaTS$ASAL==TRUE]
DaTS$Prec2m_ond[DaTS$ASAL==TRUE]<-DaTS$Prec2Months_OctDec_L1[DaTS$ASAL==TRUE]
DaTS$SDPrec_ond[DaTS$ASAL==TRUE]<-DaTS$PrecStDev_OctDec_L1[DaTS$ASAL==TRUE]
DaTS$CVPrec_ond[DaTS$ASAL==TRUE]<-DaTS$PrecCoefVar_OctDec_L1[DaTS$ASAL==TRUE]
DaTS$Spell_ond[DaTS$ASAL==TRUE]<-DaTS$DrySpell_OctDec_L1[DaTS$ASAL==TRUE]
DaTS$Spell4_ond[DaTS$ASAL==TRUE]<-DaTS$DrySpell4_OND_L1[DaTS$ASAL==TRUE]
DaTS$Spell10_ond[DaTS$ASAL==TRUE]<-DaTS$DrSpell10_OctDec_L1[DaTS$ASAL==TRUE]
DaTS$Spell20_ond[DaTS$ASAL==TRUE]<-DaTS$DrSpell20_OctDec_L1[DaTS$ASAL==TRUE]
DaTS$MaxP_ond[DaTS$ASAL==TRUE]<-DaTS$MaxRain_OctDec_L1[DaTS$ASAL==TRUE]
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
DaTS$AvgTemp_ond[DaTS$ASAL==FALSE]<-DaTS$AvgTemp_MarAug[DaTS$ASAL==FALSE]
DaTS$SDTemp_ond[DaTS$ASAL==FALSE]<-DaTS$SDTemp_MarAug[DaTS$ASAL==FALSE]
DaTS$DDays_ond[DaTS$ASAL==FALSE]<-DaTS$Cum_DD_MarAug[DaTS$ASAL==FALSE]
DaTS$HWDays_ond[DaTS$ASAL==FALSE]<-DaTS$HeatWDays_MarAug[DaTS$ASAL==FALSE]
DaTS$MaxT_ond[DaTS$ASAL==FALSE]<-DaTS$MaxTemp_MarAug[DaTS$ASAL==FALSE]

DaTS$SeasPr_ond[DaTS$ASAL==FALSE]<-DaTS$SeasRain_MarAug[DaTS$ASAL==FALSE]
DaTS$Prec2m_ond[DaTS$ASAL==FALSE]<-DaTS$Prec2Months_MarAug[DaTS$ASAL==FALSE]
DaTS$SDPrec_ond[DaTS$ASAL==FALSE]<-DaTS$PrecStDev_MarAug[DaTS$ASAL==FALSE]
DaTS$CVPrec_ond[DaTS$ASAL==FALSE]<-DaTS$PrecCoefVar_MarAug[DaTS$ASAL==FALSE]
DaTS$Spell_ond[DaTS$ASAL==FALSE]<-DaTS$DrySpell_MarAug[DaTS$ASAL==FALSE]
DaTS$Spell4_ond[DaTS$ASAL==FALSE]<-DaTS$DrySpell4_MAMJJA[DaTS$ASAL==FALSE]
DaTS$Spell10_ond[DaTS$ASAL==FALSE]<-DaTS$DrSpell10_MarAug[DaTS$ASAL==FALSE]
DaTS$Spell20_ond[DaTS$ASAL==FALSE]<-DaTS$DrSpell20_MarAug[DaTS$ASAL==FALSE]
DaTS$MaxP_ond[DaTS$ASAL==FALSE]<-DaTS$MaxRain_MarAug[DaTS$ASAL==FALSE]


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

DaTS$AvgTemp_mam[DaTS$ASAL==TRUE]<- DaTS$AvgTemp_MarMay[DaTS$ASAL==TRUE]
DaTS$SDTemp_mam[DaTS$ASAL==TRUE]<-DaTS$SDTemp_MarMay[DaTS$ASAL==TRUE]
DaTS$DDays_mam[DaTS$ASAL==TRUE]<-DaTS$Cum_DD_MarMay[DaTS$ASAL==TRUE]
DaTS$HWDays_mam[DaTS$ASAL==TRUE]<-DaTS$HeatWDays_MarMay[DaTS$ASAL==TRUE]
DaTS$MaxT_mam[DaTS$ASAL==TRUE]<-DaTS$MaxTemp_MarMay[DaTS$ASAL==TRUE]

DaTS$SeasPr_mam[DaTS$ASAL==TRUE]<-DaTS$SeasRain_MarMay[DaTS$ASAL==TRUE]
DaTS$Prec2m_mam[DaTS$ASAL==TRUE]<-DaTS$Prec2Months_MarMay[DaTS$ASAL==TRUE]
DaTS$SDPrec_mam[DaTS$ASAL==TRUE]<-DaTS$PrecStDev_MarMay[DaTS$ASAL==TRUE]
DaTS$CVPrec_mam[DaTS$ASAL==TRUE]<-DaTS$PrecCoefVar_MarMay[DaTS$ASAL==TRUE]
DaTS$Spell_mam[DaTS$ASAL==TRUE]<-DaTS$DrySpell_MarMay[DaTS$ASAL==TRUE]
DaTS$Spell4_mam[DaTS$ASAL==TRUE]<-DaTS$DrySpell4_MAM[DaTS$ASAL==TRUE]
DaTS$Spell10_mam[DaTS$ASAL==TRUE]<-DaTS$DrSpell10_MarMay[DaTS$ASAL==TRUE]
DaTS$Spell20_mam[DaTS$ASAL==TRUE]<-DaTS$DrSpell20_MarMay[DaTS$ASAL==TRUE]
DaTS$MaxP_mam[DaTS$ASAL==TRUE]<-DaTS$MaxRain_MarMay[DaTS$ASAL==TRUE]
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
DaTS$AvgTemp_mam[DaTS$ASAL==FALSE]<-DaTS$AvgTemp_MarAug[DaTS$ASAL==FALSE]
DaTS$SDTemp_mam[DaTS$ASAL==FALSE]<-DaTS$SDTemp_MarAug[DaTS$ASAL==FALSE]
DaTS$DDays_mam[DaTS$ASAL==FALSE]<-DaTS$Cum_DD_MarAug[DaTS$ASAL==FALSE]
DaTS$HWDays_mam[DaTS$ASAL==FALSE]<-DaTS$HeatWDays_MarAug[DaTS$ASAL==FALSE]
DaTS$MaxT_mam[DaTS$ASAL==FALSE]<-DaTS$MaxTemp_MarAug[DaTS$ASAL==FALSE]

DaTS$SeasPr_mam[DaTS$ASAL==FALSE]<-DaTS$SeasRain_MarAug[DaTS$ASAL==FALSE]
DaTS$Prec2m_mam[DaTS$ASAL==FALSE]<-DaTS$Prec2Months_MarAug[DaTS$ASAL==FALSE]
DaTS$SDPrec_mam[DaTS$ASAL==FALSE]<-DaTS$PrecStDev_MarAug[DaTS$ASAL==FALSE]
DaTS$CVPrec_mam[DaTS$ASAL==FALSE]<-DaTS$PrecCoefVar_MarAug[DaTS$ASAL==FALSE]
DaTS$Spell_mam[DaTS$ASAL==FALSE]<-DaTS$DrySpell_MarAug[DaTS$ASAL==FALSE]
DaTS$Spell4_mam[DaTS$ASAL==FALSE]<-DaTS$DrySpell4_MAMJJA[DaTS$ASAL==FALSE]
DaTS$Spell10_mam[DaTS$ASAL==FALSE]<-DaTS$DrSpell10_MarAug[DaTS$ASAL==FALSE]
DaTS$Spell20_mam[DaTS$ASAL==FALSE]<-DaTS$DrSpell20_MarAug[DaTS$ASAL==FALSE]
DaTS$MaxP_mam[DaTS$ASAL==FALSE]<-DaTS$MaxRain_MarAug[DaTS$ASAL==FALSE]
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
DaTS[(DaTS$Year==1981 & DaTS$ASAL==TRUE), 85:187]<-NA
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# now I need scale them..
rm(ScaledTS)
ScaledTS<-DaTS
ScaledTS[,c(5:76,81:83,85:187)]<-scale(DaTS[,c(5:76,81:83,85:187)])
ScaledTS$Yield0<-DaTS$Yield

# ~~~~~  ~~  ~~ ~~ ~~ ~~ ~   and check if scaling correctly:  ~~~~~  ~~  ~~ ~~ ~~ ~~ ~

all.equal( names(DaTS)[c(5:76,81:83,85:187)], names(ScaledTS)[c(5:76,81:83,85:187)])
all.equal(ScaledTS[4:160],testSc[4:160],check.attributes=FALSE)
all.equal(ScaledTS[4:160],testSc[4:160],check.attributes=TRUE)


mapply(cor.test,ScaledTS[,c(5:76,81:83,85:187)],DaTS[,c(5:76,81:83,85:187)] )
cor.test(ScaledTS$Spell,DaTS$Spell) ; plot(ScaledTS$Spell,DaTS$Spell) 
cor.test(ScaledTS$DrySpell4_Sep_L1,DaTS$DrySpell4_Sep_L1) ; plot(ScaledTS$DrySpell4_Sep_L1,DaTS$DrySpell4_Sep_L1)
cor.test(ScaledTS$HeatWDays_Sep,DaTS$HeatWDays_Sep); plot(ScaledTS$HeatWDays_Sep,DaTS$HeatWDays_Sep)
cor.test(ScaledTS$PrecFirstM,DaTS$Prec2m);plot(ScaledTS$PrecFirstM,DaTS$Prec2m)
cor.test(ScaledTS$days95,DaTS$days95);plot(ScaledTS$days95,DaTS$days95)
cor.test(ScaledTS$MaxT_mam,DaTS$MaxT_mam);plot(ScaledTS$MaxT_mam,DaTS$MaxT_mam)
cor.test(ScaledTS$AvgTemp_ond,DaTS$AvgTemp_ond);plot(ScaledTS$AvgTemp_ond,DaTS$AvgTemp_ond)
summary(ScaledTS)
lapply(ScaledTS[,c(5:76,81:83,85:187)],function(x) mean(x,na.rm=TRUE))
summary(sapply(ScaledTS[,c(5:76,81:83,85:187)],function(x) mean(x,na.rm=TRUE)))
plot(sapply(ScaledTS[,c(5:76,81:83,85:187)],function(x) mean(x,na.rm=TRUE)))


sum(complete.cases(ScaledTS)); sum(complete.cases(ScaledTS))
sum(complete.cases(ScaledTS))
sum(!complete.cases(ScaledTS))

sum(  !complete.cases(ScaledTS) &is.na(ScaledTS$Yield)==FALSE )
which(  !complete.cases(ScaledTS) &is.na(ScaledTS$Yield)==FALSE ) # PERFECT

sum(  !complete.cases(ScaledTS) &is.na(ScaledTS$Yield)==TRUE )

all.equal(ScaledTS[,-c(5:76,81:83,85:188)],DaTS[,-c(5:76,81:83,85:187)] ) # GROOT
all.equal(ScaledTS$Yield0,DaTS$Yield)
plot(ScaledTS$Yield0,DaTS$Yield)

# seems very well here..

