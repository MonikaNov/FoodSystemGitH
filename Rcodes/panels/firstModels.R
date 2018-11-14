rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/sussex/US/paper3") # doma

setwd(WDuni)
setwd(WDhome)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(plm)
library(pglm)
library(reshape)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
PercZ<-read.csv( "CountyClimateM/pre_zscore_kenya_adm2_1999-2015.10thperc.csv",header=TRUE)

PercZ1<-melt(PercZ, id.vars=c(1,2,3,4))
names(PercZ1)[5]<-'MonthYear'
PercZ1$Month<-as.numeric(substr(PercZ1$MonthYear,2,3))
PercZ1$Year<-as.numeric(substr(PercZ1$MonthYear,5,8))
summary(PercZ1)


rm(Phase04)
Phase04<-read.csv( "NDMA_county_bulletins/my/All04.csv",header=TRUE)
summary(Phase04)

Phase04$PhaseNum<-NA
Phase04$PhaseNum[which(Phase04$Phase=='Normal')]<-1
Phase04$PhaseNum[which(Phase04$Phase=='Alert')]<-2
Phase04$PhaseNum[which(Phase04$Phase=='Alarm')]<-3
Phase04$PhaseNum[which(Phase04$Phase=='Alarm/Alert')]<-2.5
Phase04$PhaseNum[which(Phase04$Phase=='Alert/Normal')]<-1.5
Phase04$PhaseNum[which(Phase04$Phase=='noreport')]<-NA
Phase04$PhaseNum[which(Phase04$Phase=='Recovery')]<-NA




Phase04$PhaseOrd<-NA
Phase04$PhaseOrd[which(Phase04$PhaseNum==1)]<-1
Phase04$PhaseOrd[which(Phase04$PhaseNum==2)]<-2
Phase04$PhaseOrd[which(Phase04$PhaseNum==3)]<-3
Phase04$PhaseOrd[which(Phase04$PhaseNum==2.5)]<-NA
Phase04$PhaseOrd[which(Phase04$PhaseNum==1.5)]<-NA
Phase04$PhaseOrd[which(Phase04$PhaseNum=='noreport')]<-NA
Phase04$PhaseOrd[which(Phase04$Phase=='Recovery')]<-NA


table(Phase04$PhaseNum,Phase04$PhaseOrd)







table(Phase04$PhaseNum)
table(Phase04$Phase)

rm(Phase05)
Phase05<-Phase04[which(Phase04$Year<2016),]
summary(Phase05)

Phase05$Prec<-NA

for (i in 1:nrow(Phase05))
{  
  
  if (Phase05$CountyID[i] %in% PercZ1$ID1)
    yearCurrent<-Phase05$Year[i]
  monthCurrent<-Phase05$Month[i]
  countyCurrent<-Phase05$CountyID[i] 
  
  county<-PercZ1$value[ which(PercZ1$Year== yearCurrent & PercZ1$ID1==countyCurrent & PercZ1$Month==monthCurrent  )]
  if (length(county)>0)
  { Phase05$Prec[i]<-county
 rm(county)}
}



table(Phase05$Phase,Phase05$PhaseOrd)
table(Phase05$PhaseOrd)
summary(Phase05)


#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

# Now i can test my second plm estimate
summary(Phase05)
secondie<-plm(PhaseNum~Prec, Phase05, index= c("CountyID","T"))
summary(secondie)
summary(Phase05)
secondie<-plm(PhaseNum~Prec, Phase05, index= c("CountyID","T"),model='pooling')
summary(secondie)


secondieVC<-pvcm(PhaseNum~Prec, Phase05, index= c("CountyID","T"),    model = c("within"))
summary(secondieVC)

pooltest(secondie,secondieVC)

secondie<-lm(PhaseNum~Prec, Phase05)
summary(secondie)



thirdie<-pglm(PhaseOrd~Prec, Phase05, index= c("CountyID","T"),  method = 'bfgs',  model = c("random"), family = ordinal('probit'),R = 5, print.level = 3)
summary(thirdie)

thirdie<-pglm(PhaseOrd~Prec, Phase05, index= c("CountyID","T"),   model = c("random"), family = ordinal('probit'))
summary(thirdie)
thirdie2<-pglm(PhaseOrd~Prec, Phase05, index= c("CountyID","T"),  family = ordinal('probit'))
summary(thirdie2)

