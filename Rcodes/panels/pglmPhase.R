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
summary(Phase01)


rm(Phase01)
Phase01<-read.csv( "NDMA_county_bulletins/my/All01.csv",header=TRUE)

Phase01$PhaseNum<-NA
Phase01$PhaseNum[which(Phase01$Phase=='Normal')]<-1
Phase01$PhaseNum[which(Phase01$Phase=='Alert')]<-2
Phase01$PhaseNum[which(Phase01$Phase=='Alarm')]<-3
Phase01$PhaseNum[which(Phase01$Phase=='Alarm/Alert')]<-2.5
Phase01$PhaseNum[which(Phase01$Phase=='Alert/Normal')]<-1.5
Phase01$PhaseNum[which(Phase01$Phase=='noreport')]<-NA
Phase01$PhaseNum[which(Phase01$Phase=='Recovery')]<-NA




Phase01$PhaseOrd<-NA
Phase01$PhaseOrd[which(Phase01$PhaseNum==1)]<-1
Phase01$PhaseOrd[which(Phase01$PhaseNum==2)]<-2
Phase01$PhaseOrd[which(Phase01$PhaseNum==3)]<-3
Phase01$PhaseOrd[which(Phase01$PhaseNum==2.5)]<-NA
Phase01$PhaseOrd[which(Phase01$PhaseNum==1.5)]<-NA
Phase01$PhaseOrd[which(Phase01$PhaseNum=='noreport')]<-NA
Phase01$PhaseOrd[which(Phase01$Phase=='Recovery')]<-NA


table(Phase01$PhaseNum,Phase01$PhaseOrd)







table(Phase01$PhaseNum)
table(Phase01$Phase)

rm(Phase02)
Phase02<-Phase01[which(Phase01$Year<2016),]
summary(Phase02)

Phase02$Prec<-NA

for (i in 1:nrow(Phase02))
{  
  
  if (Phase02$CountyID[i] %in% PercZ1$ID1)
    yearCurrent<-Phase02$Year[i]
  monthCurrent<-Phase02$Month[i]
  countyCurrent<-Phase02$CountyID[i] 
  
  county<-PercZ1$value[ which(PercZ1$Year== yearCurrent & PercZ1$ID1==countyCurrent & PercZ1$Month==monthCurrent  )]
  if (length(county)>0)
  { Phase02$Prec[i]<-county
 rm(county)}
}



table(Phase02$PhaseNum,Phase02$PhaseOrd)
table(Phase02$PhaseOrd)
summary(Phase02)


#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

# Now i can test my second plm estimate
summary(Phase02)
secondie<-plm(PhaseNum~Prec, Phase02, index= c("CountyID","T"))
summary(secondie)

secondie<-plm(PhaseNum~Prec, Phase02, index= c("CountyID","T"),    model = c("random"))
summary(secondie)

thirdie<-pglm(PhaseOrd~Prec, Phase02, index= c("CountyID","T"),  method = 'bfgs',  model = c("random"), family = ordinal('probit'),R = 5, print.level = 3)
summary(thirdie)

thirdie<-pglm(PhaseOrd~Prec, Phase02, index= c("CountyID","T"),   model = c("random"), family = ordinal('probit'))
summary(thirdie)
thirdie2<-pglm(PhaseOrd~Prec, Phase02, index= c("CountyID","T"),  family = ordinal('probit'))
summary(thirdie2)

