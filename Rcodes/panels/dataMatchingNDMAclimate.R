rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(plm)
library(pglm)
library(reshape)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

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

rm(Phase05)
Phase05<-Phase04[which(Phase04$Year<2016),]

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# new vars
rm(Phase06)
Phase06<-Phase05

Phase06$Prec<-NA
Phase06$PrecZ<-NA
Phase06$SPEI3<-NA
Phase06$SPEI10<-NA
Phase06$SPEI12<-NA
Phase06$SPEI18<-NA
Phase06$Tmx<-NA
Phase06$Tmxz<-NA
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

#         load all climate data

Prec<-read.csv( "CountyClimateM/pre_kenya_adm2_1999-2015.10thperc.csv",header=TRUE)
PrecZ<-read.csv( "CountyClimateM/pre_zscore_kenya_adm2_1999-2015.10thperc.csv",header=TRUE)

SPEI3<-read.csv( "CountyClimateM/spei3_kenya_adm2_1999-2015.10thperc.csv",header=TRUE)
SPEI10<-read.csv( "CountyClimateM/spei10_kenya_adm2_1999-2015.10thperc.csv",header=TRUE)
SPEI12<-read.csv( "CountyClimateM/spei12_kenya_adm2_1999-2015.10thperc.csv",header=TRUE)
SPEI18<-read.csv( "CountyClimateM/spei18_kenya_adm2_1999-2015.10thperc.csv",header=TRUE)
Tmx<-read.csv( "CountyClimateM/tmx_kenya_adm2_1999-2015.90thperc.csv",header=TRUE)
TmxZ<-read.csv( "CountyClimateM/tmx_zscore_kenya_adm2_1999-2015.90thperc.csv",header=TRUE)

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#now match the data with Phase data

j<-11
for (ClimMeasure in list(Prec,PrecZ,SPEI3,SPEI10,SPEI12,SPEI18,Tmx,TmxZ))
{
  ClimMeasure2<-melt(ClimMeasure, id.vars=c(1,2,3,4))
  names(ClimMeasure2)[5]<-'MonthYear'
  ClimMeasure2$Month<-as.numeric(substr(ClimMeasure2$MonthYear,2,3))
  ClimMeasure2$Year<-as.numeric(substr(ClimMeasure2$MonthYear,5,8))
  print(summary(ClimMeasure2))
  
  
  
  for (i in 1:nrow(Phase06))
  {  
    
    if (Phase06$CountyID[i] %in% ClimMeasure2$ID1)
      yearCurrent<-Phase06$Year[i]
    monthCurrent<-Phase06$Month[i]
    countyCurrent<-Phase06$CountyID[i] 
    
    county<-ClimMeasure2$value[ which(ClimMeasure2$Year== yearCurrent & ClimMeasure2$ID1==countyCurrent & ClimMeasure2$Month==monthCurrent  )]
    if (length(county)>0)
    { Phase06[i,j]<-county
    rm(county)}
    
  }  
  j<-j+1
  
}


Phase06$PhaseInt<-as.numeric(Phase06$PhaseOrd)
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
rm(list=setdiff(ls(),"Phase06"))
save.image("~/foodSystems/dataFS/Main/Phase06.RData")
