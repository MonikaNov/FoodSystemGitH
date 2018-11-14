rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
load("Main/VCI.RData")
library(plm)
library(pglm)
library(reshape)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# First I need to match it to the climate data. I could use data table Phase06, but that omly have years 2013-2015 to match the phase data, so I am loosing observations.
# I don't need to loose the observations unless I want analyse all 3 vars at once (VCI, Climate and Phase..) SO far I will just analyse CVI and climate..
# But I will have to reduce the VCI data to only thos which smaller than 2016 to match the climate data

VCIallz<-AllZonesDF[which(AllZonesDF$Year<2016),]
VCIallz$Prec<-NA
VCIallz$PrecZ<-NA
VCIallz$SPEI3<-NA
VCIallz$SPEI10<-NA
VCIallz$SPEI12<-NA
VCIallz$SPEI18<-NA
VCIallz$Tmx<-NA
VCIallz$Tmxz<-NA


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

j<-10
for (ClimMeasure in list(Prec,PrecZ,SPEI3,SPEI10,SPEI12,SPEI18,Tmx,TmxZ))
{
  ClimMeasure2<-melt(ClimMeasure, id.vars=c(1,2,3,4))
  names(ClimMeasure2)[5]<-'MonthYear'
  ClimMeasure2$Month<-as.numeric(substr(ClimMeasure2$MonthYear,2,3))
  ClimMeasure2$Year<-as.numeric(substr(ClimMeasure2$MonthYear,5,8))
  print(summary(ClimMeasure2))
  
  
  
  for (i in 1:nrow(VCIallz))
  {  
    
    if (VCIallz$CountyID[i] %in% ClimMeasure2$ID1)
      yearCurrent<-VCIallz$Year[i]
    monthCurrent<-VCIallz$Month[i]
    countyCurrent<-VCIallz$CountyID[i] 
    
    county<-ClimMeasure2$value[ which(ClimMeasure2$Year== yearCurrent & ClimMeasure2$ID1==countyCurrent & ClimMeasure2$Month==monthCurrent  )]
    if (length(county)>0)
    { VCIallz[i,j]<-county
    rm(county)}
    
  }  
  j<-j+1
  
}

#I need to add time index


VCIallz<-cbind(VCIallz,rep(seq(1:169),47))
names(VCIallz)[18]<-'T'

VCIallzTest<-VCIallz
VCIallz<-VCIallzTest[,c(8,9,18,1:7,10:17)]
  
rm(VCIallzTest)

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
plot(VCIallz$Year,VCIallz$T)
plot(VCIallz$Month,VCIallz$T)
table(VCIallz$Month,VCIallz$T)
plot(VCIallz$p50,VCIallz$Prec)
plot(VCIallz$p10,VCIallz$Prec)
plot(VCIallz$mean,VCIallz$Prec)
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo


summary(Audrey)

Audrey<-plm(mean~I(Prec^2), VCIallz, index= c("CountyID","T"))
summary(Audrey)

Audrey<-plm(mean~Prec, VCIallz, index= c("CountyID","T"))
summary(Audrey)

plot(VCIallz$mean, VCIallz$Prec)
plot(VCIallz$mean, VCIallz$PrecZ)
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
rm(list=setdiff(ls(),"VCIallz"))
save.image("~/foodSystems/dataFS/Main/VCIclim.RData")
write.csv(VCIallz, file = "VCI/VCIclimate.csv")