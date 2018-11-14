rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(plm)

load("~/foodSystems/dataFS/Main/VCIphase.RData")
load("~/foodSystems/dataFS/Main/VCIclim.RData")
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#I will need to put the 3 datasets together: NDMA Phases, Climate and VCI. Climate just until 2015. I will keep it until 2017, with climate as NAs
# I will try both merge and loap and compare it. just for purpose of code practicing..

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#now match the data with Phase data, 1. loap

#         load all climate data

Prec<-read.csv( "CountyClimateM/pre_kenya_adm2_1999-2015.10thperc.csv",header=TRUE)
PrecZ<-read.csv( "CountyClimateM/pre_zscore_kenya_adm2_1999-2015.10thperc.csv",header=TRUE)

SPEI3<-read.csv( "CountyClimateM/spei3_kenya_adm2_1999-2015.10thperc.csv",header=TRUE)
SPEI10<-read.csv( "CountyClimateM/spei10_kenya_adm2_1999-2015.10thperc.csv",header=TRUE)
SPEI12<-read.csv( "CountyClimateM/spei12_kenya_adm2_1999-2015.10thperc.csv",header=TRUE)
SPEI18<-read.csv( "CountyClimateM/spei18_kenya_adm2_1999-2015.10thperc.csv",header=TRUE)
Tmx<-read.csv( "CountyClimateM/tmx_kenya_adm2_1999-2015.90thperc.csv",header=TRUE)
TmxZ<-read.csv( "CountyClimateM/tmx_zscore_kenya_adm2_1999-2015.90thperc.csv",header=TRUE)

VCIphaseClim<-VCIphase

VCIphaseClim$Prec<-NA
VCIphaseClim$PrecZ<-NA
VCIphaseClim$SPEI3<-NA
VCIphaseClim$SPEI10<-NA
VCIphaseClim$SPEI12<-NA
VCIphaseClim$SPEI18<-NA
VCIphaseClim$Tmx<-NA
VCIphaseClim$TmxZ<-NA
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#now match the data with Phase data, 1. loap

j<-18
for (ClimMeasure in list(Prec,PrecZ,SPEI3,SPEI10,SPEI12,SPEI18,Tmx,TmxZ))
{
  ClimMeasure2<-melt(ClimMeasure, id.vars=c(1,2,3,4))
  names(ClimMeasure2)[5]<-'MonthYear'
  ClimMeasure2$Month<-as.numeric(substr(ClimMeasure2$MonthYear,2,3))
  ClimMeasure2$Year<-as.numeric(substr(ClimMeasure2$MonthYear,5,8))
  print(summary(ClimMeasure2))
  
  for (i in 1:nrow(VCIphaseClim))
  {  
    
    if (VCIphaseClim$CountyID[i] %in% ClimMeasure2$ID1  &  VCIphaseClim$Year[i] %in% ClimMeasure2$Year  )
     { yearCurrent<-VCIphaseClim$Year[i]
    monthCurrent<-VCIphaseClim$Month[i]
    countyCurrent<-VCIphaseClim$CountyID[i] 
    
    county<-ClimMeasure2$value[ which(ClimMeasure2$Year== yearCurrent & ClimMeasure2$ID1==countyCurrent & ClimMeasure2$Month==monthCurrent  )]
    if (length(county)>0)
    { VCIphaseClim[i,j]<-county
    rm(county)}}
    
  }  
  j<-j+1
}

# checks..
all.equal(VCIphase,VCIphaseClim[,1:17])
all.equal(VCIphaseClim2,VCIphaseClim)
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#now match the data with Phase data, 2. Merge

names(VCIallz)[18]<-"TmxZ"


rm(VCIphaseClim2)
VCIphaseClim2<-merge(VCIphase, VCIallz[c(2,4,5,11:18)],by=c("CountyID","Year","Month"),       all.x=TRUE)
VCIphaseClim2<-VCIphaseClim2[with(VCIphaseClim2, order(CountyID, Year, Month)), ]
VCIphaseClim<-VCIphaseClim[with(VCIphaseClim, order(CountyID, Year, Month)), ]


all.equal(VCIphaseClim,VCIphaseClim2)

all.equal(VCIphaseClim[1:17],VCIphase)

# PARADA !! :-)))
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# cool, some checking:


setdiff(unique(VCIphase$CountyID),unique(VCIphaseClim$CountyID))
setdiff(unique(VCIphaseClim$CountyID),unique(VCIphase$CountyID))
setdiff(unique(VCIallz$CountyID),unique(VCIphase$CountyID))
setdiff(unique(VCIphase$CountyID),unique(VCIallz$CountyID))
table(VCIphaseClim$T,VCIphaseClim$CountyID)
table(VCIphaseClim$Year,VCIphaseClim$CountyID)
table(VCIphaseClim$Month,VCIphaseClim$CountyID)
table(VCIphaseClim$Month,VCIphaseClim$Year)

nrow(unique(VCIphaseClim[,c('CountyID','Year','Month')]))
nrow(unique(VCIphaseClim[,c('CountyID','Year')]))
nrow(unique(VCIphaseClim[,c('CountyID','Month')]))
nrow(unique(VCIphaseClim[,c('CountyID','T')]))

boxplot(VCImean~PhaseInt,data=VCIphaseClim,xaxt = "n", xlab='NDMA Early Warning Phase',ylab=c("VCI mean"))
axis(1, at=1:3, labels=c("Normal","Alert","Alarm"))


# time series Kitui, Isiolo

  
VCI3<-pdata.frame(VCIphaseClim,index=c("CountyID","T"))
IsioloVCI<-subset(VCI3, County.x %in% c("Isiolo"))
KituiVCI<-subset(VCI3, County.x %in% c("Kitui"))

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
rm(list=setdiff(ls(),"VCIphaseClim"))
save.image("~/foodSystems/dataFS/Main/VCIphaseClim.RData")
