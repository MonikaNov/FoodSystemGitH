rm(list=ls())

WDuni<-c("/home/m/mn/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(reshape)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Now the Z-scores
PercZ<-read.csv( "CountyClimateM/pre_zscore_kenya_adm2_1999-2015.10thperc.csv",header=TRUE)

PercZ1<-melt(PercZ, id.vars=c(1,2,3,4))
names(PercZ1)[5]<-'MonthYear'
PercZ1$Month<-substr(PercZ1$MonthYear,2,3)
PercZ1$Year<-substr(PercZ1$MonthYear,5,8)

boxplot(PercZ1$value~PercZ1$ADM2_NAME)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# load and prepare Maize data

Crops<-read.csv( "New31Jan2018/KE_Ag_Stats_2014.csv",header=TRUE, na.strings = c("NA","NC","NC.","-","#DIV/0!"),colClasses=c("factor","factor","factor","factor","factor","factor","factor","factor","numeric","numeric","numeric"))  #numeric,numeric,numeric))
CrMaize<-subset(Crops, Crop=="Maize" & !Admin2=="")
CrMaize2<-subset(Crops, Crop=="Maize" & !Admin2=="" & Season=='')

CrMaize3<-CrMaize2[as.numeric(as.character(CrMaize2$Year))>2005,]
CrMaize3$indic<-0


for (i in 1:nrow(CrMaize3))
{  rm(county)
  county<-numeric()

  if (CrMaize3$Admin2[i] %in% PercZ1$ADM2_NAME)
    yearCurrent<-CrMaize3$Year[i]
    countyCurrent<-CrMaize3$Admin2[i] 
      
     county<-PercZ1$value[which(PercZ1$Year== yearCurrent & as.character(PercZ1$ADM2_NAME)==as.character(countyCurrent))]
   if (length(county)>0)
     {
    if (  (county[3]< -1 | county[4] < -1 | county[5] < -1) & (county[10]< -1 | county[11] < -1 | county[12] < -1)  )
     
    {CrMaize3$indic[i]<-1}}}


boxplot(CrMaize3$Yield~CrMaize3$indic)
t.test(CrMaize3$Yield~CrMaize3$indic)

boxplot(CrMaize3$Area~CrMaize3$indic)
boxplot(CrMaize3$MT~CrMaize3$indic)