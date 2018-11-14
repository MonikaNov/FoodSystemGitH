rm(list=ls())

WDuni<-c("/home/m/mn/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/sussex/US/paper3") # doma

setwd(WDuni)
setwd(WDhome)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(reshape)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Perc0<-read.csv( "CountyClimateM/pre_kenya_adm2_1999-2015.10thperc.csv",header=TRUE)
summary(Perc0)


Perc1<-melt(Perc0, id.vars=c(1,2,3))
names(Perc1)[4]<-'MonthYear'
Perc1$Month<-substr(Perc1$MonthYear,2,3)
Perc1$Year<-substr(Perc1$MonthYear,5,8)

mean_sd<-as.data.frame(summarise(group_by(Perc1, Month, ID1),
          mean=mean(value), sd=sd(value)))

Perc1$Zscore<-NA

for (i in 1:nrow(Perc1) ) {
  
  j<-which(mean_sd$Month==Perc1$Month[i] &  mean_sd$ID1==Perc1$ID1[i]  )
Perc1$Zscore[i]<-(Perc1$value[i]-mean_sd$mean[j])/mean_sd$sd[j]
}

Climat1$cat<-NA

for (i in 1:nrow(Perc1) ) {
  if (is.na(Perc1$Zscore[i]) == FALSE)
  if (Perc1$Zscore[i] < -1)  Perc1$cat[i]<-1
else if   (Perc1$Zscore[i] < 1)  Perc1$cat[i]<-2
else Perc1$cat[i]<-3}



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Now the Z-scores
PercZ<-read.csv( "CountyClimateM/pre_zscore_kenya_adm2_1999-2015.10thperc.csv",header=TRUE)

PercZ1<-melt(PercZ, id.vars=c(1,2,3,4))
names(PercZ1)[5]<-'MonthYear'
PercZ1$Month<-substr(PercZ1$MonthYear,2,3)
PercZ1$Year<-substr(PercZ1$MonthYear,5,8)

boxplot(PercZ1$value~PercZ1$ADM2_NAME)