rm(list=ls())

WDuni<-c("/home/m/mn/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

load("~/foodSystems/dataFS/Main/CrMaize5.RData")
load("~/foodSystems/dataFS/Main/climate4.RData")

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
 # I need to create indicators of drought


CrMaize5$i1<-0
CrMaize5$i_any<-0


for (i in 1:nrow(CrMaize5))
{  rm(PrecZc)
  
    yearCurrent<-CrMaize5$Year[i]
  IDCurrent<-CrMaize5$ID[i] 
  
  Precc<-climate4$Prec[which(climate4$Year== yearCurrent & climate4$ID1== IDCurrent)]
  PrecZc<-climate4$PrecZ[which(climate4$Year== yearCurrent & climate4$ID1== IDCurrent)]
  Tmxc<-climate4$Tmx[which(climate4$Year== yearCurrent & climate4$ID1== IDCurrent)]
  TmxZc<-climate4$TmxZ[which(climate4$Year== yearCurrent & climate4$ID1== IDCurrent)]
  SPEI3c<-climate4$SPEI3[which(climate4$Year== yearCurrent & climate4$ID1== IDCurrent)]
  SPEI10c<-climate4$SPEI10[which(climate4$Year== yearCurrent & climate4$ID1== IDCurrent)]
  SPEI12c<-climate4$SPEI12[which(climate4$Year== yearCurrent & climate4$ID1== IDCurrent)]
  SPEI18c<-climate4$SPEI18[which(climate4$Year== yearCurrent & climate4$ID1== IDCurrent)]
  
  if (length(PrecZc)>0)
  {
    if (  (PrecZc[3]< -1 | PrecZc[4] < -1 | PrecZc[5] < -1) & (PrecZc[10]< -1 | PrecZc[11] < -1 | PrecZc[12] < -1)  )
      
    {CrMaize5$i1[i]<-1}} 
  if (length(Precc)>0)  
    CrMaize5$icum[i]<-mean(Precc, na.rm=TRUE)
  if (length(Tmxc)>0)  
    CrMaize5$iTcum[i]<-mean(Tmxc[c(3,4,5,10,11,12)], na.rm=TRUE)
  if (length(TmxZc)>0)  
    CrMaize5$iTzcum[i]<-mean(TmxZc[c(3,4,5,10,11,12)], na.rm=TRUE) 
  }


boxplot(CrMaize5$Yield~CrMaize5$i1)

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# past year

CrMaize5$i1lag<-0


for (i in 1:nrow(CrMaize5))
{  
  
  yearCurrent<-as.numeric(as.character(CrMaize5$Year[i]))-1
  IDCurrent<-CrMaize5$ID[i] 
  
  Precc<-climate4$Prec[which(climate4$Year== yearCurrent & climate4$ID1== IDCurrent)]
  PrecZc<-climate4$PrecZ[which(climate4$Year== yearCurrent & climate4$ID1== IDCurrent)]
  Tmxc<-climate4$Tmx[which(climate4$Year== yearCurrent & climate4$ID1== IDCurrent)]
  TmxZc<-climate4$TmxZ[which(climate4$Year== yearCurrent & climate4$ID1== IDCurrent)]
  SPEI3c<-climate4$SPEI3[which(climate4$Year== yearCurrent & climate4$ID1== IDCurrent)]
  SPEI10c<-climate4$SPEI10[which(climate4$Year== yearCurrent & climate4$ID1== IDCurrent)]
  SPEI12c<-climate4$SPEI12[which(climate4$Year== yearCurrent & climate4$ID1== IDCurrent)]
  SPEI18c<-climate4$SPEI18[which(climate4$Year== yearCurrent & climate4$ID1== IDCurrent)]
  
  
  if (length(PrecZc)>0)  
  {
    if (  (PrecZc[3]< -1 | PrecZc[4] < -1 | PrecZc[5] < -1) & (PrecZc[10]< -1 | PrecZc[11] < -1 | PrecZc[12] < -1)  )
      
    {CrMaize5$i1lag[i]<-1}}
  
  if (length(Precc)>0)  
      CrMaize5$icum_lag[i]<-mean(Precc, na.rm=TRUE)
  if (length(Tmxc)>0)  
    CrMaize5$iTcum_lag[i]<-mean(Tmxc[c(3,4,5,10,11,12)], na.rm=TRUE)
  if (length(TmxZc)>0)  
    CrMaize5$iTzcum_lag[i]<-mean(TmxZc[c(3,4,5,10,11,12)], na.rm=TRUE)
  }


boxplot(CrMaize5$Yield~I(CrMaize5$i1lag))
boxplot(CrMaize5$Yield~I(CrMaize5$i1))
boxplot(CrMaize5$Yield~I(CrMaize5$i1*CrMaize5$i1lag))

CrMaize5$i1_lagcur<-CrMaize5$i1*CrMaize5$i1lag
CrMaize5$i1_sum<-CrMaize5$i1+CrMaize5$i1lag
boxplot(CrMaize5$Yield~I(CrMaize5$i1_lagcur))
boxplot(CrMaize5$Yield~I(CrMaize5$i1_sum))


#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

