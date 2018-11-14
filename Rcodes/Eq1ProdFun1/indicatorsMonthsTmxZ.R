rm(list=ls())

WDuni<-c("/home/m/mn/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

load("~/foodSystems/dataFS/Main/CrMaize5i_months.RData")
load("~/foodSystems/dataFS/Main/climate4.RData")

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
# I need to create indicators of drought

for (i in 1:nrow(CrMaize5))
{ 
  yearCurrent<-CrMaize5$Year[i]
  IDCurrent<-CrMaize5$ID[i] 
  yearLag<-as.numeric(as.character(CrMaize5$Year[i]))-1
  
  Tmxc<-climate4$Tmx[which(climate4$Year== yearCurrent & climate4$ID1== IDCurrent)]
  TempZc<-climate4$TmxZ[which(climate4$Year== yearCurrent & climate4$ID1== IDCurrent)]
 TempZl<-climate4$TmxZ[which(climate4$Year== yearLag & climate4$ID1== IDCurrent)]
  
  if (length(TempZc)>0 & sum(is.na(TempZc)==0))
  {CrMaize5$i_JanTempZ[i]<-TempZc[1]
  CrMaize5$i_FebTempZ[i]<-TempZc[2]
  CrMaize5$i_MarTempZ[i]<-TempZc[3] 
  CrMaize5$i_AprTempZ[i]<-TempZc[4]
  CrMaize5$i_MayTempZ[i]<-TempZc[5]   
  CrMaize5$i_JunTempZ[i]<-TempZc[6]
  CrMaize5$i_JulTempZ[i]<-TempZc[7] 
  CrMaize5$i_AugTempZ[i]<-TempZc[8]
  CrMaize5$i_SepTempZ[i]<-TempZc[9]   
  CrMaize5$i_OctTempZ[i]<-TempZc[10]
  CrMaize5$i_NovTempZ[i]<-TempZc[11] 
  CrMaize5$i_DecTempZ[i]<-TempZc[12]   }
  
  if (length(TempZl)>0 & sum(is.na(TempZl)==0))
  {CrMaize5$i_JanTempZl[i]<-TempZl[1]
  CrMaize5$i_FebTempZl[i]<-TempZl[2]
  CrMaize5$i_MarTempZl[i]<-TempZl[3] 
  CrMaize5$i_AprTempZl[i]<-TempZl[4]
  CrMaize5$i_MayTempZl[i]<-TempZl[5]   
  CrMaize5$i_JunTempZl[i]<-TempZl[6]
  CrMaize5$i_JulTempZl[i]<-TempZl[7] 
  CrMaize5$i_AugTempZl[i]<-TempZl[8]
  CrMaize5$i_SepTempZl[i]<-TempZl[9]   
  CrMaize5$i_OctTempZl[i]<-TempZl[10]
  CrMaize5$i_NovTempZl[i]<-TempZl[11] 
  CrMaize5$i_DecTempZl[i]<-TempZl[12]   
  }  
  
}


#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

#lags i n 1999 must be NA

for (i in 1:nrow(CrMaize5))
 if (CrMaize5$Year[i] == 1999)
{CrMaize5$i_JanTempZl[i]<-NA
CrMaize5$i_FebTempZl[i]<-NA
CrMaize5$i_MarTempZl[i]<-NA
CrMaize5$i_AprTempZl[i]<-NA
CrMaize5$i_MayTempZl[i]<-NA
CrMaize5$i_JunTempZl[i]<-NA
CrMaize5$i_JulTempZl[i]<-NA
CrMaize5$i_AugTempZl[i]<-NA
CrMaize5$i_SepTempZl[i]<-NA
CrMaize5$i_OctTempZl[i]<-NA
CrMaize5$i_NovTempZl[i]<-NA
CrMaize5$i_DecTempZl[i]<-NA

  CrMaize5$i_JanPrecZl[i]<-NA
  CrMaize5$i_FebPrecZl[i]<-NA
  CrMaize5$i_MarPrecZl[i]<-NA
  CrMaize5$i_AprPrecZl[i]<-NA
  CrMaize5$i_MayPrecZl[i]<-NA  
  CrMaize5$i_JunPrecZl[i]<-NA
  CrMaize5$i_JulPrecZl[i]<-NA
  CrMaize5$i_AugPrecZl[i]<-NA
  CrMaize5$i_SepPrecZl[i]<-NA  
  CrMaize5$i_OctPrecZl[i]<-NA
  CrMaize5$i_NovPrecZl[i]<-NA 
  CrMaize5$i_DecPrecZl[i]<-NA  }

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


rm(list=setdiff(ls(), "CrMaize5"))
save.image("~/foodSystems/dataFS/Main/CrMaize5i_months2.RData")
