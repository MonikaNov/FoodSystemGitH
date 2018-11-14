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



for (i in 1:nrow(CrMaize5))
{ 
  yearCurrent<-CrMaize5$Year[i]
  IDCurrent<-CrMaize5$ID[i] 
  yearLag<-as.numeric(as.character(CrMaize5$Year[i]))-1
  
  Precc<-climate4$Prec[which(climate4$Year== yearCurrent & climate4$ID1== IDCurrent)]
  PrecZc<-climate4$PrecZ[which(climate4$Year== yearCurrent & climate4$ID1== IDCurrent)]
  Tmxc<-climate4$Tmx[which(climate4$Year== yearCurrent & climate4$ID1== IDCurrent)]
  TmxZc<-climate4$TmxZ[which(climate4$Year== yearCurrent & climate4$ID1== IDCurrent)]
  SPEI3c<-climate4$SPEI3[which(climate4$Year== yearCurrent & climate4$ID1== IDCurrent)]
  SPEI10c<-climate4$SPEI10[which(climate4$Year== yearCurrent & climate4$ID1== IDCurrent)]
  SPEI12c<-climate4$SPEI12[which(climate4$Year== yearCurrent & climate4$ID1== IDCurrent)]
  SPEI18c<-climate4$SPEI18[which(climate4$Year== yearCurrent & climate4$ID1== IDCurrent)]
  
  Precl<-climate4$Prec[which(climate4$Year== yearLag & climate4$ID1== IDCurrent)]
  PrecZl<-climate4$PrecZ[which(climate4$Year== yearLag & climate4$ID1== IDCurrent)]
  Tmxcl<-climate4$Tmx[which(climate4$Year== yearLag & climate4$ID1== IDCurrent)]
  TmxZl<-climate4$TmxZ[which(climate4$Year== yearLag & climate4$ID1== IDCurrent)]
  SPEI3l<-climate4$SPEI3[which(climate4$Year== yearLag & climate4$ID1== IDCurrent)]
  SPEI10l<-climate4$SPEI10[which(climate4$Year== yearLag & climate4$ID1== IDCurrent)]
  SPEI12l<-climate4$SPEI12[which(climate4$Year== yearLag & climate4$ID1== IDCurrent)]
  SPEI18l<-climate4$SPEI18[which(climate4$Year== yearCurrent & climate4$ID1== IDCurrent)]
  
  if (length(PrecZc)>0 & sum(is.na(PrecZc)==0))
  {CrMaize5$i_JanPrecZ[i]<-PrecZc[1]
  CrMaize5$i_FebPrecZ[i]<-PrecZc[2]
  CrMaize5$i_MarPrecZ[i]<-PrecZc[3] 
  CrMaize5$i_AprPrecZ[i]<-PrecZc[4]
  CrMaize5$i_MayPrecZ[i]<-PrecZc[5]   
  CrMaize5$i_JunPrecZ[i]<-PrecZc[6]
  CrMaize5$i_JulPrecZ[i]<-PrecZc[7] 
  CrMaize5$i_AugPrecZ[i]<-PrecZc[8]
  CrMaize5$i_SepPrecZ[i]<-PrecZc[9]   
  CrMaize5$i_OctPrecZ[i]<-PrecZc[10]
  CrMaize5$i_NovPrecZ[i]<-PrecZc[11] 
  CrMaize5$i_DecPrecZ[i]<-PrecZc[12]   }
  
  if (length(PrecZl)>0 & sum(is.na(PrecZl)==0))
  {CrMaize5$i_JanPrecZl[i]<-PrecZl[1]
  CrMaize5$i_FebPrecZl[i]<-PrecZl[2]
  CrMaize5$i_MarPrecZl[i]<-PrecZl[3] 
  CrMaize5$i_AprPrecZl[i]<-PrecZl[4]
  CrMaize5$i_MayPrecZl[i]<-PrecZl[5]   
  CrMaize5$i_JunPrecZl[i]<-PrecZl[6]
  CrMaize5$i_JulPrecZl[i]<-PrecZl[7] 
  CrMaize5$i_AugPrecZl[i]<-PrecZl[8]
  CrMaize5$i_SepPrecZl[i]<-PrecZl[9]   
  CrMaize5$i_OctPrecZl[i]<-PrecZl[10]
  CrMaize5$i_NovPrecZl[i]<-PrecZl[11] 
  CrMaize5$i_DecPrecZl[i]<-PrecZl[12]   
  }  
  
}


#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


rm(list=setdiff(ls(), "CrMaize5"))
save.image("~/foodSystems/dataFS/Main/CrMaize5i_months.RData")
