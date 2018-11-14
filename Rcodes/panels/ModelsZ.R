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



#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

# Now i can test my second plm estimate



PhaseZ<-list()
PhaseZ[[1]]<-plm(PhaseNum~PrecZ+Tmxz, Phase06, index= c("CountyID","T"))
PhaseZ[[2]]<-plm(PhaseNum~PrecZ+Tmxz+SPEI3, Phase06, index= c("CountyID","T"))
PhaseZ[[3]]<-plm(PhaseNum~PrecZ+Tmxz+SPEI10, Phase06, index= c("CountyID","T"))
PhaseZ[[4]]<-plm(PhaseNum~PrecZ+Tmxz+SPEI12, Phase06, index= c("CountyID","T"))
PhaseZ[[5]]<-plm(PhaseNum~PrecZ+Tmxz+SPEI18, Phase06, index= c("CountyID","T"))

PhaseZ[[6]]<-plm(PhaseNum~Tmxz+SPEI3, Phase06, index= c("CountyID","T"))
PhaseZ[[7]]<-plm(PhaseNum~Tmxz+SPEI10, Phase06, index= c("CountyID","T"))

PhaseZ[[8]]<-plm(PhaseNum~Tmxz+SPEI12, Phase06, index= c("CountyID","T"))
PhaseZ[[9]]<-plm(PhaseNum~Tmxz+SPEI18, Phase06, index= c("CountyID","T"))

lapply(PhaseZ,summary)
PhaseZVC<-list()
PhaseZVC[[1]]<-pvcm(PhaseNum~PrecZ+Tmxz, Phase06, index= c("CountyID","T"),model="within")
PhaseZVC[[2]]<-pvcm(PhaseNum~PrecZ+Tmxz+SPEI3, Phase06, index= c("CountyID","T"),model="within")
PhaseZVC[[3]]<-pvcm(PhaseNum~PrecZ+Tmxz+SPEI10, Phase06, index= c("CountyID","T"),model="within")
PhaseZVC[[4]]<-pvcm(PhaseNum~PrecZ+Tmxz+SPEI12, Phase06, index= c("CountyID","T"),model="within")
PhaseZVC[[5]]<-pvcm(PhaseNum~PrecZ+Tmxz+SPEI18, Phase06, index= c("CountyID","T"),model="within")
PhaseZVC[[6]]<-pvcm(PhaseNum~Tmxz+SPEI3, Phase06, index= c("CountyID","T"),model="within")
PhaseZVC[[7]]<-pvcm(PhaseNum~Tmxz+SPEI10, Phase06, index= c("CountyID","T"),model="within")
PhaseZVC[[8]]<-pvcm(PhaseNum~Tmxz+SPEI12, Phase06, index= c("CountyID","T"),model="within")
PhaseZVC[[9]]<-pvcm(PhaseNum~Tmxz+SPEI18, Phase06, index= c("CountyID","T"),model="within")


for (i in 1:length(PhaseZ))
print(pooltest(PhaseZ[[i]],PhaseZVC[[i]]))


mapply(pooltest,PhaseZ,PhaseZVC)

#6,7 the best for now

#-----------------------------------------------------------------------------------------------------------------------------------------------------
#Random

PhaseZrandom<-list()
PhaseZrandom[[1]]<-plm(PhaseNum~PrecZ+Tmxz, Phase06, index= c("CountyID","T"), model = c("random"))
PhaseZrandom[[2]]<-plm(PhaseNum~PrecZ+Tmxz+SPEI3, Phase06, index= c("CountyID","T"), model = c("random"))
PhaseZrandom[[3]]<-plm(PhaseNum~PrecZ+Tmxz+SPEI10, Phase06, index= c("CountyID","T"), model = c("random"))
PhaseZrandom[[4]]<-plm(PhaseNum~PrecZ+Tmxz+SPEI12, Phase06, index= c("CountyID","T"), model = c("random"))
PhaseZrandom[[5]]<-plm(PhaseNum~PrecZ+Tmxz+SPEI18, Phase06, index= c("CountyID","T"), model = c("random"))

PhaseZrandom[[6]]<-plm(PhaseNum~Tmxz+SPEI3, Phase06, index= c("CountyID","T"), model = c("random"))
PhaseZrandom[[7]]<-plm(PhaseNum~Tmxz+SPEI10, Phase06, index= c("CountyID","T"), model = c("random"))

PhaseZrandom[[8]]<-plm(PhaseNum~Tmxz+SPEI12, Phase06, index= c("CountyID","T"), model = c("random"))
PhaseZrandom[[9]]<-plm(PhaseNum~Tmxz+SPEI18, Phase06, index= c("CountyID","T"), model = c("random"))

lapply(PhaseZrandom,summary)


more<-plm(PhaseNum~PrecZ+Tmxz+SPEI3+SPEI10+SPEI12+SPEI18, Phase06, index= c("CountyID","T") )
moreR<-plm(PhaseNum~PrecZ+Tmxz+SPEI3+SPEI10+SPEI12+SPEI18, Phase06, index= c("CountyID","T"), model = c("random"))
summary(more)
summary(moreR)


#-----------------------------------------------------------------------------------------------------------------------------------------------------
#pooling

PhaseZpooling<-list()
PhaseZpooling[[1]]<-plm(PhaseNum~PrecZ+Tmxz, Phase06, index= c("CountyID","T"), model = c("pooling"))
PhaseZpooling[[2]]<-plm(PhaseNum~PrecZ+Tmxz+SPEI3, Phase06, index= c("CountyID","T"), model = c("pooling"))
PhaseZpooling[[3]]<-plm(PhaseNum~PrecZ+Tmxz+SPEI10, Phase06, index= c("CountyID","T"), model = c("pooling"))
PhaseZpooling[[4]]<-plm(PhaseNum~PrecZ+Tmxz+SPEI12, Phase06, index= c("CountyID","T"), model = c("pooling"))
PhaseZpooling[[5]]<-plm(PhaseNum~PrecZ+Tmxz+SPEI18, Phase06, index= c("CountyID","T"), model = c("pooling"))

PhaseZpooling[[6]]<-plm(PhaseNum~Tmxz+SPEI3, Phase06, index= c("CountyID","T"), model = c("pooling"))
PhaseZpooling[[7]]<-plm(PhaseNum~Tmxz+SPEI10, Phase06, index= c("CountyID","T"), model = c("pooling"))

PhaseZpooling[[8]]<-plm(PhaseNum~Tmxz+SPEI12, Phase06, index= c("CountyID","T"), model = c("pooling"))
PhaseZpooling[[9]]<-plm(PhaseNum~Tmxz+SPEI18, Phase06, index= c("CountyID","T"), model = c("pooling"))

lapply(PhaseZpooling,summary)


more<-plm(PhaseNum~PrecZ+Tmxz+SPEI3+SPEI10+SPEI12+SPEI18, Phase06, index= c("CountyID","T") )
moreR<-plm(PhaseNum~PrecZ+Tmxz+SPEI3+SPEI10+SPEI12+SPEI18, Phase06, index= c("CountyID","T"), model = c("random"))
moreP<-plm(PhaseNum~PrecZ+Tmxz+SPEI3+SPEI10+SPEI12+SPEI18, Phase06, index= c("CountyID","T"), model = c("pooling"))
summary(more)
summary(moreR)
summary(moreP)