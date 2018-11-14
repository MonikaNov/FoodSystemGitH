rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(reshape)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Now the Z-scores
Prec<-read.csv( "CountyClimateM/pre_kenya_adm2_1999-2015.10thperc.csv",header=TRUE)
PrecZ<-read.csv( "CountyClimateM/pre_zscore_kenya_adm2_1999-2015.10thperc.csv",header=TRUE)
Tmx<-read.csv( "CountyClimateM/tmx_kenya_adm2_1999-2015.90thperc.csv",header=TRUE)
TmxZ<-read.csv( "CountyClimateM/tmx_zscore_kenya_adm2_1999-2015.90thperc.csv",header=TRUE)
SPEI3<-read.csv( "CountyClimateM/spei3_kenya_adm2_1999-2015.10thperc.csv",header=TRUE)
SPEI10<-read.csv( "CountyClimateM/spei10_kenya_adm2_1999-2015.10thperc.csv",header=TRUE)
SPEI12<-read.csv( "CountyClimateM/spei12_kenya_adm2_1999-2015.10thperc.csv",header=TRUE)
SPEI18<-read.csv( "CountyClimateM/spei18_kenya_adm2_1999-2015.10thperc.csv",header=TRUE)


climate<-list(Prec,PrecZ,Tmx,TmxZ,SPEI3,SPEI10,SPEI12,SPEI18)
rm(climate2)
climate2<-lapply(climate, function(x) melt(x, id.vars=c(1,2,3)))

rm(climate3)
climate3<-lapply(climate2, function(x) {names(x)[4]<-'MonthYear'
                 x$Month<-as.numeric(substr(x$MonthYear,2,3))
                   x$Year<-as.numeric(substr(x$MonthYear,5,8))
                  return(x)  }
               )

names(climate3[[1]])[5]<-"Prec"
names(climate3[[2]])[5]<-"PrecZ"
names(climate3[[3]])[5]<-"Tmx"
names(climate3[[4]])[5]<-"TmxZ"
names(climate3[[5]])[5]<-"SPEI3"
names(climate3[[6]])[5]<-"SPEI10"
names(climate3[[7]])[5]<-"SPEI12"
names(climate3[[8]])[5]<-"SPEI18"

sapply(climate3, dim)
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#now match the data with Phase data
rm(climate4)
climate4<-merge(climate3[[1]],climate3[[2]])

for (i in 3:8) 
climate4<-merge(climate4,climate3[[i]])

climate4<-climate4[with(climate4, order(ID1,Year,Month)),]

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# checking

sapply(unique(climate4$ID1), function(x) summary(climate4$TmxZ[climate4$ID1==x]))

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# now i need to merge it with crops production data
Crops<-read.csv( "New31Jan2018/KE_Ag_Stats_2014.csv",header=TRUE, na.strings = c("NA","NC","NC.","-","#DIV/0!"),colClasses=c("factor","factor","factor","factor","factor","factor","factor","factor","numeric","numeric","numeric"))  #numeric,numeric,numeric))
CrMaize<-subset(Crops, Crop=="Maize" & !Admin2=="")
CrMaize2<-subset(Crops, Crop=="Maize" & !Admin2=="" & Season=='')
CrMaize4<-CrMaize2[as.numeric(as.character(CrMaize2$Year))>1998,]
CrMaize4$Admin0<-NULL
CrMaize4$Season<-NULL

rm(CropsCnties)
uniq<-!duplicated(climate4[c("ADM2_NAME" ,"ID1")])
climateID<-climate4[uniq,c(1,2)]
CropsCnties<-as.character(unique(CrMaize4$Admin2))


CropsCnties<-data.frame(name=CropsCnties,ID=rep(NA))
 
for (i in 1:nrow(CropsCnties))
  
  {if (as.character(CropsCnties$name[i]) %in% as.character(climateID$ADM2_NAME))
  
    {for (j in 1:nrow(climateID))
      
  { if (CropsCnties$name[i] == as.character(climateID$ADM2_NAME[j]))
    CropsCnties$ID[i]<-climateID$ID[j]
    }
}}

summary(CropsCnties$ID)

CropsCnties$ID[41]<-65
CropsCnties$ID[39]<-11
CropsCnties$ID[29]<-4
# CropsCnties$ID[30]<-4 Muranga duplicated for some years->>just keep one
CropsCnties$ID[5]<-41   
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# CropsCnties$name<-as.character(CropsCnties$name)
# CropsCnties<-rbind(CropsCnties,c("Nairobi",18))
# rm(list=setdiff(ls(),"CropsCnties"))
# save.image("Main/CropsCnties.RData")

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

#now put Ids back to counties>>merge

rm(CrMaize5)
CrMaize5<-merge(CropsCnties,CrMaize4,by.x="name",by.y="Admin2")
CrMaize5<-CrMaize5[is.na(CrMaize5$ID)==FALSE,]
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

# save game

rm(list=setdiff(ls(), "climate4"))
save.image("~/foodSystems/dataFS/Main/climate4.RData")

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


rm(list=setdiff(ls(), "CrMaize5"))
save.image("~/foodSystems/dataFS/Main/CrMaize5.RData")

