rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Crops<-read.csv( "New31Jan2018/KE_Ag_Stats_2014.csv",header=TRUE, na.strings = c("NA","NC","NC.","-","#DIV/0!"),colClasses=c("factor","factor","factor","factor","factor","factor","factor","factor","numeric","numeric","numeric"))  #numeric,numeric,numeric))

# two really small values which appear as zero: 1. Sorghum, Machakos, 1970 - Yield, 2. Maize, Migori, 1995, Yield. Nothing else:
Crops$Yield[(Crops$Admin2== "Machakos" & Crops$Year==1970 & Crops$Crop=="Sorghum" )]<-0.0038
Crops$Yield[(Crops$Admin2== "Migori" & Crops$Year==1995 & Crops$Crop=="Maize" )]<-0.0027

# probably not so bad, t.test insignificant
rm(CrMaize10)
CrMaize10<-Crops
CrMaize10$county<-as.character(Crops$Admin2)
CrMaize10$county[CrMaize10$Admin1=="Nairobi"]<-"Nairobi"
CrMaize10$county<-as.factor(CrMaize10$county)
rm(CrMaize11)
CrMaize11<-subset(CrMaize10, Crop=="Maize" & !county=="")
rm(CrMaize12)
CrMaize12<-subset(CrMaize11, Yield>0 &  Season=='')
table(CrMaize11$Year[CrMaize11$Yield==0])




# I need to give them ID as well
load("Main/CropsCnties.RData")
CropsCnties$name<-as.character(CropsCnties$name)

names(CropsCnties)[1]<-"county"
setdiff(unique(CrMaize12$county),CropsCnties$county)
setdiff(CropsCnties$county,unique(CrMaize12$county))

 # for checking purposes
CrMaize12<-CrMaize12[with(CrMaize12, order(county, Year)), ]
CrMaize13<-merge(CrMaize12,CropsCnties,all.x=TRUE,sort=FALSE)
CrMaize13$ID<-as.numeric(CrMaize13$ID)
CrMaize13$ID[CrMaize13$county=="Elgeyo Marakwet"]<-c(41)
CrMaize13$ID[CrMaize13$county=="Muranga'a"]<-c(4)
CrMaize13$ID[CrMaize13$county=="Murang'a"]<-c(4)
CrMaize13$ID[CrMaize13$county=="Taita-Taveta"]<-c(11)
CrMaize13$ID[CrMaize13$county=="Tharaka Nithi"]<-c(65)
AllZonesDF[AllZonesDF$County=="Nandi",] # to see that Nani is number80. But I need to load AllZonesDF from some other part..
CrMaize13$ID[CrMaize13$county=="Nandi"]<-c(80)
summary(CrMaize13$ID)
CrMaize13[is.na(CrMaize13$ID)==TRUE,]
CrMaize13$county[is.na(CrMaize13$ID)==TRUE]

all.equal(CrMaize13[-2],CrMaize12[c(12,2:11,1)])

nrow(unique(CrMaize13[c(7,13)])) # Muranga twice>>> need to clean this

CrMaize13<-subset(CrMaize13, ! (county=="Murang'a" & Year %in% c(1975:1992,1995,1997:2001)))
nrow(unique(CrMaize13[c(1)]))

nrow(unique(CrMaize13[c(7,13)]))


CrMaize13[CrMaize13$ID==4,]
table(CrMaize13$Source)
CrMaize13<-CrMaize13[with(CrMaize13, order(ID, Year)), ]


# ehm. I forgot to remove long rains and short rains>>this can be what caused so many problems on Friday
# now checking after I have loaded climate4 using code gettingData.R

CrMaize13[1660:1666,]
climate4[which(climate4$ID1==4),]
CrMaize11[which(CrMaize11$county %in% c("Muranga'a","Murang'a")),]
CrMaize13[CrMaize13$ID==4,]
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# now the quantiles:
quantile(CrMaize13$Yield,probs=c(0.01,0.05,0.9,0.95,0.99))

hist(CrMaize13$Yield,150,xlim=c(0,5))

mxc<-quantile(CrMaize13$Yield,probs=c(0.99))

CrMaize14<-subset(CrMaize13,Yield<=mxc)
summary(CrMaize14)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

rm(list=setdiff(ls(),"CrMaize14"))
save.image("Main/CrMaize14.RData")

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

rm(list=setdiff(ls(),"Crops"))
save.image("Main/Crops.RData")

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

rm(list=setdiff(ls(),"CrMaize13"))
save.image("Main/CrMaize13.RData")
