rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
rm(VCI_NDMA)
VCI_NDMA<-read.csv( "VCI/VCI_NDMA/VCI_NDMA.csv",header=TRUE)
load("~/foodSystems/dataFS/Main/VCIphase.RData")





rm(CoIdentifier)
CoIdentifier<-as.character(unique(VCI_NDMA$COUNTY))

CoIdentifier<-data.frame(cbind(CoIdentifier,NA))
names(CoIdentifier)<-c("Name","ID")

PhaseNames<-unique(VCIphase[,c(1,17)])
names(PhaseNames)[2]<-c("Name")

Identifier<-merge(CoIdentifier,PhaseNames,all.x=TRUE)
Identifier$ID<-NULL
names(Identifier)[2]<-c("ID")

Identifier$ID[1]<-33
Identifier$ID[2]<-63
Identifier$ID[6]<-64
Identifier$ID[8]<-8
Identifier$ID[9]<-40
Identifier$ID[10]<-9
Identifier$ID[15]<-43  
Identifier$ID[16]<-6 
Identifier$ID[21]<-47 
Identifier$ID[23]<-49 


VCI_NDMA2<-VCI_NDMA

VCI_NDMA<-merge(VCI_NDMA,Identifier,by.x=("COUNTY"),by.y=("Name"),all.x=TRUE )

VCI_NDMA2<-VCI_NDMA2[with(VCI_NDMA2, order(COUNTY, YEAR, MONTH)), ]
VCI_NDMA<-VCI_NDMA[with(VCI_NDMA, order(COUNTY, YEAR, MONTH)), ]


all.equal(VCI_NDMA2,VCI_NDMA[1:4])

VCI_NDMA<-VCI_NDMA[with(VCI_NDMA, order(ID, YEAR, MONTH)), ]

names(VCI_NDMA)[4]<-"VCI_ndma"

VCI2phase<-merge(VCIphase, VCI_NDMA,by.x=c("CountyID","Year","Month"), by.y=c("ID","YEAR","MONTH")    ,  all.x=TRUE)
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

summary(VCI2phase)
cor.test(VCI2phase$VCImean,VCI2phase$VCI_ndma)
plot(VCI2phase$VCI_ndma,VCI2phase$VCImean,xlab=("VCI NDMA"),ylab="VCI BOKU")

table(VCI_NDMA$COUNTY,VCI_NDMA$CountyID)
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#just to verify

names(VCI_NDMA)[3]<-"Month"
names(VCI_NDMA)[2]<-"Year"
names(VCI_NDMA)[5]<-"CountyID"

VCI3phase<-merge(VCIphase, VCI_NDMA  ,  all.x=TRUE)
all.equal(VCI3phase,VCI2phase)

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
VCI_NDMA<-VCI_NDMA[with(VCI_NDMA, order(CountyID, Year, Month)), ]
VCI2phase<-VCI2phase[with(VCI2phase, order(CountyID, Year, Month)), ]


rm(list=setdiff(ls(),c("VCI2phase","VCI_NDMA")))
save.image("~/foodSystems/dataFS/Main/VCI_NDMAphase.RData")