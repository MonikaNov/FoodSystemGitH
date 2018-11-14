rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(plm)
library(pglm)
library(reshape)


load("~/foodSystems/dataFS/Main/VCI.RData")
load("~/foodSystems/dataFS/Main/VCIclim.RData")
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# new vars

rm(Phase08)
Phase08<-Phase04
Phase08$PhaseInt<-as.numeric(Phase08$PhaseOrd)

table(VCIallz$Year)
table(Phase08$Year)
# VCI data range: 2001-2018
# Phase data range: 2013-2017

rm(Phase09)
Phase09<-merge(Phase08, AllZonesDF,by=c("CountyID","Year","Month"))

Phase09<-Phase09[with(Phase09, order(CountyID, Year, Month)), ]

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# some checking:

length(unique(Phase09$County.x))
length(unique(Phase09$County.y))
length(unique(Phase09$Year))

setdiff(unique(Phase08$CountyID),unique(Phase09$CountyID))
# VCI for Mwingi and Ijara is missing!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
4*12*23+7*23
4*12*25+7*25

#parada, vypada to, ze pocty sedi

# I have also checked four rows (and some around them) whether the values are the same as in the original files. All seems ok.GOOD

Phase09[duplicated(Phase09[,c('CountyID','Year','Month')]),]
Phase09[Phase09$County.x=='Kwale' & duplicated(Phase09[,c('CountyID','Year')]),]
Phase09[Phase09$County.x=='Kwale' & duplicated(Phase09[,c('CountyID','Year',"Month")]),]

Phase09[Phase09$County.x=='Nyeri' & duplicated(Phase09[,c('CountyID','Year')]),]
Phase09[Phase09$County.x=='Nyeri' & duplicated(Phase09[,c('CountyID','Year',"Month")]),]



unique(Phase09[,c('CountyID','Year','Month')])
nrow(unique(Phase09[,c('CountyID','Year','Month')]))

unique(Phase09[,c('CountyID','T')])
nrow(unique(Phase09[,c('CountyID','T')]))
table(Phase09$T,Phase09$CountyID)
table(Phase09$Year,Phase09$CountyID)
table(Phase09$Month,Phase09$CountyID)
table(Phase09$Phase,Phase09$PhaseInt)
table(Phase09$Phase,Phase09$PhaseOrd)



Phase09[Phase09$County.x=='Nyeri' & duplicated(Phase09[,c('CountyID','Year')]),]
Phase09[Phase09$County.x=='Nyeri' & duplicated(Phase09[,c('CountyID','Year',"Month")]),]
Phase09[duplicated(Phase09[,c('CountyID','T')]),]

names(Phase09)[12:16]
names(Phase09)[12:16]<-paste('VCI',names(Phase09)[12:16],sep="")

names(Phase09)[12:16]

VCIphase<-Phase09
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
rm(list=setdiff(ls(),"VCIphase"))
save.image("~/foodSystems/dataFS/Main/VCIphase.RData")
