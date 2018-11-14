# rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma
library('dplyr')
library('purrr')
library("reshape")
setwd(WDuni)
setwd(WDhome)

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
  
weather<-read.csv("climateOct/Final.csv",na.strings="-999",dec=".")
rm(coefVarP)

dim(weather[complete.cases(weather[-3]),])  # seems good... now test in regression
testRe<-lm(AvgTemp_MarMay~.,data=weather[c(1,2,5:43)])
summary(testRe)
nobs(testRe)


weather[!complete.cases(weather[-3]),]
# now merge with county codes
IDdict<-read.csv("Main/IDdict.csv")
weather$name<-NULL
weather<-merge(weather,IDdict, all.x=TRUE)

#  ---- and test...   ------------------------------------------------------------------------------

table(weather$code,weather$name)
weather$name[weather$code == 51345]
weather[weather$code == 51336,c("ID1","name")]
table(weather$code,weather$ID1)

testRe<-lm(AvgTemp_MarMay~.,data=weather[c(1,2,5:43)])
summary(testRe)
nobs(testRe)

table(weather$ASAL,weather$ID1)
table(weather$ASAL,weather$name)

weather$name<-droplevels(weather$name)
table(weather$year,weather$name)

all_equal(weather[c(1:2,4:42)],weatherB[-c(3,4)])
names(weather)[2]<-"Year"
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# now merge with maize

load("Main/CrMaize15.RData") # the last (and the best available) dataset before I cut it timewise..
rm(CrMaize30)
CrMaize30<-subset(CrMaize15,Year %in% (1981:2013),select=c("Admin1","county","Admin2","Year","Area","Yield","MT","ID","west1"))
names(CrMaize30)[8]<-"ID1"

itsdata<-merge(weather,CrMaize30,all.x=TRUE)
itsdata<-itsdata[order(itsdata$ID1,itsdata$Year ),]
weather<-weather[order(weather$ID1,weather$Year ),] 
all.equal(itsdata[4:44],weather[3:43],check.attributes=FALSE)   #  so far so good.......
all.equal(itsdata[c(3,1,4:44,2)],weather[c(1:44)],check.attributes=FALSE)   # VVVVEEEEErrrrrrryyyyyyyyyyyyy  GGGGGGGGOOOOOOOODDDDDDDDD

#------------      test in a model...............
dim(itsdata[complete.cases(itsdata),])
    
testRe<-lm(Yield~.,data=itsdata[c(1:43,49)])
summary(testRe)


testRe<-lm(Yield~.-Yield-Admin2-county-Admin1,data=itsdata)
summary(testRe)
nobs(testRe)
nobs(testRe)

#G.O.O.D

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# maybe also check similarity with the previous data?

load("Main/dataOct.RData")

names(dataAll[156:176])
names(itsdata)

summary(dataAll$PrecCV_MAM)
summary(itsdata$PrecCoefVar_MarMay)
plot( aggregate(PrecCV_MAM~Year,FUN=mean,data=dataAll[dataAll$Year<2014,])[[2]],  aggregate(PrecCoefVar_MarMay~Year,FUN=mean,data=itsdata)[[2]])
cor.test( aggregate(PrecCV_MAM~Year,FUN=mean,data=dataAll[dataAll$Year<2014,])[[2]],  aggregate(PrecCoefVar_MarMay~Year,FUN=mean,data=itsdata)[[2]])

summary(dataAll$PrecCV_OND)
summary(itsdata$PrecCoefVar_OctDec)
plot( aggregate(PrecCV_OND~Year,FUN=mean,data=dataAll[dataAll$Year<2014,])[[2]],  aggregate(PrecCoefVar_OctDec~Year,FUN=mean,data=itsdata)[[2]])
cor.test( aggregate(PrecCV_OND~Year,FUN=mean,data=dataAll[dataAll$Year<2014,])[[2]],  aggregate(PrecCoefVar_OctDec~Year,FUN=mean,data=itsdata)[[2]])

summary(dataAll$PrecTot_OND)
summary(itsdata$SeasRain_OctDec)
plot( aggregate(PrecTot_OND~Year,FUN=mean,data=dataAll[dataAll$Year<2014,])[[2]],  aggregate(SeasRain_OctDec~Year,FUN=mean,data=itsdata)[[2]])
cor.test( aggregate(PrecTot_OND~Year,FUN=mean,data=dataAll[dataAll$Year<2014,])[[2]],  aggregate(SeasRain_OctDec~Year,FUN=mean,data=itsdata)[[2]])

summary(dataAll$PrecTot_MAM)
summary(itsdata$SeasRain_MarMay)
plot( aggregate(PrecTot_MAM~Year,FUN=mean,data=dataAll[dataAll$Year<2014,])[[2]],  aggregate(SeasRain_MarMay~Year,FUN=mean,data=itsdata)[[2]])
cor.test( aggregate(PrecTot_MAM~Year,FUN=mean,data=dataAll[dataAll$Year<2014,])[[2]],  aggregate(SeasRain_MarMay~Year,FUN=mean,data=itsdata)[[2]])

summary(dataAll$TempSD_OND)
summary(itsdata$SDTemp_OctDec)
plot( aggregate(TempSD_OND~Year,FUN=mean,data=dataAll[dataAll$Year<2014,])[[2]],  aggregate(SDTemp_OctDec~Year,FUN=mean,data=itsdata)[[2]])
cor.test( aggregate(TempSD_OND~Year,FUN=mean,data=dataAll[dataAll$Year<2014,])[[2]],  aggregate(SDTemp_OctDec~Year,FUN=mean,data=itsdata)[[2]])


summary(dataAll$TempDD_MAM)
summary(itsdata$Cum_DD_MarMay)
plot( aggregate(TempDD_MAM~Year,FUN=mean,data=dataAll[dataAll$Year<2014,])[[2]],  aggregate(Cum_DD_MarMay~Year,FUN=mean,data=itsdata)[[2]])
cor.test( aggregate(TempDD_MAM~Year,FUN=mean,data=dataAll[dataAll$Year<2014,])[[2]],  aggregate(Cum_DD_MarMay~Year,FUN=mean,data=itsdata)[[2]])


summary(dataAll$Spell10_OND)
summary(itsdata$DrSpell10_OctDec)
plot( aggregate(Spell10_OND~Year,FUN=mean,data=dataAll[dataAll$Year<2014,])[[2]],  aggregate(DrSpell10_OctDec~Year,FUN=mean,data=itsdata)[[2]])
cor.test( aggregate(Spell10_OND~Year,FUN=mean,data=dataAll[dataAll$Year<2014,])[[2]],  aggregate(DrSpell10_OctDec~Year,FUN=mean,data=itsdata)[[2]])


summary(dataAll$DrySpell_MAM)
summary(itsdata$DrySpell_MarMay)
plot( aggregate(DrySpell_MAM~Year,FUN=mean,data=dataAll[dataAll$Year<2014,])[[2]],  aggregate(DrySpell_MarMay~Year,FUN=mean,data=itsdata)[[2]]) 
# too many NAs in the dataAll. but probably good..

cor.test( aggregate(DrySpell_MAM~Year,FUN=mean,data=dataAll[dataAll$Year<2014,])[[2]],  aggregate(DrySpell_MarMay~Year,FUN=mean,data=itsdata)[[2]])


summary(dataAll$TempHW_MAM)
summary(itsdata$HeatWDays_MarMay)
plot( aggregate(TempHW_MAM~Year,FUN=mean,data=dataAll[dataAll$Year<2014,])[[2]],  aggregate(HeatWDays_MarMay~Year,FUN=mean,data=itsdata)[[2]])
cor.test( aggregate(TempHW_MAM~Year,FUN=mean,data=dataAll[dataAll$Year<2014,])[[2]],  aggregate(HeatWDays_MarMay~Year,FUN=mean,data=itsdata)[[2]])
## v. good

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# now just tiny bit testing if I loaded it correctly
names(itsdata)
names(itsdata)[5:43]
itsdata[itsdata$ID1==1,c(1:4,44)] # Yield= no.49

n<-sample(unique(itsdata$ID1),1)
n
itsdata[itsdata$ID1==n,c(1:4,34:44)]

# cooool, all checked and good 25.10.2018

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

rm(list=setdiff(ls(), c("itsdata")))

# save.image("Main/itsdata.RData")