# rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

library('dplyr')
library('tseries')
library(plm)
library(lme4)
library(lattice)
library(car)
library(lmerTest)
library(optimx)
library(itertools)
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

load("Main/itsdata.RData")

names(itsdata)

# well, I need to create the variable which will account for asal and non-asal. For this I need the lag of OND. so I need to create laggs..

itsdataTS<-pdata.frame(itsdata,index=c("ID1","Year"))


lagI<-grep("*OctDec",names(itsdataTS))
lagged<-data.frame(sapply(lagI ,function(x)  lag(itsdataTS[,x],1)  ))
names(lagged)<-paste0( names(itsdataTS[,lagI]),"_L1")
rm(itsdataTS)
itsdataTS<-cbind.data.frame(itsdata,lagged)
itsdataTS<-pdata.frame(itsdataTS,index=c("ID1","Year"))
itsdataTS[itsdataTS$Year==1981, 52:64]<-NA

all.equal(itsdataTS[1:32,lagI],itsdataTS[2:33,52:64],check.attributes=FALSE) # seems very good

q<-0

all.equal(itsdataTS[( ( 1+(q*33) ):  (32 + (q*33) )) ,lagI],itsdataTS[( ( 2+(q*33) ):  (33 + (q*33) ) ),52:64],check.attributes=FALSE) 


sapply(0:45,  function(q) all.equal(itsdataTS[( ( 1+(q*33) ):  (32 + (q*33) )) ,lagI],itsdataTS[( ( 2+(q*33) ):  (33 + (q*33) ) ),52:64],check.attributes=FALSE)) # Verry good.


write.csv(itsdataTS,"itsdataTS.csv")  # COOOL, all the checks seems good by now...

#  ----------  --- now I have to create the seasonal aggregates  -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

rm(isdata)
isdata<-itsdata[c(2:3,1,45,49,4,51,48)]
names(isdata)
isdataTS<-pdata.frame(isdata,index=c("ID1","Year"))
isdataTS$AvgTemp<-NA
isdataTS$SDTemp<-NA
isdataTS$DDays<-NA
isdataTS$HWDays<-NA
isdataTS$MaxT<-NA
isdataTS$SeasPr<-NA
isdataTS$Prec2m<-NA
isdataTS$SDPrec<-NA
isdataTS$CVPrec<-NA
isdataTS$Spell<-NA
isdataTS$Spell10<-NA
isdataTS$Spell20<-NA
isdataTS$MaxP<-NA
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

isdataTS$AvgTemp[isdataTS$ASAL==TRUE]<-( (itsdataTS$AvgTemp_MarMay[isdataTS$ASAL==TRUE]+itsdataTS$AvgTemp_OctDec_L1[isdataTS$ASAL==TRUE]) /2)
isdataTS$SDTemp[isdataTS$ASAL==TRUE]<-( (itsdataTS$SDTemp_MarMay[isdataTS$ASAL==TRUE]+itsdataTS$SDTemp_OctDec_L1[isdataTS$ASAL==TRUE]) /2)
isdataTS$DDays[isdataTS$ASAL==TRUE]<-( (itsdataTS$Cum_DD_MarMay[isdataTS$ASAL==TRUE]+itsdataTS$Cum_DD_OctDec_L1[isdataTS$ASAL==TRUE]) /2)
isdataTS$HWDays[isdataTS$ASAL==TRUE]<-( (itsdataTS$HeatWDays_MarMay[isdataTS$ASAL==TRUE]+itsdataTS$HeatWDays_OctDec_L1[isdataTS$ASAL==TRUE]) /2)
isdataTS$MaxT[isdataTS$ASAL==TRUE]<-( pmax(itsdataTS$MaxTemp_MarMay[isdataTS$ASAL==TRUE],itsdataTS$MaxTemp_OctDec_L1[isdataTS$ASAL==TRUE]))

isdataTS$SeasPr[isdataTS$ASAL==TRUE]<-((itsdataTS$SeasRain_MarMay[isdataTS$ASAL==TRUE]+itsdataTS$SeasRain_OctDec_L1[isdataTS$ASAL==TRUE])/2)
isdataTS$Prec2m[isdataTS$ASAL==TRUE]<-((itsdataTS$Prec2Months_MarMay[isdataTS$ASAL==TRUE]+itsdataTS$Prec2Months_OctDec_L1[isdataTS$ASAL==TRUE])/2)
isdataTS$SDPrec[isdataTS$ASAL==TRUE]<-((itsdataTS$PrecStDev_MarMay[isdataTS$ASAL==TRUE]+itsdataTS$PrecStDev_OctDec_L1[isdataTS$ASAL==TRUE])/2)
isdataTS$CVPrec[isdataTS$ASAL==TRUE]<-((itsdataTS$PrecCoefVar_MarMay[isdataTS$ASAL==TRUE]+itsdataTS$PrecCoefVar_OctDec_L1[isdataTS$ASAL==TRUE])/2)
isdataTS$Spell[isdataTS$ASAL==TRUE]<-((itsdataTS$DrySpell_MarMay[isdataTS$ASAL==TRUE]+itsdataTS$DrySpell_OctDec_L1[isdataTS$ASAL==TRUE])/2)
isdataTS$Spell10[isdataTS$ASAL==TRUE]<-((itsdataTS$DrSpell10_MarMay[isdataTS$ASAL==TRUE]+itsdataTS$DrSpell10_OctDec_L1[isdataTS$ASAL==TRUE])/2)
isdataTS$Spell20[isdataTS$ASAL==TRUE]<-((itsdataTS$DrSpell20_MarMay[isdataTS$ASAL==TRUE]+itsdataTS$DrSpell20_OctDec_L1[isdataTS$ASAL==TRUE])/2)
isdataTS$MaxP[isdataTS$ASAL==TRUE]<-(pmax(itsdataTS$MaxRain_MarMay[isdataTS$ASAL==TRUE]+itsdataTS$MaxRain_OctDec_L1[isdataTS$ASAL==TRUE]))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
isdataTS$AvgTemp[isdataTS$ASAL==FALSE]<-itsdataTS$AvgTemp_MarAug[isdataTS$ASAL==FALSE]
isdataTS$SDTemp[isdataTS$ASAL==FALSE]<-itsdataTS$SDTemp_MarAug[isdataTS$ASAL==FALSE]
isdataTS$DDays[isdataTS$ASAL==FALSE]<-itsdataTS$Cum_DD_MarAug[isdataTS$ASAL==FALSE]
isdataTS$HWDays[isdataTS$ASAL==FALSE]<-itsdataTS$HeatWDays_MarAug[isdataTS$ASAL==FALSE]
isdataTS$MaxT[isdataTS$ASAL==FALSE]<-itsdataTS$MaxTemp_MarAug[isdataTS$ASAL==FALSE]

isdataTS$SeasPr[isdataTS$ASAL==FALSE]<-itsdataTS$SeasRain_MarAug[isdataTS$ASAL==FALSE]
isdataTS$Prec2m[isdataTS$ASAL==FALSE]<-itsdataTS$Prec2Months_MarAug[isdataTS$ASAL==FALSE]
isdataTS$SDPrec[isdataTS$ASAL==FALSE]<-itsdataTS$PrecStDev_MarAug[isdataTS$ASAL==FALSE]
isdataTS$CVPrec[isdataTS$ASAL==FALSE]<-itsdataTS$PrecCoefVar_MarAug[isdataTS$ASAL==FALSE]
isdataTS$Spell[isdataTS$ASAL==FALSE]<-itsdataTS$DrySpell_MarAug[isdataTS$ASAL==FALSE]
isdataTS$Spell10[isdataTS$ASAL==FALSE]<-itsdataTS$DrSpell10_MarAug[isdataTS$ASAL==FALSE]
isdataTS$Spell20[isdataTS$ASAL==FALSE]<-itsdataTS$DrSpell20_MarAug[isdataTS$ASAL==FALSE]
isdataTS$MaxP[isdataTS$ASAL==FALSE]<-itsdataTS$MaxRain_MarAug[isdataTS$ASAL==FALSE]
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


summary(isdataTS)
summary(isdataTS[9:13])
summary(itsdataTS[5:9]);            summary(itsdataTS[10:14]) ;                          summary(itsdataTS[15:19])

summary(isdataTS[14:21])
summary(itsdataTS[20:27]);          summary(itsdataTS[28:35]);             summary(itsdataTS[36:43]);          

#ok, looks litle bit similar-ish
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# now I have to scale it..

isdataScTS<-isdataTS
isdataScTS[,c(5, 8:21)]<-scale(isdataTS[,c(5, 8:21)])

all.equal(isdataScTS[c(1:4,6,7)],isdataTS[c(1:4,6,7)]) # checking, seems good...

plot(isdataTS[[5]],isdataScTS[[5]])

plot(isdataTS[[5]],type='l',ylim=c(-3,4))
lines(isdataScTS[[5]],col=2)

i<-7

i<-i+1;i
plot(isdataTS[[i]],isdataScTS[[i]])
# ok, scaling seems to be allright
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# ok, nice..seems good and checked..

# save.image("~/foodSystems/Rcodes/octoberNew/dataManip.RData")

rm(list=setdiff(ls(),c("isdataTS","isdataScTS")))
#          save.image("Main/isdataTS.RData")