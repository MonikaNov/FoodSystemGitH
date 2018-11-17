rm(list=ls())

library('dplyr')
library('purrr')
library("reshape")

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

weath<-read.csv("dataFS/climateNov/Seasonally.csv",na.strings="-999",dec=".")
summary(weath)
testRe<-lm(AvgTemp_MarMay~.,data=weath)
summary(testRe)
nobs(testRe)  # grood, all loaded
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------
# dry spell 4 in a separate file
weath<-merge(weath,read.csv("dataFS/climateNov/Dry_spell4_seasonsM.csv",na.strings="-999",dec=".") )
weath<-weath[with(weath,order(code,year)),]
testRe<-lm(AvgTemp_MarMay~.,data=weath)
summary(testRe)
nobs(testRe)
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------
# monthly also in separate file

weath<-merge(weath,read.csv("dataFS/climateNov/MonthlyM.csv",na.strings="-999",dec=".") )
weath<-weath[with(weath,order(code,year)),]

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------
# some more testing,... are the new data the same as the previous ones??
all.equal(names(weath)[1:42],names(weather)[-3])

all_equal(weath[(weath$Year %in% 1981:2012),20:42],weather[(weather$year %in% 1981:2012),21:43])
# just out of interes, why do they differ??
xx<-weath[(weath$year %in% 1981:2012),1:42]
yy<-weather[(weather$year %in% 1981:2012),-3]

setdiff(weath$code,weather$code)
setdiff(weather$code,weath$code)
#....................................................................................................................................................................................
# ok, seems good. now I will have to match it with yield

# but first merge with county codes
IDdict<-read.csv("dataFS/Main/IDdict.csv")
weath<-merge(weath,IDdict, all.x=TRUE)

load("dataFS/Main/CrMaize15.RData") 
rm(CrMaize34)
CrMaize34<-subset(CrMaize15,Year %in% (1981:2014),select=c("Admin1","county","Admin2","Year","Area","Yield","MT","ID","west1"))
names(CrMaize34)[grep("ID",names(CrMaize34))]<-"ID1"
names(weath)[grep("year",names(weath))]<-"Year"

Da<-merge(weath,CrMaize34,all.x=TRUE)
Da<-Da[with(Da,order(ID1,Year)),]
weath<-weath[with(weath,order(ID1,Year)),]  

#........   .... .   .......    ..........           .  . . . . .

# and checking

all.equal(Da[4:77],weath[3:76],check.attributes=FALSE)   #  so far so good......

Da<-subset(Da,Year %in% 1981:2014)

x<-sample(1:(1702-5),1)
x
Da[(1210):(1225),c(1:4,79,81:83)]
#------------      test in a model...............

testRe<-lm(Yield~.,data=Da[c(1:43,49,79,82)])
summary(testRe)
nobs(testRe)

rm(list=setdiff(ls(), c("Da")))

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#        ok, I still need to get lags of OND seasons...

DaTS<-pdata.frame(Da,index=c("ID1","Year"))
lagI<-grep("Sep|Oct|Nov|Dec|OND",names(DaTS))
lagged<-data.frame(sapply(lagI ,function(x)  lag(DaTS[,x],1)  ))
names(lagged)<-paste0( names(DaTS[,lagI]),"_L1")
rm(DaTS)
DaTS<-cbind.data.frame(Da,lagged)
DaTS<-pdata.frame(DaTS,index=c("ID1","Year"))
DaTS[DaTS$Year==1981, 85:110]<-NA

all.equal(DaTS[1:33,lagI],DaTS[2:34,85:110],check.attributes=FALSE) # seems very good
q<-0
all.equal(DaTS[( ( 1+(q*34) ):  (33 + (q*34) )) ,lagI],DaTS[( ( 2+(q*34) ):  (34 + (q*34) ) ),85:110],check.attributes=FALSE) 
sapply(0:45,  function(q) all.equal(DaTS[( ( 1+(q*34) ):  (33 + (q*34) )) ,lagI],DaTS[( ( 2+(q*34) ):  (34 + (q*34) ) ),85:110],check.attributes=FALSE)) # Verry good.

write.csv(DaTS, "climateNov/DaTS.csv")

# fine, checked and its ok..
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

#  ----------  --- now I have to create the seasonal aggregates  -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

DaTS$AvgTemp<-NA
DaTS$SDTemp<-NA
DaTS$DDays<-NA
DaTS$HWDays<-NA
DaTS$MaxT<-NA

DaTS$SeasPr<-NA
DaTS$Prec2m<-NA
DaTS$SDPrec<-NA
DaTS$CVPrec<-NA
DaTS$Spell<-NA
DaTS$Spell4<-NA
DaTS$Spell10<-NA
DaTS$Spell20<-NA
DaTS$MaxP<-NA
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

DaTS$AvgTemp[DaTS$ASAL==TRUE]<-( (DaTS$AvgTemp_MarMay[DaTS$ASAL==TRUE]+DaTS$AvgTemp_OctDec_L1[DaTS$ASAL==TRUE]) /2)
DaTS$SDTemp[DaTS$ASAL==TRUE]<-( (DaTS$SDTemp_MarMay[DaTS$ASAL==TRUE]+DaTS$SDTemp_OctDec_L1[DaTS$ASAL==TRUE]) /2)
DaTS$DDays[DaTS$ASAL==TRUE]<-( DaTS$Cum_DD_MarMay[DaTS$ASAL==TRUE]+DaTS$Cum_DD_OctDec_L1[DaTS$ASAL==TRUE])
DaTS$HWDays[DaTS$ASAL==TRUE]<-(DaTS$HeatWDays_MarMay[DaTS$ASAL==TRUE]+DaTS$HeatWDays_OctDec_L1[DaTS$ASAL==TRUE])
DaTS$MaxT[DaTS$ASAL==TRUE]<-( pmax(DaTS$MaxTemp_MarMay[DaTS$ASAL==TRUE],DaTS$MaxTemp_OctDec_L1[DaTS$ASAL==TRUE]))

DaTS$SeasPr[DaTS$ASAL==TRUE]<-(DaTS$SeasRain_MarMay[DaTS$ASAL==TRUE]+DaTS$SeasRain_OctDec_L1[DaTS$ASAL==TRUE])
DaTS$Prec2m[DaTS$ASAL==TRUE]<-((DaTS$Prec2Months_MarMay[DaTS$ASAL==TRUE]+DaTS$Prec2Months_OctDec_L1[DaTS$ASAL==TRUE])/2)
DaTS$SDPrec[DaTS$ASAL==TRUE]<-((DaTS$PrecStDev_MarMay[DaTS$ASAL==TRUE]+DaTS$PrecStDev_OctDec_L1[DaTS$ASAL==TRUE])/2)
DaTS$CVPrec[DaTS$ASAL==TRUE]<-((DaTS$PrecCoefVar_MarMay[DaTS$ASAL==TRUE]+DaTS$PrecCoefVar_OctDec_L1[DaTS$ASAL==TRUE])/2)
DaTS$Spell[DaTS$ASAL==TRUE]<-((DaTS$DrySpell_MarMay[DaTS$ASAL==TRUE]+DaTS$DrySpell_OctDec_L1[DaTS$ASAL==TRUE])/2)
DaTS$Spell4[DaTS$ASAL==TRUE]<-(DaTS$DrySpell4_MAM[DaTS$ASAL==TRUE]+DaTS$DrySpell4_OND_L1[DaTS$ASAL==TRUE])
DaTS$Spell10[DaTS$ASAL==TRUE]<-(DaTS$DrSpell10_MarMay[DaTS$ASAL==TRUE]+DaTS$DrSpell10_OctDec_L1[DaTS$ASAL==TRUE])
DaTS$Spell20[DaTS$ASAL==TRUE]<-(DaTS$DrSpell20_MarMay[DaTS$ASAL==TRUE]+DaTS$DrSpell20_OctDec_L1[DaTS$ASAL==TRUE])
DaTS$MaxP[DaTS$ASAL==TRUE]<-pmax(DaTS$MaxRain_MarMay[DaTS$ASAL==TRUE],DaTS$MaxRain_OctDec_L1[DaTS$ASAL==TRUE])
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
DaTS$AvgTemp[DaTS$ASAL==FALSE]<-DaTS$AvgTemp_MarAug[DaTS$ASAL==FALSE]
DaTS$SDTemp[DaTS$ASAL==FALSE]<-DaTS$SDTemp_MarAug[DaTS$ASAL==FALSE]
DaTS$DDays[DaTS$ASAL==FALSE]<-DaTS$Cum_DD_MarAug[DaTS$ASAL==FALSE]
DaTS$HWDays[DaTS$ASAL==FALSE]<-DaTS$HeatWDays_MarAug[DaTS$ASAL==FALSE]
DaTS$MaxT[DaTS$ASAL==FALSE]<-DaTS$MaxTemp_MarAug[DaTS$ASAL==FALSE]

DaTS$SeasPr[DaTS$ASAL==FALSE]<-DaTS$SeasRain_MarAug[DaTS$ASAL==FALSE]
DaTS$Prec2m[DaTS$ASAL==FALSE]<-DaTS$Prec2Months_MarAug[DaTS$ASAL==FALSE]
DaTS$SDPrec[DaTS$ASAL==FALSE]<-DaTS$PrecStDev_MarAug[DaTS$ASAL==FALSE]
DaTS$CVPrec[DaTS$ASAL==FALSE]<-DaTS$PrecCoefVar_MarAug[DaTS$ASAL==FALSE]
DaTS$Spell[DaTS$ASAL==FALSE]<-DaTS$DrySpell_MarAug[DaTS$ASAL==FALSE]
DaTS$Spell4[DaTS$ASAL==FALSE]<-DaTS$DrySpell4_MAMJJA[DaTS$ASAL==FALSE]
DaTS$Spell10[DaTS$ASAL==FALSE]<-DaTS$DrSpell10_MarAug[DaTS$ASAL==FALSE]
DaTS$Spell20[DaTS$ASAL==FALSE]<-DaTS$DrSpell20_MarAug[DaTS$ASAL==FALSE]
DaTS$MaxP[DaTS$ASAL==FALSE]<-DaTS$MaxRain_MarAug[DaTS$ASAL==FALSE]

DaTS[(DaTS$Year==1981 & DaTS$ASAL==TRUE), 85:110]<-NA

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ok, now I have to check everything
summary(DaTS)
names(DaTS)[!complete.cases(t(DaTS))]

i<-sample(1:(nrow(DaTS)-5),1)    
i
DaTS[i:(i+5),c(1,2,4,5:9,12,13,85:89,111:115)]

# cool, well done, all has been checked 8.11.2018
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# now I need scale them..

isdataScTS<-isdataTS
isdataScTS[,c(5, 8:21)]<-scale(isdataTS[,c(5, 8:21)])

rm(ScaledTS)
ScaledTS<-DaTS
ScaledTS[,c(5:76,81:83,85:124)]<-scale(DaTS[,c(5:76,81:83,85:124)])
ScaledTS$Yield0<-DaTS$Yield

# and check if scaling correctly

names(DaTS)[c(5:76,81:83,85:124)]

mapply(cor.test,ScaledTS[,c(5:76,81:83,85:124)],DaTS[,c(5:76,81:83,85:124)] )
cor.test(ScaledTS$Prec2m,DaTS$Prec2m) 
summary(ScaledTS)
sum(complete.cases(ScaledTS))
sum(complete.cases(ScaledTS))
sum(!complete.cases(ScaledTS))

sum(  !complete.cases(ScaledTS) &is.na(ScaledTS$Yield)==FALSE )

sum(  !complete.cases(ScaledTS) &is.na(ScaledTS$Yield)==TRUE )

all.equal(ScaledTS[,-c(5:76,81:83,85:125)],DaTS[,-c(5:76,81:83,85:124)] ) # seems ok
all.equal(ScaledTS$Yield0,DaTS$Yield)
plot(ScaledTS$Yield0,DaTS$Yield)
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
remove(list=setdiff(ls(),c("DaTS","Da")))

# save.image("Main/Da.RData")

remove(list=setdiff(ls(),c("DaTS","ScaledTS")))
# save.image("Main/DaTS.RData")
# save.image("../Rcodes/November/DaTS.RData")