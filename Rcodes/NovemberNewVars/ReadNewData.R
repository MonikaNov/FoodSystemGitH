rm(list=ls())

library('dplyr')
library('purrr')
load("dataFS/Main/DaTS.RData");testSc<-ScaledTS; testD<-DaTS
library(dplyr); library(tseries); library(plm); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

floods<-read.csv("../FoodSystemGitH/dataFS/climNov2/Flood.csv",na.strings="-999",dec=".")
summary(floods);head(floods)

summary(lm(cum_90_MarMay~cum_95_OctDec,data=floods)) # test regression: are there some missings or not?
nobs(lm(cum_90_MarMay~cum_95_OctDec,data=floods)) 

plot(days_90_MarMay~days_90_MarAug,data=floods)
# seems good

monthlyRa<-read.csv("../FoodSystemGitH/dataFS/climNov2/Monthly_cumul.csv",na.strings="-999",dec=".")
summary(monthlyRa); head(monthlyRa)
nobs(lm(cum_Oct~cum_Jun,data=monthlyRa)) ; summary(lm(cum_Oct~cum_Jun,data=monthlyRa)) #testing regression. Test sems to be sucsessful

names(monthlyRa)
#more checking:
summary(DaTS["SeasRain_MarMay"]); summary(monthlyRa["cum_Mar"])
summary(DaTS["SeasRain_OctDec"]); summary(monthlyRa["cum_Oct"])

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#  now I have to match it with the current main dataset 

names(floods)[1]<-"Year"
names(monthlyRa)[1]<-"Year"

floods2<-floods; floods<-floods2[-c(3,4)]; floods$Year<-as.factor(floods$Year)
monthlyRa2<-monthlyRa; monthlyRa<-monthlyRa2[-c(3,4)]; monthlyRa$Year<-as.factor(monthlyRa$Year)



DaTS$code<-as.factor(DaTS$code)
test1<-DaTS
DaTS<-merge(DaTS,floods, all.x=TRUE); DaTS<-merge(DaTS,monthlyRa, all.x=TRUE)
DaTS<-DaTS[with(DaTS,order(ID1,Year) ),]
# to fix malformed factors, I need to first convert factors to characters and then back to factors:
test99<-DaTS;                  i<-as.numeric(which(sapply(DaTS,function(x) is.factor(x)==TRUE)))
DaTS[as.numeric(which(sapply(DaTS,function(x) is.factor(x)==TRUE)))]<-lapply(DaTS[as.numeric(which(sapply(DaTS,function(x) is.factor(x)==TRUE)))],as.character)
DaTS[i]<-lapply(DaTS[i],as.factor)
# and test:
all.equal(test99[-i], DaTS[-i]  )
all_equal(test1,DaTS[1:124]);          all.equal(test1[-(2:3)],DaTS[c(1,4:124)],check.attributes=FALSE)

summary(DaTS); sum(complete.cases(DaTS))

all.equal(test99[i],DaTS[i],check.attributes=FALSE)


#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# other checking: 
sum(DaTS$cum_95_MarMay<DaTS$cum_99_MarMay)
sum(DaTS$cum_95_MarMay>=DaTS$cum_99_MarMay)
sum(DaTS$cum_90_MarMay>=DaTS$cum_95_MarMay)
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#------------      test in a model...............

testRe<-lm(Yield~.,data=DaTS[c(1,3,5:76,81:145)])
summary(testRe)
nobs(testRe)
DaTS<-pdata.frame(DaTS,index=c("ID1","Year"))

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#        ok, I still need to get lags of OND seasons...

test1<-DaTS
lagI<-c(137:142,145)
lagged<-data.frame(sapply(lagI ,function(x)  lag(DaTS[,x],1)  ))
names(lagged)<-paste0( names(DaTS[,lagI]),"_L1") # and again. Malformed factor

DaTS<-cbind.data.frame(DaTS,lagged)
i<-grep("_L1",names(DaTS))
DaTS[DaTS$Year==1981,i]<-NA

test99<-DaTS;                  i<-as.numeric(which(sapply(DaTS,function(x) is.factor(x)==TRUE)))
DaTS[as.numeric(which(sapply(DaTS,function(x) is.factor(x)==TRUE)))]<-lapply(DaTS[as.numeric(which(sapply(DaTS,function(x) is.factor(x)==TRUE)))],as.character)
DaTS[i]<-lapply(DaTS[i],as.factor)
# and test:
all.equal(test99[-i], DaTS[-i]  )

DaTS<-pdata.frame(DaTS,index=c("ID1","Year"))


# checking:
all.equal(test1,DaTS[1:145],check.attributes=FALSE)
ra<-sample(1:(nrow(DaTS)-10),1)
ra; DaTS[ra:(ra+10),c(1:4,lagI,146:152)] # checked 22.11.2018
test1<-DaTS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# now I have to sort out the seasons: ASAL MAM and OND_L1, non-ASAL MAMJJA

DaTS$cum90[DaTS$ASAL==TRUE]<-(DaTS$cum_90_MarMay[DaTS$ASAL==TRUE]+DaTS$cum_90_OctDec_L1[DaTS$ASAL==TRUE])
DaTS$cum95[DaTS$ASAL==TRUE]<-(DaTS$cum_95_MarMay[DaTS$ASAL==TRUE]+DaTS$cum_95_OctDec_L1[DaTS$ASAL==TRUE])
DaTS$cum99[DaTS$ASAL==TRUE]<-(DaTS$cum_99_MarMay[DaTS$ASAL==TRUE]+DaTS$cum_99_OctDec_L1[DaTS$ASAL==TRUE])
DaTS$days90[DaTS$ASAL==TRUE]<-(DaTS$days_90_MarMay[DaTS$ASAL==TRUE]+DaTS$days_90_OctDec_L1[DaTS$ASAL==TRUE])
DaTS$days95[DaTS$ASAL==TRUE]<-(DaTS$days_95_MarMay[DaTS$ASAL==TRUE]+DaTS$days_95_OctDec_L1[DaTS$ASAL==TRUE])
DaTS$days99[DaTS$ASAL==TRUE]<-(DaTS$days_99_MarMay[DaTS$ASAL==TRUE]+DaTS$days_99_OctDec_L1[DaTS$ASAL==TRUE])

DaTS$cum90[DaTS$ASAL==FALSE]<-DaTS$cum_90_MarAug[DaTS$ASAL==FALSE]
DaTS$cum95[DaTS$ASAL==FALSE]<-DaTS$cum_95_MarAug[DaTS$ASAL==FALSE]
DaTS$cum99[DaTS$ASAL==FALSE]<-DaTS$cum_99_MarAug[DaTS$ASAL==FALSE]
DaTS$days90[DaTS$ASAL==FALSE]<-DaTS$days_90_MarAug[DaTS$ASAL==FALSE]
DaTS$days95[DaTS$ASAL==FALSE]<-DaTS$days_95_MarAug[DaTS$ASAL==FALSE]
DaTS$days99[DaTS$ASAL==FALSE]<-DaTS$days_99_MarAug[DaTS$ASAL==FALSE]

DaTS$PrecFirstM[DaTS$ASAL==TRUE]<-( (DaTS$cum_Mar[DaTS$ASAL==TRUE]+DaTS$cum_Oct_L1[DaTS$ASAL==TRUE]) /2)
DaTS$PrecFirstM[DaTS$ASAL==FALSE]<-DaTS$cum_Mar[DaTS$ASAL==FALSE]

summary(DaTS)
all.equal(test1,DaTS[1:152])  # goood
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# a bit more checking: now I will just have a look if the newly created transformed variables look somehow similar to those from Guigma and how they should look like

sum(DaTS$days99[!DaTS$Year==1981]>DaTS$days90[!DaTS$Year==1981])
sum(DaTS$days99[!DaTS$Year==1981]>DaTS$days95[!DaTS$Year==1981])
sum(DaTS$days95[!DaTS$Year==1981]>DaTS$days90[!DaTS$Year==1981])

sum(DaTS$cum99[!DaTS$Year==1981]>DaTS$cum90[!DaTS$Year==1981])
sum(DaTS$cum99[!DaTS$Year==1981]>DaTS$cum95[!DaTS$Year==1981])
sum(DaTS$cum95[!DaTS$Year==1981]>DaTS$cum90[!DaTS$Year==1981])

plot(DaTS$cum95,DaTS$cum_95_MarAug)
plot(DaTS$cum95,DaTS$cum_95_MarMay)
plot(DaTS$cum95,DaTS$cum_95_OctDec)
plot(DaTS$cum95,DaTS$cum_95_OctDec_L1)

plot(DaTS$days95,DaTS$days_95_MarAug)
plot(DaTS$days95,DaTS$days_95_MarMay)
plot(DaTS$days95,DaTS$days_95_OctDec)
plot(DaTS$days95,DaTS$days_95_OctDec_L1)  
# ok, all seems to be good. I may have to do some more 1 by 1 testing through...AND THEN SCALE IT !!!!
DaTS$ID1<-as.numeric(as.character(DaTS$ID1))

DaTS<-DaTS[with(DaTS,order(ID1,Year)),]
testD<-testD[with(testD,order(ID1,Year)),]

DaTS$ID1<-as.factor(DaTS$ID1)
all.equal(DaTS[4:124],testD[4:124],check.attributes=FALSE)
all.equal(DaTS[4:124],testD[4:124],check.attributes=TRUE)

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# now some more checks..before the scaling and more checks.
ran<-sample(1:(nrow(DaTS)-5),1  );ran
DaTS[ran:(ran+5),] 

# OK, CHECKED FOR NOW.. 

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# now I need scale them..
rm(ScaledTS)
ScaledTS<-DaTS
ScaledTS[,c(5:76,81:83,85:159)]<-scale(DaTS[,c(5:76,81:83,85:159)])
ScaledTS$Yield0<-DaTS$Yield

# ~~~~~  ~~  ~~ ~~ ~~ ~~ ~   and check if scaling correctly:  ~~~~~  ~~  ~~ ~~ ~~ ~~ ~

all.equal( names(DaTS)[c(5:76,81:83,85:159)], names(ScaledTS)[c(5:76,81:83,85:159)])
all.equal(ScaledTS[4:124],testSc[4:124],check.attributes=FALSE)
all.equal(ScaledTS[4:124],testSc[4:124],check.attributes=TRUE)


mapply(cor.test,ScaledTS[,c(5:76,81:83,85:152)],DaTS[,c(5:76,81:83,85:152)] )
cor.test(ScaledTS$Spell,DaTS$Spell) ; plot(ScaledTS$Spell,DaTS$Spell) 
cor.test(ScaledTS$DrySpell4_Sep_L1,DaTS$DrySpell4_Sep_L1) ; plot(ScaledTS$DrySpell4_Sep_L1,DaTS$DrySpell4_Sep_L1)
cor.test(ScaledTS$HeatWDays_Sep,DaTS$HeatWDays_Sep); plot(ScaledTS$HeatWDays_Sep,DaTS$HeatWDays_Sep)
cor.test(ScaledTS$PrecFirstM,DaTS$Prec2m);plot(ScaledTS$PrecFirstM,DaTS$Prec2m)
cor.test(ScaledTS$days95,DaTS$days95);plot(ScaledTS$days95,DaTS$days95)
cor.test(ScaledTS$days99,DaTS$days95);plot(ScaledTS$days99,DaTS$days95)

summary(ScaledTS)
lapply(ScaledTS[,c(5:76,81:83,85:152)],function(x) mean(x,na.rm=TRUE))
summary(sapply(ScaledTS[,c(5:76,81:83,85:152)],function(x) mean(x,na.rm=TRUE)))
plot(sapply(ScaledTS[,c(5:76,81:83,85:152)],function(x) mean(x,na.rm=TRUE)))


sum(complete.cases(ScaledTS)); sum(complete.cases(ScaledTS))
sum(complete.cases(ScaledTS))
sum(!complete.cases(ScaledTS))

sum(  !complete.cases(ScaledTS) &is.na(ScaledTS$Yield)==FALSE )
which(  !complete.cases(ScaledTS) &is.na(ScaledTS$Yield)==FALSE ) # PERFECT

sum(  !complete.cases(ScaledTS) &is.na(ScaledTS$Yield)==TRUE )

all.equal(ScaledTS[,-c(5:76,81:83,85:160)],DaTS[,-c(5:76,81:83,85:159)] ) # GROOT
all.equal(ScaledTS$Yield0,DaTS$Yield)
plot(ScaledTS$Yield0,DaTS$Yield)

# seems very well here..