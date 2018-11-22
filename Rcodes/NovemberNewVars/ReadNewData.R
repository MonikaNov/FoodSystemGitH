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
names(lagged)<-paste0( names(DaTS[,lagI]),"_L1")
DaTS<-cbind.data.frame(DaTS,lagged)
DaTS<-pdata.frame(DaTS,index=c("ID1","Year"))
i<-grep("_L1",names(DaTS))
DaTS[DaTS$Year==1981,i]<-NA

# checking:
all.equal(test1,DaTS[1:145],check.attributes=FALSE)
ra<-sample(1:(nrow(DaTS)-10),1)
ra; DaTS[ra:(ra+10),c(1:4,lagI,146:152)] # checked 22.11.2018

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# now I have to sort out the seasons: ASAL MAM and OND_L1, non-ASAL MAMJJA

DaTS$cum90[DaTS$ASAL==TRUE]<-( (DaTS$AvgTemp_MarMay[DaTS$ASAL==TRUE]+DaTS$AvgTemp_OctDec_L1[DaTS$ASAL==TRUE]) /2)