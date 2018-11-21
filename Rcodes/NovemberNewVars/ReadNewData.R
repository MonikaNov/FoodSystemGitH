rm(list=ls())

library('dplyr')
library('purrr')
load("dataFS/Main/DaTS.RData")

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

test1<-DaTS

floods<-pdata.frame(index=c())
DaTS<-merge(DaTS,floods, all.x=TRUE)
DaTS<-merge(DaTS,monthlyRa, all.x=TRUE)


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
