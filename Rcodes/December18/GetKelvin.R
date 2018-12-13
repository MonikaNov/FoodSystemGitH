rm(list=ls())

library('dplyr')
library('purrr')
load("dataFS/Main/DaTS.RData")

DaTS<-DaTS[1:159]
test<-DaTS
DaTS$code<-factor(DaTS$code);DaTS$name<-factor(DaTS$name);DaTS$Admin1<-factor(DaTS$Admin1);DaTS$county<-factor(DaTS$county);DaTS$Admin2<-factor(DaTS$Admin2)
rm(ScaledTS)

library(dplyr); library(tseries); library(plm); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

CV<-read.csv("../FoodSystemGitH/dataFS/GuigmaDec/coef_var.csv",na.strings="-999",dec=".")
summary(CV);head(CV)

colnames(CV)[5:10]<-c("AvgTempK_MarMay","CVTempK_MarMay","AvgTempK_MarAug","CVTempK_MarAug","AvgTempK_OctDec","CVTempK_OctDec")
CV2<-CV
CV<-CV2[-c(3,4)]
names(CV)[1]<-"Year"
CV$Year<-as.factor(CV$Year)
CV$code<-as.factor(CV$code)
DaTS<-merge(DaTS,CV, all.x=TRUE)
DaTS$ID1<-factor(DaTS$ID1);DaTS$name<-factor(DaTS$name);DaTS$code<-factor(DaTS$code);DaTS$Year<-factor(DaTS$Year);DaTS$Admin1<-factor(DaTS$Admin1);DaTS$county<-factor(DaTS$county);DaTS$Admin2<-factor(DaTS$Admin2)
summary(DaTS)

all_equal(DaTS[1:159],test)

DaTS<-DaTS[with(DaTS,order(ID1,Year)),]
test<-test[with(test,order(ID1,Year)),]
all.equal(DaTS[1:159],test,check.attributes=FALSE)
# laggs of OND season
DaTS<-pdata.frame(DaTS,index=c("ID1","Year"))
data.frame(sapply(c(164,165) ,function(x)  lag(DaTS[,x],1)  ))
lagged<-data.frame(sapply(c(164,165) ,function(x)  lag(DaTS[,x],1)  ))
names(lagged)<-c("AvgTempK_OctDec_L1", "CVTempK_OctDec_L1")

DaTS<-cbind.data.frame(DaTS,lagged)
DaTS$ID1<-factor(DaTS$ID1);DaTS$name<-factor(DaTS$name);DaTS$code<-factor(DaTS$code);DaTS$Year<-factor(DaTS$Year);DaTS$Admin1<-factor(DaTS$Admin1);DaTS$county<-factor(DaTS$county);DaTS$Admin2<-factor(DaTS$Admin2)
summary(DaTS)
DaTS<-pdata.frame(DaTS,index=c("ID1","Year"))
DaTS[DaTS$Year==1981, 166:167]<-NA

#--------------------------------------------------------------------------------------------------------------------------------------------------------------
# a bit of testing:
plot(DaTS$AvgTempK_OctDec_L1,DaTS$AvgTempK_OctDec)
cor.test(DaTS$AvgTempK_OctDec_L1,DaTS$AvgTempK_OctDec)

plot(ScaledTS$AvgTempK_OctDec_L1,ScaledTS$AvgTempK_OctDec)
cor.test(ScaledTS$AvgTempK_OctDec_L1,ScaledTS$AvgTempK_OctDec)

plot(DaTS$AvgTempK_OctDec_L1,DaTS$AvgTempK_OctDec)
cor.test(DaTS$AvgTempK_OctDec_L1,DaTS$AvgTempK_OctDec)


plot(DaTS$CVTempK_OctDec,DaTS$CVTempK_OctDec_L1)

cor.test(DaTS$CVTempK_OctDec,DaTS$CVTempK_OctDec_L1)

cor.test(DaTS$CVTempK_OctDec[1:1563],DaTS$CVTempK_OctDec_L1[2:1564])
cor.test(DaTS$CVTempK_OctDec[1:1564],DaTS$CVTempK_OctDec_L1[1:1564])

cor.test(ScaledTS$CVTempK_OctDec,ScaledTS$CVTempK_OctDec_L1)
plot(ScaledTS$CVTempK_OctDec,ScaledTS$CVTempK_OctDec_L1)

plot(DaTS$CVTempK_OctDec,DaTS$SDTemp_OctDec)
plot(DaTS$SDTemp_OctDec,DaTS$CVTempK_OctDec)

plot(DaTS$AvgTempK_MarMay,DaTS$AvgTemp_MarMay)
plot(DaTS$CVTempK_MarMay,DaTS$SDTemp_MarMay)
plot(DaTS$SDTemp_MarMay,DaTS$CVTempK_MarMay)

plot(DaTS$AvgTempK_MarAug,DaTS$AvgTemp_MarAug)
plot(DaTS$CVTempK_MarAug,DaTS$SDTemp_MarAug)
plot(DaTS$SDTemp_MarAug,DaTS$CVTempK_MarAug)
# now seasons   ........   ........        ..........      ..... . .   . . . . . .

test2<-DaTS

DaTS$AvgTempK[DaTS$ASAL==TRUE]<-( (DaTS$AvgTempK_MarMay[DaTS$ASAL==TRUE]+DaTS$AvgTempK_OctDec_L1[DaTS$ASAL==TRUE]) /2)
DaTS$CVTempK[DaTS$ASAL==TRUE]<-( (DaTS$CVTempK_MarMay[DaTS$ASAL==TRUE]+DaTS$CVTempK_OctDec_L1[DaTS$ASAL==TRUE]) /2)

DaTS$AvgTempK[DaTS$ASAL==FALSE]<-(DaTS$AvgTempK_MarAug[DaTS$ASAL==FALSE])
DaTS$CVTempK[DaTS$ASAL==FALSE]<-(DaTS$CVTempK_MarAug[DaTS$ASAL==FALSE])

all.equal(DaTS[1:167],test2)
all.equal(DaTS[1:159],test,check.attributes=FALSE)

# now scaling...   ........   ........        ..........      ..... . .   . . . . . .

rm(ScaledTS)
ScaledTS<-DaTS
ScaledTS[,c(5:76,81:83,85:169)]<-scale(DaTS[,c(5:76,81:83,85:169)])
ScaledTS$Yield0<-DaTS$Yield
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

# and check if scaling correctly

names(DaTS)[c(5:76,81:83,85:169)]

mapply(cor.test,ScaledTS[,c(5:76,81:83,85:159)],DaTS[,c(5:76,81:83,85:159)] )
mapply(function(x,y) cor.test(x,y)$p.value, ScaledTS[,c(5:76,81:83,85:159)],DaTS[,c(5:76,81:83,85:159)])

cor.test(ScaledTS$Prec2m,DaTS$Prec2m) 
summary(ScaledTS)
sapply(ScaledTS,function(x) mean(x,na.rm=TRUE));plot(sapply(ScaledTS,function(x) mean(x,na.rm=TRUE)))

which(sapply(ScaledTS,function(x) mean(x,na.rm=TRUE))>0.3)

sum(complete.cases(ScaledTS))
sum(complete.cases(ScaledTS))
sum(!complete.cases(ScaledTS))

sum(  !complete.cases(ScaledTS) &is.na(ScaledTS$Yield)==FALSE )

sum(  !complete.cases(ScaledTS) &is.na(ScaledTS$Yield)==TRUE )

all.equal(ScaledTS[,-c(5:76,81:83,85:170)],DaTS[,-c(5:76,81:83,85:169)] ) # seems ok
all.equal(ScaledTS$Yield0,DaTS$Yield)
plot(ScaledTS$Yield0,DaTS$Yield)

#-------------------------------------------------------------------------------------------------------------------------------------------------------

# a bit of checking??
i<-sample(1:1559,1)
i
DaTS[i:(i+8),c(1:5,160:169)]  # HURRAY, ALL SEEMS GOOD HERE
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

remove(list=setdiff(ls(),c("DaTS","ScaledTS")))
# save.image("dataFS/Main/DaTS.RData")
# save.image("Rcodes/December18/DaTS.RData")  # names???