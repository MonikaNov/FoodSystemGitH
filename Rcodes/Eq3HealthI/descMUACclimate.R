rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)
library(plm)
library(lme4)

load("Main/Phase22.RData")

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

head(Phase22)
Phase22ts<-pdata.frame(Phase22,index=c("CountyID","T"))
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

# precipitation current

lapply(1:54,function(x)  cor.test(lag(Phase22ts$PreMean,k=x),Phase22ts$MUACn))


lapply(1:54,function(x)  cor.test(lag(Phase22ts$PreMeanZ,k=x),Phase22ts$MUACn))

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#     precipitation lagged - median


# compare correlation square and non-square

par(mfrow=c(2,1),mar=c(3,2,1,1))
for (i in 0:25)
{plot(Phase22ts$MUACn,lag(Phase22ts$PreMed,k=i),main=" ")  
  legend(x=40,y=500,legend=paste("Precipitation,","lag: ",i))
  plot(Phase22ts$MUACn,lag(Phase22ts$PreMed,k=i)^2,main="MUAC")
  legend(x=40,y=250000,legend=paste("Prec. square.,","lag: ",i))
  Sys.sleep(1.5)
}
par(mfrow=c(1,1),mar=c(5.1,4.1,4.1,2.1))


lapply(1:54,function(x)  cor.test(lag(Phase22ts$PreMed,k=x),Phase22ts$MUACn)) # again, the correlation never dies
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------
# now compare row and z-scores
par(mfrow=c(2,1),mar=c(3,2,1,1))
for (i in 0:25)
{plot(Phase22ts$MUACn,lag(Phase22ts$PreMed,k=i),main=" ")  
  legend(x=30,y=500,legend=paste("Precipitation raw,","lag: ",i))
  plot(Phase22ts$MUACn,lag(Phase22ts$PreMedZ,k=i),main="MUAC")
  legend(x=30,y=8,legend=paste("Prec. z-score,","lag: ",i))
  Sys.sleep(1.5)
}
par(mfrow=c(1,1),mar=c(5.1,4.1,4.1,2.1))


lapply(1:54,function(x)  cor.test(lag(Phase22ts$PreMedZ,k=x),Phase22ts$MUACn)) # the correlation  dies cvery late



#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#     precipitation lagged - mean


# compare correlation square and non-square

par(mfrow=c(2,1),mar=c(3,2,1,1))
for (i in 0:25)
{plot(Phase22ts$MUACn,lag(Phase22ts$PreMean,k=i),main=" ")  
  legend(x=40,y=500,legend=paste("Precipitation,","lag: ",i))
  plot(Phase22ts$MUACn,lag(Phase22ts$PreMean,k=i)^2,main="MUAC")
  legend(x=40,y=200000,legend=paste("Prec. square.,","lag: ",i))
  Sys.sleep(1.5)
}
par(mfrow=c(1,1),mar=c(5.1,4.1,4.1,2.1))


lapply(1:54,function(x)  cor.test(lag(Phase22ts$PreMean,k=x),Phase22ts$MUACn)) # again, the correlation never dies
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------
# now compare row and z-scores
par(mfrow=c(2,1),mar=c(3,2,1,1))
for (i in 0:25)
{plot(Phase22ts$MUACn,lag(Phase22ts$PreMean,k=i),main=" ")  
  legend(x=38,y=490,legend=paste("Precipitation raw,","lag: ",i))
  plot(Phase22ts$MUACn,lag(Phase22ts$PreMeanZ,k=i),main="MUAC")
  legend(x=40,y=12,legend=paste("Prec. z-score,","lag: ",i))
  Sys.sleep(1.5)
}
par(mfrow=c(1,1),mar=c(5.1,4.1,4.1,2.1))


lapply(1:54,function(x)  cor.test(lag(Phase22ts$PreMeanZ,k=x),Phase22ts$MUACn)) # the correlation  dies cvery late