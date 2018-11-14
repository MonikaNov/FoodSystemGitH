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

plot(MUACn~PreMean, data = Phase22ts,main=" MUACn~PreMean",  xlab="Precipitation")

# plot(lm(MUACn~PreMean, data = Phase22ts),main=" MUACn~PreMean")
plot(Phase22ts$PreMean,Phase22ts$MUACn,main=" MUACn~PreMean",  xlab="Precipitation")
plot(Phase22ts$PreMeanZ,Phase22ts$MUACn,main=" MUACn~PreMeanZ",  xlab="Precipitation")
plot(I(Phase22ts$PreMean^2),Phase22ts$MUACn,main=" MUACn~PreMeanZ sq.",  xlab="Precipitation")
plot(I(Phase22ts$PreMeanZ^2),Phase22ts$MUACn,main=" MUACn~PreMeanZ",  xlab="Precipitation")  # this not too much interesting
plot(I(Phase22ts$PreMean^3),Phase22ts$MUACn,main=" MUACn~PreMeanZ sq.",  xlab="Precipitation")
plot(I(Phase22ts$PreMeanZ^3),Phase22ts$MUACn,main=" MUACn~PreMeanZ",  xlab="Precipitation") 

# median (comparison with the mean)

plot(Phase22ts$PreMean,Phase22ts$MUACn,main=" MUACn~PreMean",  xlab="Precipitation")
plot(Phase22ts$PreMed,Phase22ts$MUACn,main=" MUACn~PreMed",  xlab="Precipitation")
plot(I(Phase22ts$PreMean^2),Phase22ts$MUACn,main=" MUACn~PreMean sq.",  xlab="Precipitation")
plot(I(Phase22ts$PreMed^2),Phase22ts$MUACn,main=" MUACn~PreMed sq.",  xlab="Precipitation")  # this not too much interesting

plot(Phase22ts$PreMeanZ,Phase22ts$MUACn,main=" MUACn~PreMeanZ",  xlab="Precipitation")
plot(Phase22ts$PreMedZ,Phase22ts$MUACn,main=" MUACn~PreMedZ",  xlab="Precipitation")

# ehm... the following 2 don't make much sense :-)
plot(Phase22ts$PreMeanZ^2,Phase22ts$MUACn,main=" MUACn~PreMeanZ",  xlab="Precipitation")
plot(Phase22ts$PreMedZ^2,Phase22ts$MUACn,main=" MUACn~PreMedZ",  xlab="Precipitation")

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#     precipitation lagged - mean


plot(Phase22ts$PreMean,Phase22ts$MUACn,main=" MUACn~PreMean",  xlab="Precipitation")
plot(lag(Phase22ts$PreMean),Phase22ts$MUACn,main=" MUACn~PreMean",  xlab="Precipitation")
plot(lag(Phase22ts$PreMean,k=2),Phase22ts$MUACn,main=" MUACn~PreMean",  xlab="Precipitation")
plot(lag(Phase22ts$PreMean,k=3),Phase22ts$MUACn,main=" MUACn~PreMean",  xlab="Precipitation")
plot(lag(Phase22ts$PreMean,k=4),Phase22ts$MUACn,main=" MUACn~PreMean",  xlab="Precipitation")
plot(lag(Phase22ts$PreMean,k=5),Phase22ts$MUACn,main=" MUACn~PreMean",  xlab="Precipitation")
plot(lag(Phase22ts$PreMean,k=6),Phase22ts$MUACn,main=" MUACn~PreMean",  xlab="Precipitation")

plot(Phase22ts$PreMean^2,Phase22ts$MUACn,main=" MUACn~PreMean",  xlab="Precipitation")
plot(lag(Phase22ts$PreMean)^2,Phase22ts$MUACn,main=" MUACn~PreMean",  xlab="Precipitation")
plot(lag(Phase22ts$PreMean,k=2)^2,Phase22ts$MUACn,main=" MUACn~PreMean",  xlab="Precipitation")
plot(lag(Phase22ts$PreMean,k=3)^2,Phase22ts$MUACn,main=" MUACn~PreMean",  xlab="Precipitation")
plot(lag(Phase22ts$PreMean,k=4)^2,Phase22ts$MUACn,main=" MUACn~PreMean",  xlab="Precipitation")
plot(lag(Phase22ts$PreMean,k=5)^2,Phase22ts$MUACn,main=" MUACn~PreMean",  xlab="Precipitation")
plot(lag(Phase22ts$PreMean,k=6)^2,Phase22ts$MUACn,main=" MUACn~PreMean",  xlab="Precipitation")


cor.test(lag(Phase22ts$PreMean,k=3),Phase22ts$MUACn)
lapply(1:54,function(x)  cor.test(lag(Phase22ts$PreMean,k=x),Phase22ts$MUACn))
# all the lags of precipitation correlated. Interesting :-)

# ---------------
# lagged z-scores

plot(Phase22ts$PreMeanZ,Phase22ts$MUACn,main=" MUACn~PreMeanZ",  xlab="Precipitation")
plot(lag(Phase22ts$PreMeanZ),Phase22ts$MUACn,main=" MUACn~PreMeanZ",  xlab="Precipitation")
plot(lag(Phase22ts$PreMeanZ,k=2),Phase22ts$MUACn,main=" MUACn~PreMeanZ",  xlab="Precipitation")
Sys.sleep(2)
plot(lag(Phase22ts$PreMeanZ,k=3),Phase22ts$MUACn,main=" MUACn~PreMeanZ",  xlab="Precipitation")
Sys.sleep(2)
plot(lag(Phase22ts$PreMeanZ,k=4),Phase22ts$MUACn,main=" MUACn~PreMeanZ",  xlab="Precipitation")
Sys.sleep(2)
plot(lag(Phase22ts$PreMeanZ,k=5),Phase22ts$MUACn,main=" MUACn~PreMeanZ",  xlab="Precipitation")
Sys.sleep(2)
plot(lag(Phase22ts$PreMeanZ,k=6),Phase22ts$MUACn,main=" MUACn~PreMeanZ",  xlab="Precipitation")


# an interesting comparison:-------------------------------------------------------------------------------
plot(lag(Phase22ts$PreMean),Phase22ts$MUACn,main=" MUACn~PreMean",  xlab="Precipitation")
plot(lag(Phase22ts$PreMeanZ),Phase22ts$MUACn,main=" MUACn~PreMeanZ",  xlab="Precipitation")
# an interesting comparison:-------------------------------------------------------------------------------but I think that it is actually expectable- for the z -score to be more close together
# and very similar for the higher orde lags

lapply(1:54,function(x)  cor.test(lag(Phase22ts$PreMeanZ,k=x),Phase22ts$MUACn))
lapply(1:54,function(x)  cor.test(lag(Phase22ts$PreMedZ,k=x),Phase22ts$MUACn))
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#     precipitation lagged - median
for (i in 0:25)
{plot(lag(Phase22ts$PreMed,k=i),Phase22ts$MUACn,main=" MUACn~PreMed",  xlab=paste("Precipitation,","lag: ",i))
  Sys.sleep(0.5)
  }

for (i in 0:25)
{plot(lag(Phase22ts$PreMed,k=i)^2,Phase22ts$MUACn,main=" MUACn~PreMed",  xlab=paste("Precipitation sq,","lag: ",i))
  Sys.sleep(0.5)
}

lapply(1:54,function(x)  cor.test(lag(Phase22ts$PreMed,k=x),Phase22ts$MUACn)) # again, the correlation never dies

# compare correlation square and non-square

par(mfrow=c(2,1),mar=c(3,1,1,1))
for (i in 0:25)
{plot(lag(Phase22ts$PreMed,k=i),Phase22ts$MUACn,main=" ")  
  legend(y=60,x=300,legend=paste("Precipitation,","lag: ",i))
  plot(lag(Phase22ts$PreMed,k=i)^2,Phase22ts$MUACn,main="MUAC")
  legend(y=60,x=4.5,legend=paste("Prec. square.,","lag: ",i))
  Sys.sleep(1.5)
}
par(mfrow=c(1,1),mar=c(5.1,4.1,4.1,2.1))
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------
# now compare row and z-scores
par(mfrow=c(2,1),mar=c(3,2,1,1))
for (i in 0:25)
{plot(lag(Phase22ts$PreMed,k=i),Phase22ts$MUACn,main=" ")  
  legend(y=60,x=300,legend=paste("Precipitation raw,","lag: ",i))
  plot(lag(Phase22ts$PreMedZ,k=i),Phase22ts$MUACn,main="MUAC")
  legend(y=60,x=5,legend=paste("Prec. z-score,","lag: ",i))
  Sys.sleep(1.5)
}
par(mfrow=c(1,1),mar=c(5.1,4.1,4.1,2.1))