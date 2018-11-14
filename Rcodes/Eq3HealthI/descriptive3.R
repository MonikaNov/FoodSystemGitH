rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)
library(plm)
library(lme4)

load("Main/Phase22.RData")

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

head(Phase22)
Phase22ts<-pdata.frame(Phase22,index=c("CountyID","T"))

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# 1. MUAC

par(mfrow=c(2,1))
plot(sapply(1:54,function(x)  cor.test(lag(Phase22ts$PreMean,k=x),Phase22ts$MUACn)[[3]]),ylab="MUAC p-val.",xlab="lag")
plot(sapply(1:54,function(x)  cor.test(lag(Phase22ts$PreMeanZ,k=x),Phase22ts$MUACn)[[3]]),ylab="MUAC p-val.",xlab="lag")
plot(sapply(1:54,function(x)  cor.test(lag(Phase22ts$PreMed,k=x),Phase22ts$MUACn)[[3]]),ylab="MUAC p-val.",xlab="lag")
plot(sapply(1:54,function(x)  cor.test(lag(Phase22ts$PreMedZ,k=x),Phase22ts$MUACn)[[3]]),ylab="MUAC p-val.",xlab="lag")

plot(sapply(1:54,function(x)  cor.test(lag(Phase22ts$TemMean,k=x),Phase22ts$MUACn)[[3]]),ylab="MUAC p-val.",xlab="lag")
plot(sapply(1:54,function(x)  cor.test(lag(Phase22ts$TemMeanZ,k=x),Phase22ts$MUACn)[[3]]),ylab="MUAC p-val.",xlab="lag")
plot(sapply(1:54,function(x)  cor.test(lag(Phase22ts$TemMed,k=x),Phase22ts$MUACn)[[3]]),ylab="MUAC p-val.",xlab="lag")
plot(sapply(1:54,function(x)  cor.test(lag(Phase22ts$TemMedZ,k=x),Phase22ts$MUACn)[[3]]),ylab="MUAC p-val.",xlab="lag")

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# 2. CSI

plot(sapply(1:54,function(x)  cor.test(lag(Phase22ts$PreMean,k=x),Phase22ts$CSIn)[[3]]),ylab="CSI p-val.",xlab="lag")
plot(sapply(1:54,function(x)  cor.test(lag(Phase22ts$PreMeanZ,k=x),Phase22ts$CSIn)[[3]]),ylab="CSI p-val.",xlab="lag")
plot(sapply(1:54,function(x)  cor.test(lag(Phase22ts$PreMed,k=x),Phase22ts$CSIn)[[3]]),ylab="CSI p-val.",xlab="lag")
plot(sapply(1:54,function(x)  cor.test(lag(Phase22ts$PreMedZ,k=x),Phase22ts$CSIn)[[3]]),ylab="CSI p-val.",xlab="lag")

plot(sapply(1:54,function(x)  cor.test(lag(Phase22ts$TemMean,k=x),Phase22ts$CSIn)[[3]]),ylab="CSI p-val.",xlab="lag")
plot(sapply(1:54,function(x)  cor.test(lag(Phase22ts$TemMeanZ,k=x),Phase22ts$CSIn)[[3]]),ylab="CSI p-val.",xlab="lag")
plot(sapply(1:54,function(x)  cor.test(lag(Phase22ts$TemMed,k=x),Phase22ts$CSIn)[[3]]),ylab="CSI p-val.",xlab="lag")
plot(sapply(1:54,function(x)  cor.test(lag(Phase22ts$TemMedZ,k=x),Phase22ts$CSIn)[[3]]),ylab="CSI p-val.",xlab="lag")
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# .. More of scaling to see significance 

#MUAC:
plot(sapply(1:54,function(x)  cor.test(lag(Phase22ts$PreMeanZ,k=x),Phase22ts$MUACn)[[3]]),ylab="MUAC p-val.",xlab="lag",ylim=c(0,0.2))
plot(sapply(1:54,function(x)  cor.test(lag(Phase22ts$PreMedZ,k=x),Phase22ts$MUACn)[[3]]),ylab="MUAC p-val.",xlab="lag",ylim=c(0,0.2))

plot(sapply(1:54,function(x)  cor.test(lag(Phase22ts$TemMeanZ,k=x),Phase22ts$MUACn)[[3]]),ylab="MUAC p-val.",xlab="lag",ylim=c(0,0.2))
plot(sapply(1:54,function(x)  cor.test(lag(Phase22ts$TemMedZ,k=x),Phase22ts$MUACn)[[3]]),ylab="MUAC p-val.",xlab="lag",ylim=c(0,0.2))

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#CSI
plot(sapply(1:54,function(x)  cor.test(lag(Phase22ts$PreMeanZ,k=x),Phase22ts$CSIn)[[3]]),ylab="CSI p-val.",xlab="lag",ylim=c(0,0.2))
plot(sapply(1:54,function(x)  cor.test(lag(Phase22ts$PreMedZ,k=x),Phase22ts$CSIn)[[3]]),ylab="CSI p-val.",xlab="lag",ylim=c(0,0.2))

plot(sapply(1:54,function(x)  cor.test(lag(Phase22ts$TemMeanZ,k=x),Phase22ts$CSIn)[[3]]),ylab="CSI p-val.",xlab="lag",ylim=c(0,0.2))
plot(sapply(1:54,function(x)  cor.test(lag(Phase22ts$TemMedZ,k=x),Phase22ts$CSIn)[[3]]),ylab="CSI p-val.",xlab="lag",ylim=c(0,0.2))


#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# some squares:

# 1. MUAC

plot(sapply(1:54,function(x)  cor.test(lag(Phase22ts$PreMed,k=x),Phase22ts$MUACn)[[3]]),ylab="MUAC p-val.",xlab="lag")
plot(sapply(1:54,function(x)  cor.test(lag(Phase22ts$PreMed,k=x)^2,Phase22ts$MUACn)[[3]]),ylab="MUAC p-val.",xlab="lag")
plot(sapply(1:54,function(x)  cor.test(lag(Phase22ts$PreMedZ,k=x),Phase22ts$MUACn)[[3]]),ylab="MUAC p-val.",xlab="lag")
plot(sapply(1:54,function(x)  cor.test(lag(Phase22ts$PreMedZ,k=x)^2,Phase22ts$MUACn)[[3]]),ylab="MUAC p-val.",xlab="lag")

plot(sapply(1:54,function(x)  cor.test(lag(Phase22ts$TemMed,k=x),Phase22ts$MUACn)[[3]]),ylab="MUAC p-val.",xlab="lag")
plot(sapply(1:54,function(x)  cor.test(lag(Phase22ts$TemMed,k=x)^2,Phase22ts$MUACn)[[3]]),ylab="MUAC p-val.",xlab="lag")
plot(sapply(1:54,function(x)  cor.test(lag(Phase22ts$TemMedZ,k=x),Phase22ts$MUACn)[[3]]),ylab="MUAC p-val.",xlab="lag")
plot(sapply(1:54,function(x)  cor.test(lag(Phase22ts$TemMedZ,k=x)^2,Phase22ts$MUACn)[[3]]),ylab="MUAC p-val.",xlab="lag")


# 1. CSI

plot(sapply(1:54,function(x)  cor.test(lag(Phase22ts$PreMed,k=x),Phase22ts$CSIn)[[3]]),ylab="CSI p-val.",xlab="lag")
plot(sapply(1:54,function(x)  cor.test(lag(Phase22ts$PreMed,k=x)^2,Phase22ts$CSIn)[[3]]),ylab="CSI p-val.",xlab="lag")
plot(sapply(1:54,function(x)  cor.test(lag(Phase22ts$PreMedZ,k=x),Phase22ts$CSIn)[[3]]),ylab="CSI p-val.",xlab="lag")
plot(sapply(1:54,function(x)  cor.test(lag(Phase22ts$PreMedZ,k=x)^2,Phase22ts$CSIn)[[3]]),ylab="CSI p-val.",xlab="lag")

plot(sapply(1:54,function(x)  cor.test(lag(Phase22ts$TemMed,k=x),Phase22ts$CSIn)[[3]]),ylab="CSI p-val.",xlab="lag")
plot(sapply(1:54,function(x)  cor.test(lag(Phase22ts$TemMed,k=x)^2,Phase22ts$CSIn)[[3]]),ylab="CSI p-val.",xlab="lag")
plot(sapply(1:54,function(x)  cor.test(lag(Phase22ts$TemMedZ,k=x),Phase22ts$CSIn)[[3]]),ylab="CSI p-val.",xlab="lag")
plot(sapply(1:54,function(x)  cor.test(lag(Phase22ts$TemMedZ,k=x)^2,Phase22ts$CSIn)[[3]]),ylab="CSI p-val.",xlab="lag")