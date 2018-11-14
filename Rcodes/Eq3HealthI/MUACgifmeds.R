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
# Precipitation raw and square

par(mfrow=c(2,1),mar=c(2.4,4,1,1))
for (i in 0:30)
{plot(lag(Phase22ts$PreMed,k=i),Phase22ts$MUACn,ylab="MUAC",main=" ")  
  legend(x=300,y=60,legend=paste("Climate var.: raw, lag: " ,i),bty="n")
  plot(lag(Phase22ts$PreMed,k=i)^2,Phase22ts$MUACn,ylab="MUAC",main="Precipitation (median)")
  legend(x=130000,y=60,legend=paste("Climate var.: squared, lag: ",i),bty="n")
  #  Sys.sleep(1.5)
 dev.copy(jpeg,filename=paste("/its/home/mn301/foodSystems/Rcodes/Eq3HealthI/plotsGif/PrecSq/tsq",i,".jpg",sep="")); dev.off ()
}

par(mfrow=c(1,1),mar=c(5.1,4.1,4.1,2.1))

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# Temperature raw and square

par(mfrow=c(2,1),mar=c(2.4,4,1,1))
for (i in 0:30)
{plot(lag(Phase22ts$TemMed,k=i),Phase22ts$MUACn,ylab="MUAC",main=" ")  
  legend(x=285,y=60,legend=paste("Temperature: raw, lag= " ,i),bty="n")
  plot(lag(Phase22ts$TemMed,k=i)^2,Phase22ts$MUACn,ylab="MUAC",main="Temperature (median)")
  legend(x=81000,y=60,legend=paste("Temperature: squared, lag= ",i),bty="n")
  #  Sys.sleep(1.5)
 dev.copy(jpeg,filename=paste("/its/home/mn301/foodSystems/Rcodes/Eq3HealthI/plotsGif/TempSq/tsq",i,".jpg",sep="")); dev.off ()
}
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# Precipitation, temperature z-score

par(mfrow=c(2,1),mar=c(2.4,4,1,1))  
for (i in seq(0,20,2))  # double or triple step to make the changes more pronounced
{plot(lag(Phase22ts$PreMedZ,k=i),Phase22ts$MUACn,ylab="MUAC",main=" ")  
  legend(x=3,y=60,legend=paste("Precipitation Z -score, lag= " ,i),bty="n")
  plot(lag(Phase22ts$TemMedZ,k=i),Phase22ts$MUACn,ylab="MUAC",main=" ")
  legend(x=0,y=60,legend=paste("Temperature Z -score, lag= ",i),bty="n")
   Sys.sleep(0.5)
   # dev.copy(jpeg,filename=paste("/its/home/mn301/foodSystems/Rcodes/Eq3HealthI/plotsGif/Zscores2/Zpt",i,".jpg",sep="")); dev.off ()
}

par(mfrow=c(2,1),mar=c(4,4,2,1)) 
plot(sapply(0:54, function(x) cor.test(lag(Phase22ts$PreMed,k=x ),Phase22ts$MUACn)[[3]]),
     ylab="P-value",main="Correlation MUAC and Precipitation Raw",cex.main=0.8,xlab="lag precipitation")
plot(sapply(0:54, function(x) cor.test(lag(Phase22ts$PreMedZ,k=x ),Phase22ts$MUACn)[[3]]),
     ylab="P-value",main="Correlation MUAC and Precipitation Z score",cex.main=0.8,xlab="lag precipitation")

plot(sapply(0:54, function(x) cor.test(lag(Phase22ts$TemMed,k=x ),Phase22ts$MUACn)[[3]]),ylab="P-value",main="Correlation MUAC and Temperature Raw")
plot(sapply(0:54, function(x) cor.test(lag(Phase22ts$TemMedZ,k=x ),Phase22ts$MUACn)[[3]]),ylab="P-value",main="Correlation MUAC and temperature Z score")


# Precipitation, temperature z-scores Squares

par(mfrow=c(2,1),mar=c(2.4,4,1,1))  
for (i in seq(0,40,3))  # double or triple step to make the changes more pronounced
{plot(lag(Phase22ts$PreMedZ^2,k=i),Phase22ts$MUACn,ylab="MUAC",main=" ",xlim=c(0,10))  
  legend(x=3,y=60,legend=paste("Precipitation Z -score, lag= " ,i),bty="n")
  plot(lag(Phase22ts$TemMedZ^2,k=i),Phase22ts$MUACn,ylab="MUAC",main=" ",xlim=c(0,8))
  legend(x=0,y=60,legend=paste("Temperature Z -score, lag= ",i),bty="n")
   Sys.sleep(0.5)
   #  dev.copy(jpeg,filename=paste("/its/home/mn301/foodSystems/Rcodes/Eq3HealthI/plotsGif/Zscores2/Zpt",i,".jpg",sep="")); dev.off ()
}