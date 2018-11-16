rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)

library('dplyr')
library('tseries')
library(plm)
library(lme4)
library(lattice)
library(car)
library(lmerTest)



load("Main/CrMaize16.RData")
CrMaize16ts<-pdata.frame(CrMaize16,index=c("ID","Year"))


#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

cor.test(CrMaize16$PreMed,CrMaize16$Yield)

par(mar=c(4.2,4.2,3.5,1))
plot(Yield~PreMed,data=CrMaize16,xlab="Average monthly precipitation, growing season",
     ylab="Maize Yield, tonnes per hectare",main=c("+100 mm in average monthly precipitation -> + 0.78 t/hectare"),cex.lab=1.2)
abline(lm (Yield~PreMed,data=CrMaize16),lwd=2)
summary(lm (Yield~PreMed,data=CrMaize16))
lines(c(156.59112,156.59112),c(1,1.77512),col="red",lwd=3,lty=2)
lines(c(56.59112,156.59112),c(1,1),col="red",lwd=3,lty=2)

legend(160,1.8,legend = c("0.78 tonnes/ hectare"),text.col=c("red")
       ,bg="beige",box.col="white",  cex=1)
legend(100,0.90,legend = c("100mm"),bg = "beige" ,box.col="white",pch=NA,
       text.col=c("red"))

#---------------------------------------------------------------------------------------------------------
# also in kg
cor.test(CrMaize16$PreMed,CrMaize16$Yield)
cor.test(CrMaize16$TemMed,CrMaize16$Yield)
plot(1000*Yield~PreMed,data=CrMaize16,xlab="Average monthly precipitation, growing season",
     ylab="Maize Yield, kg / hectare",
     main=c("+100 mm in average monthly precipitation -> + 780 kg/hectare"))
abline(lm (1000*Yield~PreMed,data=CrMaize16),lwd=2)
summary(lm (1000*Yield~PreMed,data=CrMaize16))
lines(c(156.59112,156.59112),c(1000,1775.12),col="red",lwd=3,lty=2)
lines(c(56.59112,156.59112),c(1000,1000),col="red",lwd=3,lty=2)

legend(160,1800,legend = c("7800 tonnes/ hectare"),text.col=c("red")
       ,bg="beige",box.col="white",  cex=1)
legend(100,900,legend = c("100mm"),bg = "beige" ,box.col="white",pch=NA,
       text.col=c("red"))

#-ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

a<-61.94258
b<-293.9467
y1<- -724.12698 -0.40181822+ 0.21559 *  (a-mean(CrMaize16ts$PreMed))/sd(CrMaize16$PreMed)-24.98781 *(b-mean(CrMaize16$TemMed))/sd(CrMaize16$TemMed)+726.58628*(b/sqrt(  sum(CrMaize16$TemMed^2)/ (length( CrMaize16$TemMed)-1)  ))^2 +   0.48875711*(b-mean(CrMaize16$TemMed))/sd(CrMaize16$TemMed) 

a<-1.3*61.94258
b<-293.9467
y2<- -724.12698 -0.40181822+ 0.21559 *  (a-mean(CrMaize16ts$PreMed))/sd(CrMaize16$PreMed)-24.98781 *(b-mean(CrMaize16$TemMed))/sd(CrMaize16$TemMed)+726.58628*(b/sqrt(  sum(CrMaize16$TemMed^2)/ (length( CrMaize16$TemMed)-1)  ))^2 +   0.48875711*(b-mean(CrMaize16$TemMed))/sd(CrMaize16$TemMed) 

a<-1.3*61.94258
b<-289.9467
y3<- -724.12698 -0.40181822+ 0.21559 *  (a-mean(CrMaize16ts$PreMed))/sd(CrMaize16$PreMed)-24.98781 *(b-mean(CrMaize16$TemMed))/sd(CrMaize16$TemMed)+726.58628*(b/sqrt(  sum(CrMaize16$TemMed^2)/ (length( CrMaize16$TemMed)-1)  ))^2 +   0.48875711*(b-mean(CrMaize16$TemMed))/sd(CrMaize16$TemMed) 

#-----------------------------------------------------------------------------------------------------------------------

par(mar=c(1,4.2,3.5,1))
barplot(c(y1,y2,y3)-0.3,axes=FALSE,ylab=("Yield, tonnes per hectare"),main=" Muranga'a, 2000 ",cex.lab=1.3)
axis(2, at=seq(0, 0.6, 0.1), labels=c("0.3","0.4","0.5","0.6","0.7","0.9","1")) 

legend(-0.05,0.28,legend = c("Weather:\n as observed"),text.col=c("black")
       ,bg="beige",box.col="white",pt.cex=0.1,pt.lwd=0,bty="n",cex=1.1)

legend(-0.05,0.21,legend = c("Precipitation:\n 61.9 mm"),cex=1.1,text.col=c("blue")
       ,bg="beige",box.col="white",pt.cex=0.1,pt.lwd=0,bty="n")
legend(-0.05,0.14,legend = c("Temperature:\n 28.99C°"),cex=1.1,text.col=c("red")
       ,bg="beige",box.col="white",pt.cex=0.1,pt.lwd=0,bty="n")
legend(1.15,0.28,legend = c("Precipitation:\n +30%"),cex=1.1,text.col=c("blue")
       ,bg="beige",box.col="white",pt.cex=0.1,pt.lwd=0,bty="n")
legend(1.15,0.21,legend = c("Temperature:\n as observed"),text.col=c("red")
       ,bg="beige",box.col="white",pt.cex=0.1,pt.lwd=0,bty="n",cex=1.1)
legend(2.37,0.39,legend = c("Precipitation:\n +30%"),cex=1.1,text.col=c("blue")
       ,bg="beige",box.col="white",pt.cex=0.1,pt.lwd=0,bty="n")
legend(2.37,0.32,legend = c("Temperature:\n -0.4C°"),cex=1.1,text.col=c("red")
       ,bg="beige",box.col="white",pt.cex=0.1,pt.lwd=0,bty="n")



#-ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#equation for Kericho 2009
#  a=PrecMed
#  b=temMed
predict(A22)
a<-106.6515
b<- 291.54 
y1<- -724.12698 +0.36243566 +0.21559 *  (a-mean(CrMaize16ts$PreMed))/sd(CrMaize16$PreMed)-24.98781 *(b-mean(CrMaize16$TemMed))/sd(CrMaize16$TemMed)+726.58628*(b/sqrt(  sum(CrMaize16$TemMed^2)/ (length( CrMaize16$TemMed)-1)  ))^2 -0.44085359*(b-mean(CrMaize16$TemMed))/sd(CrMaize16$TemMed) 

a<-1.3*106.6515
b<-291.54 
y2<- -724.12698 +0.36243566 +0.21559 *  (a-mean(CrMaize16ts$PreMed))/sd(CrMaize16$PreMed)-24.98781 *(b-mean(CrMaize16$TemMed))/sd(CrMaize16$TemMed)+726.58628*(b/sqrt(  sum(CrMaize16$TemMed^2)/ (length( CrMaize16$TemMed)-1)  ))^2 -0.44085359*(b-mean(CrMaize16$TemMed))/sd(CrMaize16$TemMed)  

5+5
a<-1.3*106.6515
b<-287.54 
y3<- -724.12698 +0.36243566 +0.21559 *  (a-mean(CrMaize16ts$PreMed))/sd(CrMaize16$PreMed)-24.98781 *(b-mean(CrMaize16$TemMed))/sd(CrMaize16$TemMed)+726.58628*(b/sqrt(  sum(CrMaize16$TemMed^2)/ (length( CrMaize16$TemMed)-1)  ))^2 -0.44085359*(b-mean(CrMaize16$TemMed))/sd(CrMaize16$TemMed) 

#-----------------------------------------------------------------------------------------------------------------------
barplot(c(y1,y2,y3))  

par(mar=c(1,4.2,3.5,1))
barplot(c(y1,y2,y3)-0.3,axes=FALSE,ylab=("Yield, tonnes per hectare"),main=" Kericho, 2009 ",cex.lab=1.3)
axis(2, at=seq(0.2, 3.8, 0.5), labels=c("0.5","1","1.5","2","2.5","3","3.5","4")) 

legend(0.01,1.75,legend = c("Weather:\n as observed"),text.col=c("black")
       ,bg="beige",box.col="white",pt.cex=0.9,pt.lwd=0,bty="n")

legend(0.0,1.4,legend = c("Precipitation:\n   106.65 mm"),text.col=c("blue")
       ,bg="beige",box.col="white",pt.cex=0.1,pt.lwd=0,cex=1.1,bty="n")
legend(0.0,1.05,legend = c("Temperature:\n    29.15C°"),cex=1.1,text.col=c("red")
       ,bg="beige",box.col="white",pt.cex=0.1,pt.lwd=0,bty="n")
legend(1.21,1.4,legend = c("Precipitation:\n   +30%"),cex=1.1,text.col=c("blue")
       ,bg="beige",box.col="white",pt.cex=0.1,pt.lwd=0,bty="n")

legend(1.21,1.05,legend = c("Temperature:\n as observed"),text.col=c("red")
       ,bg="beige",box.col="white",pt.cex=0.1,pt.lwd=0,bty="n")
legend(2.39,1.45,legend = c("Precipitation:\n   +30%"),cex=1.1,text.col=c("blue")
       ,bg="beige",box.col="white",pt.cex=0.1,pt.lwd=0,bty="n")
legend(2.39,1.1,legend = c("Temperature:\n   -0.4C°"),cex=1.1,text.col=c("red")
       ,bg="beige",box.col="white",pt.cex=0.1,pt.lwd=0,bty="n")

#-----------------------------------------------------------------------------------------------------------------------


