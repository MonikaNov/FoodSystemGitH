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


A22<-lmer(Yield~scale(PreMed) +  scale(TemMed) 
              +I(scale(TemMed,center=FALSE)^2) 
              + (scale(TemMed) |ID),data=CrMaize16ts)
summary(A22) 
ranef(A22) 
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

hist(CrMaize16ts$PreMed)
hist(scale(CrMaize16ts$PreMed))
hist(scale(CrMaize16ts$PreMed,center=FALSE))
summary(CrMaize16$PreMed)


scale(CrMaize16$PreMed)[CrMaize16ts$ID==9 & CrMaize16ts$Year==2011]
mean(CrMaize16$PreMed)
sd(CrMaize16$PreMed)

(61.94258 - 110.9097)/50.06666
hist(CrMaize16ts$PreMedZ)


-24.98781* -0.2561794+726.58628* 0.9948118
#-----------
# temperature:


mean(CrMaize16$TemMed)
sd(CrMaize16$TemMed)

#-----------
CrMaize16ts[CrMaize16ts$PreMedZ<(-1),]

CrMaize16ts[CrMaize16ts$PreMedZ<(-0.9),]
CrMaize16ts[CrMaize16ts$PreMed<(20),]
CrMaize16ts[CrMaize16ts$ID==38 & CrMaize16$Year==2009,]
#-------------------------------------------------------------------------------------------------------------------------------------
#equation for Kericho 2009
#  a=PrecMed
#  b=temMed
predict(A22)
a<-106.6515
b<- 291.54 
y1<- -724.12698 +0.36243566 +0.21559 *  (a-mean(CrMaize16ts$PreMed))/sd(CrMaize16$PreMed)-24.98781 *(b-mean(CrMaize16$TemMed))/sd(CrMaize16$TemMed)+726.58628*(b/sqrt(  sum(CrMaize16$TemMed^2)/ (length( CrMaize16$TemMed)-1)  ))^2 -0.44085359*(b-mean(CrMaize16$TemMed))/sd(CrMaize16$TemMed) 
y1


a<-1.3*106.6515
b<-291.54 
y2<- -724.12698 +0.36243566 +0.21559 *  (a-mean(CrMaize16ts$PreMed))/sd(CrMaize16$PreMed)-24.98781 *(b-mean(CrMaize16$TemMed))/sd(CrMaize16$TemMed)+726.58628*(b/sqrt(  sum(CrMaize16$TemMed^2)/ (length( CrMaize16$TemMed)-1)  ))^2 -0.44085359*(b-mean(CrMaize16$TemMed))/sd(CrMaize16$TemMed)  
y2

a<-1.3*106.6515
b<-287.54 
y3<- -724.12698 +0.36243566 +0.21559 *  (a-mean(CrMaize16ts$PreMed))/sd(CrMaize16$PreMed)-24.98781 *(b-mean(CrMaize16$TemMed))/sd(CrMaize16$TemMed)+726.58628*(b/sqrt(  sum(CrMaize16$TemMed^2)/ (length( CrMaize16$TemMed)-1)  ))^2 -0.44085359*(b-mean(CrMaize16$TemMed))/sd(CrMaize16$TemMed) 
y3

#-----------------------------------------------------------------------------------------------------------------------
barplot(c(y1,y2,y3))  

par(mar=c(1,4.2,3.5,1))
  barplot(c(y1,y2,y3)-0.3,axes=FALSE,ylab=("Yield, tonnes per hectare"),main=" Kericho, 2009 ")
axis(2, at=seq(0.2, 3.8, 0.5), labels=c("0.5","1","1.5","2","2.5","3","3.5","4")) 

legend(0.04,1.65,legend = c("Weather as observed"),text.col=c("black")
       ,bg="beige",box.col="white",pt.cex=0.9,pt.lwd=0,bty="n",cex=0.9)

legend(0.04,1.42,legend = c("Precipitation = 106.65 ml"),text.col=c("blue")
       ,bg="beige",box.col="white",pt.cex=0.1,pt.lwd=0,bty="n",cex=0.9)
legend(0.04,1.18,legend = c("Temperature = 29.15C°"),text.col=c("red")
       ,bg="beige",box.col="white",pt.cex=0.1,pt.lwd=0,bty="n",cex=0.9)
legend(1.28,1.23,legend = c("Precipitation: +30%"),cex=0.9,text.col=c("blue")
       ,bg="beige",box.col="white",pt.cex=0.1,pt.lwd=0,bty="n")
legend(2.52,1.39,legend = c("Precipitation: +30%"),cex=0.9,text.col=c("blue")
       ,bg="beige",box.col="white",pt.cex=0.1,pt.lwd=0,bty="n")
legend(2.45,1.09,legend = c("Temperature: +0.4C°"),cex=0.9,text.col=c("red")
       ,bg="beige",box.col="white",pt.cex=0.1,pt.lwd=0,bty="n")

#-----------------------------------------------------------------------------------------------------------------------



barplot(c(y1,y2,y3))


,ylim=c(0.4,0.95))
(0.6296666-0.6029937)/0.6029937




a<-61.94258
b<-295.9467
y<- -724.12698 -0.40181822+ 0.21559 *  (a-mean(CrMaize16ts$PreMed))/sd(CrMaize16$PreMed)-24.98781 *(b-mean(CrMaize16$TemMed))/sd(CrMaize16$TemMed)+726.58628*(b/sqrt(  sum(CrMaize16$TemMed^2)/ (length( CrMaize16$TemMed)-1)  ))^2 +   0.48875711*(b-mean(CrMaize16$TemMed))/sd(CrMaize16$TemMed) 
y

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# so which county do next? which is the driest?
aggregate(cbind(PreMed,100*Yield)~county,data=CrMaize16,FUN=mean)
bar<-aggregate(cbind(PreMed,100*Yield)~county,data=CrMaize16,FUN=mean)
barplot(heigh=as.matrix(t(bar[,c(2,3)])),names.arg=bar[,1],beside=TRUE)
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#
cor.test(CrMaize16$PreMed,CrMaize16$Yield)

plot(Yield~PreMed,data=CrMaize16,xlab="Average monthly precipitation, growing season",
     ylab="Maize Yield, tonnes per hectare",main=c("+100 ml in average monthly precipitation -> + 0.78 t/hectare"))
abline(lm (Yield~PreMed,data=CrMaize16),lwd=2)
summary(lm (Yield~PreMed,data=CrMaize16))
lines(c(156.59112,156.59112),c(1,1.77512),col="red",lwd=3,lty=2)
lines(c(56.59112,156.59112),c(1,1),col="red",lwd=3,lty=2)

legend(160,1.8,legend = c("0.78 tonnes/ hectare"),text.col=c("red")
,bg="beige",box.col="white",  cex=1)
legend(100,0.90,legend = c("100ml"),bg = "beige" ,box.col="white",pch=NA,
     text.col=c("red"))


#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
save.image("~/foodSystems/Rcodes/Eq1ProdFun4/forPresentation6b.RData")