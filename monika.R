rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)


emissions<-read.csv( "emissions.csv",header=TRUE,dec=".")

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
summary(emissions)

#1
plot(emissions$noise_level)
which(emissions$noise_level<20)
sum(which(emissions$noise_level<20))


#2
plot(emissions$extra_urban_metric)
emissions[emissions$extra_urban_metric>80 & !is.na(emissions$extra_urban_metric),]
emissions$model[emissions$extra_urban_metric>80 & !is.na(emissions$extra_urban_metric)]

#3
names(emissions)
lm1<-lm(  (emissions$combined_metric-emissions$extra_urban_metric)~ I(emissions$urban_metric-emissions$extra_urban_metric )  -1)
summary(lm1)

#4
i<-which(emissions$extra_urban_metric>80 & !is.na(emissions$extra_urban_metric))
w<-summary(lm1)$coef[1]

newVal<-(emissions$combined_metric[i]-w*emissions$urban_metric[i])/(1-w)

#5
par(mar=c(5,5,5,5))
plot(log(emissions$urban_imperial)~log(emissions$urban_metric))

#7

aggregate(emissions$noise_level ,list( emissions$manufacturer), mean)

min(aggregate(emissions$noise_level ,list( emissions$manufacturer), mean)[2],na.rm=TRUE)

#8

lm2<-lm(co2~engine_capacity,data=emissions)
summary(lm2)
a=lm2$coeff[1]+lm2$coefficients[2]*4000
a