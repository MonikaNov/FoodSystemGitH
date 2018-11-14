rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)
library('plm')
library('dplyr')
library('tseries')
library('urca')
options(max.print=10000)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

load("Main/CrMaize14.RData")
load("Main/CrMaize13.RData")

rm(CrMaize13ts)
CrMaize13ts<-pdata.frame(CrMaize13,index=c("ID","Year"))
rm(CrMaize14ts)   # CrMaize14 is the same as CrMaize13,  just without the one dimensional outliers in terms of yield
CrMaize14ts<-pdata.frame(CrMaize14,index=c("ID","Year"))
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

boxplot(Yield~Year,data=CrMaize14ts)
boxplot(lag(Yield)~Year,data=CrMaize14ts)


plot(aggregate(Yield ~ Year, CrMaize14ts, sd))
plot(diff(CrMaize14ts$Yield)~CrMaize14ts$Year)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# do the unit root test for the average yields? I am not sure what will happen to the DF thought. but...
# first I need to get the average yield..


MTtotal<-aggregate(CrMaize14$MT, by=list(CrMaize14$Year), FUN=sum)
names(MTtotal)[1]<-"Year"
names(MTtotal)[2]<-"MT"
AreaTotal<-aggregate(CrMaize14ts$Area, by=list(CrMaize14ts$Year), FUN=sum)
names(AreaTotal)[1]<-"Year"
names(AreaTotal)[2]<-"Area"
rm(YieldTotal)
YieldTotal<-MTtotal
YieldTotal$MT<-MTtotal$MT/AreaTotal$Area
names(YieldTotal)[1]<-"Year"
names(YieldTotal)[2]<-"Yield"

adf.test(YieldTotal$Yield)
ur.df(YieldTotal$Yield)  

# just to test the unit root test:-)

adf.test(   2*(seq(1,100,1)+1.2))
adf.test(   2*(seq(1,100,2)+rnorm(50)))
adf.test(rnorm(500))   #  TAKZE, POKUD ZAMITNUTO, JE STACIONARNI A VICA VERSA !!!!!!!!

# TAKZE TO VYPADA, ZE NENI STACIONARNI, JE UNIT ROOT TEST


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#
# now get a subset which will be balanced

rowSums(table(CrMaize14ts$ID,CrMaize14ts$Year))

rm(UnitRootData)
UnitRootData<-droplevels(CrMaize14[CrMaize14$Year %in% 1981:2014,c(1,7,11,13) ])

table(UnitRootData$ID,UnitRootData$Year)
rowSums(table(UnitRootData$ID,UnitRootData$Year))
colSums(table(UnitRootData$ID,UnitRootData$Year))


MeanYr<-aggregate(x=(Yld=UnitRootData$Yield), by=list(Year=UnitRootData$Year),FUN=mean)
MeanID<-aggregate(x=(Yld=UnitRootData$Yield), by=list(ID=UnitRootData$ID),FUN=mean)
MeanYr; MeanID


names(MeanYr)[2]<-"YearAverage"
names(MeanID)[2]<-"IDaverage"

rm(MeanYrID)
MeanYrID<-data.frame(Year=rep(MeanYr$Year, rep(47,34)) , ID=rep(MeanID$ID,34),YearAverage=rep(MeanYr$YearAverage, rep(47,34)) )

rm(MeanYearID)
MeanYearID<-merge(MeanYrID,MeanID)
MeanYearID<-MeanYearID[with(MeanYearID,order(ID,Year)),  ]
head(MeanYearID)


rm(UnitRData)
UnitRData<-merge(UnitRootData,MeanYearID, all=TRUE)
head(UnitRData)
summary(UnitRData)

#-------------------------------------------------------------------------------------------------------
MaizeBalanced<-pdata.frame(UnitRData,index=c("ID","Year"))


rowSums(table(MaizeBalanced$ID,MaizeBalanced$Year))
colSums(table(MaizeBalanced$ID,MaizeBalanced$Year))

MaizeBalanced$Yield2<-MaizeBalanced$Yield


rowSums(table(UnitRootData$ID,UnitRootData$Year))
colSums(table(UnitRootData$ID,UnitRootData$Year))

MaizeBalanced$Yield2[is.na(MaizeBalanced$Yield2)==TRUE & MaizeBalanced$Year == 1996]<-MaizeBalanced$IDaverage[is.na(MaizeBalanced$Yield2)==TRUE & MaizeBalanced$Year == 1996]

MaizeBalanced$Yield2[is.na(MaizeBalanced$Yield2)==TRUE]<-MaizeBalanced$YearAverage[is.na(MaizeBalanced$Yield2)==TRUE ]
summary(MaizeBalanced)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# now some plots to see how does the avregae stands for..

i<-27
axis(1,as.character(MaizeBalanced$Year[MaizeBalanced$ID %in% i]))
plot(MaizeBalanced$Yield2[MaizeBalanced$ID %in% i],xaxt="n",type="l",col="blue")
plot(MaizeBalanced$Yield[MaizeBalanced$ID %in% i],xaxt="n",type="l")

  axis(1,MaizeBalanced$Year[MaizeBalanced$ID %in% i],at = 1:34)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


purtest(MaizeBalanced$Yield2, test = c("levinlin", "ips", "madwu", "Pm" , "invnormal", "logit", "hadri"))  

purtest(MaizeBalanced$Yield2, test = c("ips"))  
         purtest(MaizeBalanced$Yield2, test = c("ips"),exo=c("intercept"))  
         purtest(MaizeBalanced$Yield2, test = c("ips"),exo=c("trend"))  

purtest(MaizeBalanced$Yield2, test = c("madwu"))   
      purtest(MaizeBalanced$Yield2, test = c("madwu"),exo=c("intercept"))  
      purtest(MaizeBalanced$Yield2, test = c("madwu"),exo=c("trend"))  

purtest(MaizeBalanced$Yield2, test = c("Pm"))  
    purtest(MaizeBalanced$Yield2, test = c("Pm"),exo=c("intercept"))  
    purtest(MaizeBalanced$Yield2, test = c("Pm"),exo=c("trend"))  

purtest(MaizeBalanced$Yield2, test = c("invnormal"))
  purtest(MaizeBalanced$Yield2, test = c("invnormal"),exo=c("intercept"))  
  purtest(MaizeBalanced$Yield2, test = c("invnormal"),exo=c("trend"))  

purtest(MaizeBalanced$Yield2, test = c("logit"))  
  purtest(MaizeBalanced$Yield2, test = c("logit"),exo=c("intercept"))  
  purtest(MaizeBalanced$Yield2, test = c("logit"),exo=c("trend"))  

purtest(MaizeBalanced$Yield2, test = c("hadri"))  
  purtest(MaizeBalanced$Yield2, test = c("hadri"),exo=c("intercept"))  
  purtest(MaizeBalanced$Yield2, test = c("hadri"),exo=c("trend"))  

  
  
#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# mozna zkusit jen pozdejsi sub-soubor, tak aby tam byla min NA's


purtest(subset(MaizeBalanced, Year %in% 1991:2014)$Yield2, test = c("levinlin"))  
purtest(subset(MaizeBalanced, Year %in% 1991:2014)$Yield2, test = c("ips"),exo=c("intercept"))  
purtest(subset(MaizeBalanced, Year %in% 1991:2014)$Yield2, test = c("ips"),exo=c("trend"))    


purtest(subset(MaizeBalanced, Year %in% 1991:2014)$Yield2, test = c("madwu"))   
purtest(subset(MaizeBalanced, Year %in% 1991:2014)$Yield2, test = c("madwu"),exo=c("intercept"))  
purtest(subset(MaizeBalanced, Year %in% 1991:2014)$Yield2, test = c("madwu"),exo=c("trend"))  

purtest(subset(MaizeBalanced, Year %in% 1991:2014)$Yield2, test = c("Pm"))  
purtest(subset(MaizeBalanced, Year %in% 1991:2014)$Yield2, test = c("Pm"),exo=c("intercept"))  
purtest(subset(MaizeBalanced, Year %in% 1991:2014)$Yield2, test = c("Pm"),exo=c("trend"))  

purtest(subset(MaizeBalanced, Year %in% 1991:2014)$Yield2, test = c("invnormal"))
purtest(subset(MaizeBalanced, Year %in% 1991:2014)$Yield2, test = c("invnormal"),exo=c("intercept"))  
purtest(subset(MaizeBalanced, Year %in% 1991:2014)$Yield2,exo=c("trend"))  

purtest(MaizeBalanced$Yield2, test = c("logit"))  
purtest(MaizeBalanced$Yield2, test = c("logit"),exo=c("intercept"))  
purtest(MaizeBalanced$Yield2, test = c("logit"),exo=c("trend"))  

purtest(MaizeBalanced$Yield2, test = c("hadri"))  
purtest(MaizeBalanced$Yield2, test = c("hadri"),exo=c("intercept"))  
purtest(MaizeBalanced$Yield2, test = c("hadri"),exo=c("trend"))  
    
# a pak jednotlive rady..
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


adf.test(CrMaize14ts$Yield) #not sure if  this work for panels
ur.df(CrMaize14$Yield)  

rm(adftests)
adftests<- sapply(unique(CrMaize14ts$ID),  function(x)    rbind(as.numeric(as.character(x)),adf.test(CrMaize14ts$Yield[CrMaize14ts$ID==x])[4]),simplify="matrix" )
adftests<-data.frame(t(adftests))
names(adftests)<-c("ID","pvalue")
adftests

adf.test(CrMaize14ts$Yield[CrMaize14ts$ID==53])  # to check

length(which(adftests$pvalue<0.01))
length(which(adftests$pvalue<0.05  & adftests$pvalue>=0.01 ))
length(which(adftests$pvalue<0.1  & adftests$pvalue>=0.05 ))
length(which(adftests$pvalue<0.15  & adftests$pvalue>=0.1 ))
length(which(adftests$pvalue>=0.15 ))

length(which(adftests$pvalue>=0.05 ))

#following are the counties, which are probably stationary (no unit root)

adftests$ID[adftests$pvalue<0.05 ]  # Marsabit, Meru, Kissi, Kakamega, Elgeyo Marakwet, Migori
adftests$ID[adftests$pvalue<0.1  & adftests$pvalue>=0.05 ]  # Kilifi, Samburu, Kericho, Wajir, Makueni, Kiambu

adftests$ID[adftests$pvalue>0.5 ]  

unique(CrMaize14ts[, c("ID","county")])

#------------------------------
# Plots ?

i<-19
axis(1,as.character(MaizeBalanced$Year[MaizeBalanced$ID %in% i]))
plot(MaizeBalanced$Yield2[MaizeBalanced$ID %in% i],xaxt="n",type="l",col="blue")
plot(MaizeBalanced$Yield[MaizeBalanced$ID %in% i],xaxt="n",type="l")

axis(1,MaizeBalanced$Year[MaizeBalanced$ID %in% i],at = 1:34)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#  testing the function  

  adf.test(   2*(seq(1,100,1)+1.2))
  rm(test_frame)
  test_frame<-data.frame( cbind( 2*(seq(1,1000,1)+0.2+rnorm(1000)),2.05*(seq(1,1000,1)+0.13  +rnorm(1000)),Year=1021:2020,ID=rep(1:5,  rep(200,5)  )  ))
  test_frame<-pdata.frame(test_frame,index=c("ID","Year"))
purtest(test_frame$V1)
  
  adf.test(   2*(seq(1,100,2)+rnorm(50)))
  adf.test(rnorm(500)) 