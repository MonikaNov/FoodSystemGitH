rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)
library('plm')
library('tseries')

options(max.print=10000)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

load("Main/CrMaize14.RData")
load("Main/CrMaize13.RData")

rm(CrMaize13ts)
CrMaize13ts<-pdata.frame(CrMaize13,index=c("ID","Year"))
rm(CrMaize14ts)   # CrMaize14 is the same as CrMaize13,  just without the one dimensional outliers in terms of yield
CrMaize14ts<-pdata.frame(CrMaize14,index=c("ID","Year"))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
rm(CountyNames)
CountyNames<-as.character(unique(CrMaize14ts$county))[-3]

j<-1
for (i in unique(CrMaize14ts$ID))
{ png(filename=paste("/its/home/mn301/foodSystems/Rcodes/Maize/plotsYieldCounties/",CountyNames[j],".png",sep=""))
plot(Yield~ Year, data = CrMaize14ts[which(CrMaize14ts$ID==i),]
     ,ylab=c("Yield in tonnes per hectar"), main=c(CountyNames[j]))
dev.off()
j<-j+1}

#--------------------------
# now checking

unique(CrMaize14ts[c(1,13)])
plot(CrMaize14ts$Yield[which(CrMaize14ts$ID==36)]~CrMaize14ts$Year[which(CrMaize14ts$ID==36)] ,ylab=c("Yield in tonnes per hectar"))
plot(CrMaize14ts$Yield[which(CrMaize14ts$ID==6)] ,ylab=c("Yield in tonnes per hectar"))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# t-tests of trend

CrMaize14ts$t<-as.numeric(as.character(CrMaize14ts$Year))-1969
Ultravox<-lmer(Yield~t +  (t|ID),data=CrMaize14ts)
summary(Ultravox)

rm(CountyModels)
CountyModels<-lapply(unique(CrMaize14ts$ID),function(x) lm(Yield~t,data=CrMaize14ts[CrMaize14ts$ID==x,])  )


TrendEst<-sapply(CountyModels,function(x) summary(x)$coefficients[2,c(1,4)])
TrendEst2<-sapply(1:47,function(x) summary(CountyModels[[x]])$coefficients[2,c(1,4)])  # just to experiment and learn...
all.equal(TrendEst,TrendEst2)

#--------------------------
# now checking
unique(CrMaize14ts[c(1,13)])
summary(lm(Yield~t,data=CrMaize14ts[CrMaize14ts$ID==53,]))
which(unique(CrMaize14ts[c(1,13)])$ID %in% 53 )  # to find out order of the particular ID. 
# I need to substract 1 if higher than three, because Muranga has two names, so it appears in the unique..twice

TrendEst[,38]
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# now I have to sort out positive, negative, significant and insignificant

NegSig<-which(TrendEst[1,]<0  & TrendEst[2,]<0.05)
NegInsig<-which(TrendEst[1,]<0  & TrendEst[2,]>=0.05)
PosSig<-which(TrendEst[1,]>0  & TrendEst[2,]<0.05)
PosInsig<-which(TrendEst[1,]>0  & TrendEst[2,]>=0.05)

intersect(PosSig, PosInsig)  #checking...
intersect(PosSig, NegSig)
intersect(PosInsig, NegSig)


CrMaize14ts[!duplicated(CrMaize14ts[13]),c(1,13)][NegSig,]
CrMaize14ts[!duplicated(CrMaize14ts[13]),c(1,13)][NegInsig,]
CrMaize14ts[!duplicated(CrMaize14ts[13]),c(1,13)][PosInsig,]

nrow(CrMaize14ts[!duplicated(CrMaize14ts[13]),c(1,13)][NegSig,])
nrow(CrMaize14ts[!duplicated(CrMaize14ts[13]),c(1,13)][NegInsig,])
nrow(CrMaize14ts[!duplicated(CrMaize14ts[13]),c(1,13)][PosInsig,])