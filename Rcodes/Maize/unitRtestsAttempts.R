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

load("Main/CrMaize13.RData")
load("Main/CrMaize11.RData")

rm(CrMaize13ts)
CrMaize13ts<-pdata.frame(CrMaize13,index=c("ID","Year"))
CrMaize8<-pdata.frame(CrMaize8,index=c("ID","Year"))
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

boxplot(Yield~Year,data=CrMaize13ts)
boxplot(lag(Yield)~Year,data=CrMaize13ts)


plot(aggregate(Yield ~ Year, CrMaize13ts, sd))
plot(diff(CrMaize13ts$Yield)~CrMaize13ts$Year)

adf.test(CrMaize13ts$Yield) #not sure if  this work for panels
purtest(CrMaize13ts$Yield)  
ur.df(CrMaize13$Yield)  
rowSums(table(CrMaize13ts$ID,CrMaize13ts$Year))


purtest(CrMaize130ts$Yield)  
# I probably need to get it balanced
rowSums(table(CrMaize13ts$ID,CrMaize13ts$Year))
colSums(table(CrMaize13ts$ID,CrMaize13ts$Year))

rm(idids)
idids<-unique(CrMaize13ts$ID)[which(rowSums(table(CrMaize13ts$ID,CrMaize13ts$Year))>25)]

rm(CrMaize130ts)
CrMaize130ts<-subset(CrMaize13ts,CrMaize13ts$ID %in% idids )
unique(CrMaize130ts$ID)
CrMaize130ts$ID<-as.character(CrMaize130ts$ID)
unique(CrMaize130ts$ID)
CrMaize130ts$ID<-as.factor(CrMaize130ts$ID)
unique(CrMaize130ts$ID)



setdiff(unique(CrMaize13ts$ID),unique(CrMaize130ts$ID))
length(setdiff(unique(CrMaize13ts$ID),unique(CrMaize130ts$ID)))

unique(CrMaize130ts$ID)
sum(is.na(CrMaize130ts$Yield[CrMaize130ts$ID ==6 ]))

rowSums(table(CrMaize130ts$ID,CrMaize130ts$Year))
colSums(table(CrMaize130ts$ID,CrMaize130ts$Year))



rowSums(table(CrMaize13ts$ID[CrMaize13ts$Year %in% 1978:2014],CrMaize13ts$Year[CrMaize13ts$Year %in% 1978:2014]))
length(unique(CrMaize13ts$Year[CrMaize13ts$Year %in% 1978:2014]))

MyIDS<-CrMaize13ts$ID[which(is.na(CrMaize13ts$Yield)==FALSE & CrMaize13ts$Year ==1970)    ]
