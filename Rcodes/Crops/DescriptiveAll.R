rm(list=ls())

WDuni<-c("/home/m/mn/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Crops<-read.csv( "New31Jan2018/KE_Ag_Stats_2014.csv",header=TRUE, na.strings = c("NA","NC","NC.","-","#DIV/0!"),colClasses=c("factor","factor","factor","factor","factor","factor","factor","factor","numeric","numeric","numeric"))  #numeric,numeric,numeric))

# two really small values which appear as zero: 1. Sorghum, Machakos, 1970 - Yield, 2. Maize, Migori, 1995, Yield. Nothing else:
Crops$Yield[(Crops$Admin2== "Machakos" & Crops$Year==1970 & Crops$Crop=="Sorghum" )]<-0.0038
Crops$Yield[(Crops$Admin2== "Migori" & Crops$Year==1995 & Crops$Crop=="Maize" )]<-0.0027

#--------------------------------------------------------------------------------------------------------------

table(Crops$Year,Crops$Crop)

barplot(table(Crops$Year,Crops$Crop))

Crops[Crops$Area==0 & Crops$Crop=="Maize",] # carefully here. 0 mean no data-->look at this, which years are they from and remove probably
# Is there some pattern in the zeros, which actually means no data?

table(Crops$Season[Crops$Yield==0])
table(Crops$Season[Crops$MT==0])
table(Crops$Season[Crops$Area==0])

Crops[Crops$Area==0 & (Crops$Yield>0 | Crops$MT>0),]

(Crops[Crops$Area==0 & (Crops$Yield>0 | Crops$MT>0) & is.na(Crops$Yield)=="FALSE" & is.na(Crops$Area)=="FALSE"& is.na(Crops$MT)=="FALSE"  ,])
nrow(Crops[Crops$Area==0 & (Crops$Yield>0 | Crops$MT>0) & is.na(Crops$Yield)=="FALSE" & is.na(Crops$Area)=="FALSE"& is.na(Crops$MT)=="FALSE"  ,])

(Crops[Crops$Yield==0 & (Crops$Area>0 | Crops$MT>0) & is.na(Crops$Yield)=="FALSE" & is.na(Crops$Area)=="FALSE"& is.na(Crops$MT)=="FALSE"  ,])
nrow(Crops[Crops$Yield==0 & (Crops$Area>0 | Crops$MT>0) & is.na(Crops$Yield)=="FALSE" & is.na(Crops$Area)=="FALSE"& is.na(Crops$MT)=="FALSE"  ,])


(Crops[Crops$Yield==0 & ( Crops$MT>0) & is.na(Crops$Yield)=="FALSE" & is.na(Crops$Area)=="FALSE"& is.na(Crops$MT)=="FALSE"  ,])
nrow(Crops[Crops$Yield==0 & ( Crops$MT>0) & is.na(Crops$Yield)=="FALSE" & is.na(Crops$Area)=="FALSE"& is.na(Crops$MT)=="FALSE"  ,])


table(Crops$Year[Crops$Area==0 & Crops$Crop=="Maize"])
table(Crops$Year[!Crops$Area==0 & Crops$Crop=="Maize"])


table(droplevels(Crops$Year[Crops$Area==0 & Crops$Crop=="Maize"]),droplevels(Crops$Admin2[Crops$Area==0 & Crops$Crop=="Maize"]))

table(droplevels(Crops$Admin2[Crops$Area==0 & Crops$Crop=="Maize"]),droplevels(Crops$Year[Crops$Area==0 & Crops$Crop=="Maize"]))

table(droplevels(Crops$Year[Crops$Area==0 & Crops$Crop=="Maize"]))
table(droplevels(Crops$Admin2[Crops$Area==0 & Crops$Crop=="Maize"]))
table(droplevels(Crops$Year[Crops$Yield==0 & Crops$Crop=="Maize"]))
table(droplevels(Crops$Admin2[Crops$Yield==0 & Crops$Crop=="Maize"]))
table(droplevels(Crops$Year[Crops$MT==0 & Crops$Crop=="Maize"]))

table(droplevels(Crops$Year[Crops$Area==0 & Crops$Crop=="Maize"]))
table(droplevels(Crops$Admin2[Crops$Area==0 & Crops$Crop=="Maize"]))
table(droplevels(Crops$Year[Crops$Yield==0 & Crops$Crop=="Maize"]))
table(droplevels(Crops$Admin2[Crops$Yield==0 & Crops$Crop=="Maize"]))
table(droplevels(Crops$Year[Crops$MT==0 & Crops$Crop=="Maize"]))

# nice bar to see how many zero yields are and MT:
barplot(rbind(table(Crops$Year[Crops$Yield==0 & Crops$Crop=="Maize"]),table(Crops$Year[!Crops$Yield==0 & Crops$Crop=="Maize"])))
barplot(rbind(table(Crops$Year[Crops$Area==0 & Crops$Crop=="Maize"]),table(Crops$Year[!Crops$Area==0 & Crops$Crop=="Maize"])))
barplot(rbind(table(Crops$Year[Crops$MT==0 & Crops$Crop=="Maize"]),table(Crops$Year[!Crops$MT==0 &( Crops$Crop=="Maize")])))


barplot(rbind(table(Crops$Year[Crops$Yield==0 & Crops$Crop=="Maize"]),table(Crops$Year[!Crops$Yield==0 ])))
barplot(rbind(table(Crops$Year[Crops$Area==0 & Crops$Crop=="Maize"]),table(Crops$Year[!Crops$Area==0 ])))
barplot(rbind(table(Crops$Year[Crops$MT==0 & Crops$Crop=="Maize"]),table(Crops$Year[!Crops$MT==0 ])))

barplot(rbind(table(Crops$Year[Crops$Yield==0 & !Crops$Crop=="Maize"]),table(Crops$Year[!Crops$Yield==0 & !Crops$Crop=="Maize"])))
barplot(rbind(table(Crops$Year[Crops$Area==0 & !Crops$Crop=="Maize"]),table(Crops$Year[!Crops$Area==0 & !Crops$Crop=="Maize"])))
barplot(rbind(table(Crops$Year[Crops$MT==0 & !Crops$Crop=="Maize"]),table(Crops$Year[!Crops$MT==0 & !Crops$Crop=="Maize"])))

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# how much did I mess up by not including Nairobi?

subset(Crops, Crop=="Maize" & Admin2=="") # seems that I had excluded Nairobi before, although I didn't have to. Include now..

# how much did I mess up by not including Nairobi?

rm(CrMaize10)
CrMaize10<-Crops
CrMaize10$county<-as.character(Crops$Admin2)
CrMaize10$county[CrMaize10$Admin1=="Nairobi"]<-"Nairobi"

CrMaize10$county<-as.factor(CrMaize10$county)

#-----------------

summary(CrMaize10[CrMaize10$county=="Nairobi",])
summary(CrMaize10[! CrMaize10$county=="Nairobi",])

hist(CrMaize10$Yield[CrMaize10$county=="Nairobi"],,xlim=c(0,40),1000)
hist(CrMaize10$Yield[! CrMaize10$county=="Nairobi"],xlim=c(0,40),1000)

boxplot(CrMaize10$Yield[CrMaize10$county=="Nairobi"])
boxplot(CrMaize10$Yield[! CrMaize10$county=="Nairobi"])

summary(CrMaize10$Yield[CrMaize10$county=="Nairobi"])
summary(CrMaize10$Yield[! CrMaize10$county=="Nairobi"])

t.test(CrMaize10$Yield[! CrMaize10$county=="Nairobi"],CrMaize10$Yield[ CrMaize10$county=="Nairobi"])

# probably not so bad, t.test insignificant
rm(CrMaize11)
CrMaize11<-subset(CrMaize10, Crop=="Maize" & !county=="")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# and save:
rm(list=setdiff(ls(),"CrMaize11"))
save.image("Main/CrMaize11.RData")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


boxplot(CrMaize11$Yield~CrMaize11$Year,ylab=c("Yield"))
hist(CrMaize11$Yield,100,xlab=c("Yield"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

CrMaize12<-subset(CrMaize11, Yield>0 )

# and save:
rm(list=setdiff(ls(),"CrMaize12"))
save.image("Main/CrMaize12.RData")