rm(list=ls())

WDuni<-c("/home/m/mn/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/sussex/US/paper3") # doma

setwd(WDuni)
setwd(WDhome)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(plm)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# getting data
Crops<-read.csv( "New31Jan2018/KE_Ag_Stats_2014.csv",header=TRUE, na.strings = c("NA","NC","NC.","-","#DIV/0!"),colClasses=c("factor","factor","factor","factor","factor","factor","factor","factor","numeric","numeric","numeric"))  #numeric,numeric,numeric))
# two really small values which appear as zero: 1. Sorghum, Machakos, 1970 - Yield, 2. Maize, Migori, 1995, Yield. Nothing else:

Crops[(Crops$Admin2== "Machakos" & Crops$Year==1970 & Crops$Crop=="Sorghum" ),]
Crops[(Crops$Admin2== "Migori" & Crops$Year==1995 & Crops$Crop=="Maize" ),]

Crops$Yield[(Crops$Admin2== "Machakos" & Crops$Year==1970 & Crops$Crop=="Sorghum" )]<-0.0038
Crops$Yield[(Crops$Admin2== "Migori" & Crops$Year==1995 & Crops$Crop=="Maize" )]<-0.0027

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

CrMaize10<-Crops
CrMaize10$county<-as.character(Crops$Admin2)
CrMaize10$county[CrMaize10$Admin1=="Nairobi"]<-"Nairobi"
CrMaize10$county<-as.factor(CrMaize10$county)
CrMaize11<-subset(CrMaize10, Crop=="Maize" & !county=="")
# maybe also drop the zeros here


CrMaize12<-subset(CrMaize11, Yield>0 )
all.equal(CrMaize12[1:6,],CrMaize11[1:6,])
#+++++++++++++++++++++++++

# to compare with what I analysed before:
CrMaize<-subset(Crops, Crop=="Maize" & !Admin2=="")
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


Short<-subset(CrMaize11, CrMaize11$Season=='Short Rains')
Long<-subset(CrMaize11, CrMaize11$Season=='Long Rains')
Total<-subset(CrMaize11, CrMaize11$Season=='')
Total$ID <- seq.int(nrow(Total))  # create identifier variable for total.


nrow(unique(Short[c('Year','county')]))
nrow(unique(Long[c('Year','county')]))
nrow(unique(Total[c('Year','county')]))

table(Crops$Crop,Crops$Season)
table(CrMaize$Season)
table(CrMaize11$Season)

# now if a row which is in Long (or Short) is also in Total, I will give it same ID as the row has in total

rm(Long2)
Long2<-merge(Long,Total[c(6,12,13)],all.x=TRUE,sort=FALSE,by=c("county","Year"))

Long2[duplicated(Long2[c(1,2)]),]
Long2[c(2,4,5),] 
Long3<-Long2[-c(2,4,5),] # duplicated from different sources. Thats why I get three more observations
Long3[duplicated(Long3[c(1,2)]),]

Long3<-Long3[with(Long3, order(county, Year)), ]
Long<-Long[with(Long, order(county, Year)), ]
all.equal(Long3[c(3:7,2,8:12,1)],Long)


# now the same for short
rm(Short2)
Short2<-merge(Short,Total[c(6,12,13)],all.x=TRUE,sort=FALSE,by=c("county","Year"))

Short2[duplicated(Short2[c(1,2)]),]
Short2[c(2,4,5),] 
Short3<-Short2[-c(2,4,5),] # duplicated from different sources. Thats why I get three more observations
Short3[duplicated(Short3[c(1,2)]),]

Short3<-Short3[with(Short3, order(county, Year)), ]
Short<-Short[with(Short, order(county, Year)), ]
all.equal(Short3[c(3:7,2,8:12,1)],Short)

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#test if merging and attaching the ID has been done correctly

Long3[c(400:407),]
Long[Long$county=="Tana River",]
Total[Total$county=="Tana River" & Total$Year %in% c(1996:2001),]
Short3[c(201:205),]
Short[Short$county=="Nyamira"& Short$Year %in% c(2001),]
Total[Total$county=="Nyamira" & Total$Year %in% c(2001),]

# cool, looks good
summary(Total)



#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Long<-Long3
Short<-Short3

rm(list=c("Long2","Long3","Short2","Short3"))


# Now in the subset Total, I will create a variable for short and long rains and match it based on the ID with the Short and Long subsets


Total$AreaShort<-NA
Total$YieldShort<-NA
Total$MTShort<-NA

Total$AreaLong<-NA
Total$YieldLong<-NA
Total$MTLong<-NA



for (i in 1:nrow(Total))
{
  if (Total$ID[i] %in% Short$ID ){
    Total$AreaShort[i]<-Short$Area[which(Short$ID==Total$ID[i]) ]
    Total$YieldShort[i]<-Short$Yield[which(Total$ID[i]==Short$ID) ]
    Total$MTShort[i]<-Short$MT[which(Total$ID[i]==Short$ID) ]
  }
}





for (i in 1:nrow(Total))
{
  if (Total$ID[i] %in% Long$ID ){
    Total$AreaLong[i]<-Long$Area[which(Long$ID==Total$ID[i]) ]
    Total$YieldLong[i]<-Long$Yield[which(Total$ID[i]==Long$ID) ]
    Total$MTLong[i]<-Long$MT[which(Total$ID[i]==Long$ID) ]
  }
}

# checkings...

summary(Total)
summary(Long)
summary(Short)

Short[Short$county=="Bungoma"& Short$Year %in% c(1996:2001),]
Total[Total$county=="Bungoma" & Total$Year %in% c(1996:2001),]

Long[Long$county=="Siaya"& Long$Year %in% c(1997:2000),]
Total[Total$county=="Siaya" & Total$Year %in% c(1997:2000),]

# Tests, checks ok. Now I will test whether: long season + short season == ca season undefined (just for MT, obviously :-)

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# 1 Yield

plot(Total$Yield,Total$YieldLong)
plot(Total$Yield,Total$YieldShort)
plot(Total$YieldLong,Total$YieldShort)

summary(Total$Yield-Total$YieldLong)
plot(Total$Yield-Total$YieldLong)
boxplot(Total$Yield-Total$YieldLong)


summary(Total$YieldShort-Total$YieldLong)
plot(Total$YieldShort-Total$YieldLong)
boxplot(Total$YieldShort-Total$YieldLong)
hist(Total$YieldShort-Total$YieldLong)
hist((Total$YieldShort-Total$YieldLong)/Total$YieldLong,80)
hist(100*(Total$YieldShort-Total$YieldLong)/Total$YieldLong,80)
summary((Total$YieldShort-Total$YieldLong)/Total$YieldLong)
boxplot((Total$YieldShort-Total$YieldLong)/Total$YieldLong)
sum(is.na((Total$YieldShort-Total$YieldLong)/Total$YieldLong))

Total$YieldLongDiff<-(Total$Yield-Total$YieldLong)/Total$Yield
summary(Total$YieldLongDiff)
#median a nd mean rel.ok
plot(Total$YieldLongDiff)
boxplot(Total$YieldLongDiff)
boxplot(Total$YieldLongDiff[which(Total$YieldLongDiff>0)])
boxplot(Total$YieldLongDiff[Total$YieldLongDiff>0])


hist(Total$YieldLongDiff,50)
hist(Total$YieldLongDiff,450, xlim=c(0,1))



summary(Total$YieldLongDiff[which(is.na(Total$YieldShort)==TRUE   )])
hist(Total$YieldLongDiff[which(is.na(Total$YieldShort)==TRUE   )],50)
table((Total$YieldLongDiff[which(is.na(Total$YieldShort)==TRUE   )]))
table(Total$YieldLongDiff)

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# 2 AREA


plot(Total$Area,Total$AreaLong)
plot(Total$Area,Total$AreaShort)
plot(Total$AreaLong,Total$AreaShort)

summary(Total$Area-Total$AreaLong)
plot(Total$Area-Total$AreaLong)
boxplot(Total$Area-Total$AreaLong)


summary(Total$AreaShort-Total$AreaLong)
plot(Total$AreaShort-Total$AreaLong)
boxplot(Total$AreaShort-Total$AreaLong)
hist(Total$AreaShort-Total$AreaLong)
hist((Total$AreaShort-Total$AreaLong)/Total$AreaLong,80)
summary((Total$AreaShort-Total$AreaLong)/Total$AreaLong)
boxplot((Total$AreaShort-Total$AreaLong)/Total$AreaLong)
sum(is.na((Total$AreaShort-Total$AreaLong)/Total$AreaLong))

Total$AreaLongDiff<-(Total$Area-Total$AreaLong)/Total$Area  # not sure if difference in area or yields between total and one of the seasons actually makes sense..
summary(Total$AreaLongDiff)
#median a nd mean rel.ok
plot(Total$AreaLongDiff)
boxplot(Total$AreaLongDiff)
hist(Total$AreaLongDiff,50)

summary(Total$AreaLongDiff[which(is.na(Total$AreaShort)==TRUE   )])
hist(Total$AreaLongDiff[which(is.na(Total$AreaShort)==TRUE   )],50)
table((Total$AreaLongDiff[which(is.na(Total$AreaShort)==TRUE   )]))
table(Total$AreaLongDiff)

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# 3 MT


Total$MTsum<-Total$MTLong+Total$MTShort
hist(Total$MT-Total$MTsum,50)
Total$MTres<-Total$MT-Total$MTsum
summary((Total$MTres))
plot(na.omit(Total$MTres))
table(na.omit(Total$MTres))
summary((Total$MT-Total$MTsum))

Total[which(Total$MTres< -100 |Total$MTres>100),  ]
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plot(Total$MT,Total$MTLong)
plot(Total$MT,Total$MTShort)
plot(Total$MTLong,Total$MTShort)

summary(Total$MT-Total$MTLong)
plot(Total$MT-Total$MTLong)
boxplot(Total$MT-Total$MTLong)


summary(Total$MTShort-Total$MTLong)
plot(Total$MTShort-Total$MTLong)
boxplot(Total$MTShort-Total$MTLong)
hist(Total$MTShort-Total$MTLong)
hist((Total$MTShort-Total$MTLong)/Total$MTLong,80)
summary((Total$MTShort-Total$MTLong)/Total$MTLong)
boxplot((Total$MTShort-Total$MTLong)/Total$MTLong)
sum(is.na((Total$MTShort-Total$MTLong)/Total$MTLong))

Total$MTLongDiff<-(Total$MT-Total$MTLong)/Total$MT
summary(Total$MTLongDiff)
#median a nd mean rel.ok
plot(Total$MTLongDiff)
boxplot(Total$MTLongDiff)
boxplot(Total$MTLongDiff[which(Total$MTLongDiff>0)])
boxplot(Total$MTLongDiff[Total$MTLongDiff>0])


hist(Total$MTLongDiff,50)
hist(Total$MTLongDiff,450, xlim=c(0,1))



summary(Total$MTLongDiff[which(is.na(Total$MTShort)==TRUE   )])
hist(Total$MTLongDiff[which(is.na(Total$MTShort)==TRUE   )],50)
table((Total$MTLongDiff[which(is.na(Total$MTShort)==TRUE   )]))
table(Total$MTLongDiff)
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
uniq<-unique(CrMaize[,c('Admin2','Year','Season')])
uniqAll<-unique(Crops[,c('Admin2','Year','Season','Crop')])
which(duplicated(Crops[,c('Year','Admin1','Admin2','Year','Season','Crop')]))
Crops[which(duplicated(Crops[,c('Year','Admin1','Admin2','Year','Season','Crop')])),]

# I need to create a unique identifier from year and season which I will use as a time index in panel. ACTUALLY I DONT NEED.year can probably work as an index as it is.
# Actually, I need to combine the seasons as for the most counties I only have one. or exclude them..UPDATE: I need to exclude cases with short season and long season. Only keep
    # those without defined season. (Long and short season rows usualy also have a row with no season- which is probably their sum)
