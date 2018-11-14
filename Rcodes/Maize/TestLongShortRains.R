rm(list=ls())

WDuni<-c("/home/m/mn/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/sussex/US/paper3") # doma

setwd(WDuni)
setwd(WDhome)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(plm)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Crops<-read.csv( "New31Jan2018/KE_Ag_Stats_2014.csv",header=TRUE, na.strings = c("NA","NC","NC.","-","#DIV/0!"),colClasses=c("factor","factor","factor","factor","factor","factor","factor","factor","numeric","numeric","numeric"))  #numeric,numeric,numeric))
summary(Crops)


CrMaize<-subset(Crops, Crop=="Maize" & !Admin2=="")

nrow(CrMaize[duplicated(CrMaize[,c('Year','Admin2')]),])

Short<-subset(CrMaize, CrMaize$Season=='Short Rains')
Long<-subset(CrMaize, CrMaize$Season=='Long Rains')
Total<-subset(CrMaize, CrMaize$Season=='')

unique(Short[c('Year','Admin2')])
unique(Long[c('Year','Admin2')])
unique(Total[c('Year','Admin2')])

nrow(unique(Short[c('Year','Admin2')]))
nrow(unique(Long[c('Year','Admin2')]))
nrow(unique(Total[c('Year','Admin2')]))

table(Crops$Crop,Crops$Season)
table(CrMaize$Season)

# first create identifier variable for total.
Total$ID <- seq.int(nrow(Total))

# now if a row which is in Long (or Short) is also in Total, I will give it same ID as the row has in total
Long$ID<-NULL
Long$ID <-NA

for (i in 1:nrow(Long))
{
if (Long$Admin2[i] %in% Total$Admin2 ){
  if
  (Long$Year[i] %in% Total$Year[which(Total$Admin2  == Long$Admin2[i])])

  Long$ID[i]<-Total$ID[which(Long$Year[i]==Total$Year & Long$Admin2[i]==Total$Admin2)  ]
    
    }
}



Short$ID<-NULL
Short$ID <-NA

for (i in 1:nrow(Short))
{
  if (Short$Admin2[i] %in% Total$Admin2 ){
    if
    (Short$Year[i] %in% Total$Year[which(Total$Admin2  == Short$Admin2[i])])
      
      Short$ID[i]<-Total$ID[which(Short$Year[i]==Total$Year & Short$Admin2[i]==Total$Admin2)  ]
    
  }
}


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


summary(Total)
# Tests, checks ok. Now I will test whether: long season + short season == ca season undefined (just for MT, obviously :-)

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# 1 AREA


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

Total$AreaLongDiff<-(Total$Area-Total$AreaLong)/Total$Area
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
# 1 MT

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
