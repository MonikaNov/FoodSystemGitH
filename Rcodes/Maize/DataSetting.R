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

# two really small values which appear as zero: 1. Sorghum, Machakos, 1970 - Yield, 2. Maize, Migori, 1995, Yield. Nothing else:
# ? Crops$Yield[(Crops$Admin2== "Machakos" & Crops$Year==1970 & Crops$Crop=="Sorghum" )]<-0.0038
# ? Crops$Yield[(Crops$Admin2== "Migori" & Crops$Year==1995 & Crops$Crop=="Maize" )]<-0.0027

#--------------------------------------------------------------------------------------------------------------



summary(Crops)

table(Crops$SourceDoc, Crops$Season)
uniq<-unique(CrMaize[,c('Admin2','Year','Season')])

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

CrMaize<-subset(Crops, Crop=="Maize" & !Admin2=="")
nrow(CrMaize[duplicated(CrMaize[,c('Year','Admin2')]),])


# Each row is one of these 3 seasons: Short, Long, undefined. I will have to use just those with seasons undefined, as only few of them have long and short separately. 
        # those that do usually also have the undefined season (which probably is long and short season together)

# So now I create a set which only includes those with undefined season:

CrMaize2<-subset(Crops, Crop=="Maize" & !Admin2=="" & Season=='')

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

# Now i can test my first plm estimate

firstie<-plm(Yield~Area, CrMaize2, index= c("Admin2","Year"))
summary(firstie)

summary(CrMaize$MT[which(CrMaize$Season=="")])
summary(CrMaize$MT[which(CrMaize$Season=="Short Rains")])
summary(CrMaize$MT[which(CrMaize$Season=="Long Rains")])


# I need to create a unique identifier from year and season which I will use as a time index in panel
# Actually, I need to combine the seasons as for the most counties I only have one. or exclude them..
