rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

load("~/foodSystems/dataFS/Main/CrMaize8.RData")
library(plm)
CrMaize9<-pdata.frame(CrMaize8, index = c("ID", "Year"))

# Is there actually individual effect and time effect??

boxplot(Yield~ID,data=CrMaize8)  #yep, def there is individual
boxplot(Yield~Year,data=CrMaize8[as.numeric(CrMaize8$Year)>1998,])  
boxplot(Yield~Year,data=CrMaize8, drop=TRUE)  # maybe also effect of year??


purtest(CrMaize9$Yield)

library(tseries)
adf.test(CrMaize9$Yield, k=2) 
adf.test(CrMaize9$Yield[CrMaize9$ID==1], k=2) #good, seems stationary
adf.test(CrMaize9$MT, k=2)
adf.test(CrMaize9$Area, k=2)
adf.test(CrMaize9$PrecZones, k=2)
adf.test(CrMaize9$PrecZonVar, k=2)
adf.test(CrMaize9$TempZones, k=2)
adf.test(CrMaize9$TempZonVar, k=2)
adf.test(seq(1:100), k=2)  # just learning to use this :-)
