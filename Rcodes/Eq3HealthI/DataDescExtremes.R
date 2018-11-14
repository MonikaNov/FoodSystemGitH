rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)
library(plm)
library(lme4)

load("Main/Phase23.RData")

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  'Based on the plots of MUAC, there is an outlier with MUAC about 60. WHich one is it?'

Phase23[Phase23$MUACn>50 & !(is.na(Phase23$MUACn)) ,]
Phase23[Phase23$County=="Kilifi" & Phase23$Year==2017 & !(is.na(Phase23$MUACn)) ,]
Phase23[Phase23$County=="Kilifi" & !(is.na(Phase23$MUACn)) ,]

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 'Investigate counties which are in the Normal phase and MUAC > 10. What was the value of VCI ?'

Phase23[Phase23$MUACn>10 & Phase23$PhaseNum==1 & !(is.na(Phase23$MUACn)) ,]
table(droplevels(Phase23$County[Phase23$MUACn>10 & Phase23$PhaseNum==1 & !(is.na(Phase23$MUACn))]))
table((Phase23$Year[Phase23$MUACn>10 & Phase23$PhaseNum==1 & !(is.na(Phase23$MUACn))]))
table((Phase23$Month[Phase23$MUACn>10 & Phase23$PhaseNum==1 & !(is.na(Phase23$MUACn))]))


#------------------- the nicest: 

CountyYear<-table(droplevels(Phase23$County[Phase23$MUACn>10 & Phase23$PhaseNum==1 & !(is.na(Phase23$MUACn))]),
      Phase23$Year[Phase23$MUACn>10 & Phase23$PhaseNum==1 & !(is.na(Phase23$MUACn))]   )

CountyMonth<-table(droplevels(Phase23$County[Phase23$MUACn>10 & Phase23$PhaseNum==1 & !(is.na(Phase23$MUACn))]),
       (Phase23$Month[Phase23$MUACn>10 & Phase23$PhaseNum==1 & !(is.na(Phase23$MUACn))]   ))

Month<-table((Phase23$Month[Phase23$MUACn>10 & Phase23$PhaseNum==1 & !(is.na(Phase23$MUACn))]))

write.csv(CountyYear,"/its/home/mn301/foodSystems/Rcodes/Eq3HealthI/Tables/CountyYear.csv")
write.csv(CountyMonth,"/its/home/mn301/foodSystems/Rcodes/Eq3HealthI/Tables/CountyMonth.csv")
write.csv(Month,"/its/home/mn301/foodSystems/Rcodes/Eq3HealthI/Tables/Month.csv")
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 'Check where CSI>30'

Phase23[Phase23$CSIn>30 & !(is.na(Phase23$CSIn)) ,]
Phase23[Phase23$CSIn>25 & !(is.na(Phase23$CSIn)) ,]
Phase23[Phase23$County=="Turkana" & Phase23$Year==2016 & !(is.na(Phase23$MUACn)) ,]

Phase23[Phase23$County=="Turkana"  & !(is.na(Phase23$MUACn)) ,]