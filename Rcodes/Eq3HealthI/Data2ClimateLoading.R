rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)

load("Main/Phase20.RData")
load("Main/climate13.RData")
library(dplyr)
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

small<-lapply(climate13,function(y) select(y,c("CountyID"="ID1",5,"Month","Year")))

                
reMerge<-function(x) {  if (x==1)    return(  merge(small[[1]],Phase20)  )
      else return( merge(     small[[x]], reMerge(x-1 )))}
Phase21<-reMerge(8)                 

#ordering rows and columns and testing
Phase22<-Phase21[with(Phase21, order(CountyID,Year,Month)),]
test1<-Phase22
Phase22<-Phase21[with(Phase21, order(CountyID,Year,Month)),c(1,12,3,2,13,11,9,10,8,7,5,6,4,14:20)]
all_equal(test1, Phase22,ingnore_col_order=TRUE, ignore_row_order=TRUE)
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# save game
rm(list=setdiff(ls(),"Phase22"))
save.image("Main/Phase22.RData")