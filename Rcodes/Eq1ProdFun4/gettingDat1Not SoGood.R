rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(reshape)
library(dplyr)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Now the Z-scores
PreMean<-read.csv( "climateNew/climate_data/chirps_kenya_adm2_1999-2017.mean.csv",header=TRUE)
PreMed<-read.csv( "climateNew/climate_data/chirps_kenya_adm2_1999-2017.median.csv",header=TRUE)
PreMeanZ<-read.csv("climateNew/climate_data/chirps_zsc_kenya_adm2_1999-2017.mean.csv",header=TRUE)
PreMedZ<-read.csv( "climateNew/climate_data/chirps_zsc_kenya_adm2_1999-2017.median.csv",header=TRUE)
TemMean<-read.csv( "climateNew/climate_data/ghcn_kenya_adm2_1999-2017.mean.csv",header=TRUE)
TemMed<-read.csv( "climateNew/climate_data/ghcn_kenya_adm2_1999-2017.median.csv",header=TRUE)
TemMeanZ<-read.csv("climateNew/climate_data/ghcn_zsc_kenya_adm2_1999-2017.mean.csv",header=TRUE)
TemMedZ<-read.csv( "climateNew/climate_data/ghcn_zsc_kenya_adm2_1999-2017.median.csv",header=TRUE)


climate10<-list(PreMean,PreMed,PreMeanZ,PreMedZ, TemMean, TemMed, TemMeanZ, TemMedZ)
lapply(climate10,names)  # original files of PreMeanZ  and PreMedZ - have a bit different format - county names missing. need to fix this first
lapply(climate10,dim)

all.equal(PreMean[c(3,4)],PreMeanZ[c(2,3)] )

# first I have to verify if I can just match the county names based on id and code

names(PreMeanZ)[2]<-"ID1"
names(PreMedZ)[2]<-"ID1"

all.equal(PreMean[c(3,4)],PreMeanZ[c(2,3)] )

climate10<-list(PreMean,PreMed,PreMeanZ,PreMedZ, TemMean, TemMed, TemMeanZ, TemMedZ)


all.equal(unique(climate10[[1]][1:4]),   unique(climate10[[2]][1:4]  ))
all.equal(unique(climate10[[1]][3:4]),   unique(climate10[[2]][3:4]  ))

lapply(climate10[c(2,5:8)],  function(x) all.equal(  unique(climate10[[1]][1:4]),unique(x)[1:4]  )  )

all.equal(PreMean[c(3,4)],PreMeanZ[c(2,3)] )


lapply(climate10[c(2:8)],  function(x) all.equal(  unique(climate10[[1]][c("ID1","ADM2_CODE")]),unique(x)[c("ID1","ADM2_CODE")]  )  )
lapply(climate10[c(2:8)],  function(x) all.equal(  unique(climate10[[1]][c("X","ID1","ADM2_CODE")]),unique(x)[c("X","ID1","ADM2_CODE")]  )  )


lapply(climate10[c(2:8)],  function(x) all.equal(  unique(climate10[[1]][c("ID1","ADM2_CODE")]),unique(x)[c("ID1","ADM2_CODE")]  )  )
lapply(climate10[c(2:8)],  function(x) all.equal(  climate10[[1]][c("X","ID1","ADM2_CODE")],x[c("X","ID1","ADM2_CODE")]  )) 

# ok, seems fine

climate10[[3]]$ADM2_NAME<-climate10[[1]]$ADM2_NAME
climate10[[4]]$ADM2_NAME<-climate10[[5]]$ADM2_NAME

climate10[[3]]<-climate10[[3]][c(1,232,2:231)]
climate10[[4]]<-climate10[[4]][c(1,232,2:231)]

# chack in:
all.equal(names( climate10[[3]]),names(climate10[[1]]))
lapply(climate10[2:8], function(x) all.equal(  names(climate10[[1]]), names(x))   )

lapply(climate10[2:8], function(x) all.equal(  climate10[[1]][1:4], x[1:4])   )

head(climate10[[3]][1:5])
head(climate10[[1]][1:5])
# so far so good



rbind( climate10[[1]], climate10[[1]][climate10[[1]]$ID1==50, ]  )
climate10[[1]]["ADM2_NAME"]<-as.character(climate10[[1]]["ADM2_NAME"])
  climate10[[1]][66,"ADM2_NAME"] <-"Nandi"    
  climate10[[1]][66,"ID1"] <-80          
  climate10[[1]][66,"ADM2_CODE"] <-NA   
  climate10[[1]][66,5:232] <-   apply( subset( climate10[[1]], ID1 %in% 50:51, select=5:232),2,mean)      

  
  
  
  
  subset( climate10[[1]], ID1 %in% 50:51, select=20:30)
  
        apply( subset( climate10[[1]], ID1 %in% 50:51, select=5:232),2,mean)
  
  
rm(climate11)
climate11<-lapply(climate10, function(x) melt(x, id.vars=c(1,2,3,4)))
lapply(climate11,head)

climate12<-lapply(climate11, function(x) (x[-1]))


rm(climate13)
climate13<-lapply(climate12, function(x) {names(x)[4]<-'MonthYear'
                 x$Month<-as.numeric(substr(x$MonthYear,2,3))
                   x$Year<-as.numeric(substr(x$MonthYear,5,8))
                  return(x)  }
               )


myNames<-c("PreMean","PreMed","PreMeanZ","PreMedZ","TemMean","TemMed","TemMeanZ","TemMedZ")


climate13<-mapply(   function(x,y) {names(climate13[[x]])[5]<-y
                                        return(climate13[[x]])}  ,1:8,myNames,SIMPLIFY =FALSE   )

# so now sort everything and also test as I go..
test1<-climate13

climate13<-lapply(climate13,    function(x) x[order(x["ID1"],x["Year"],x["Month"]),])
lapply(climate13,head)

mapply( function(x,y) all_equal(x,y), test1,climate13)
head(climate13[[3]])
head(test1[[3]])

rm(test1)



#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

# to do:

  # create average for Nandi South and North
  # Maize: take subset of years comfortable with the weather
  # add the weather to the dataset created in the point above


#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# so now average for nandi south and north. Otherwise I should be able to match with CrMaize though ID1...
# is there correlation in theather??
unique(climate13[[1]]$ADM2_NAME[climate13[[1]]$ID1 %in% c(50:51)])
# So I am after ID 50 and ID 51

cor.test(climate13[[1]][climate13[[1]]["ID1"]==50,  5],climate13[[1]][climate13[[1]]["ID1"]==51,  5] )

mapply(function(x,y) cor.test(x[x["ID1"]==50,5],y[y["ID1"]==51,5]),  climate13,climate13   )
mapply(function(x,y) cor.test(x[x["ID1"]==50,5],y[y["ID1"]==14,5]),  climate13,climate13   )


lapply(1:8, function(x)  cor.test(climate13[[x]][climate13[[x]]["ID1"]==50,  5],climate13[[x]][climate13[[x]]["ID1"]==51,  5] ))
sapply(1:8, function(x)  cor.test(climate13[[x]][climate13[[x]]["ID1"]==50,  5],climate13[[x]][climate13[[x]]["ID1"]==51,  5] )$estimate  )
sapply(1:8, function(x)  cor.test(climate13[[x]][climate13[[x]]["ID1"]==50,  5],climate13[[x]][climate13[[x]]["ID1"]==51,  5] )$p.val  )

# to compare if for the 2 nandi counties the correlation is much higher than for other combinations of counties

sapply(1:8, function(x)  cor.test(climate13[[x]][climate13[[x]]["ID1"]==21,  5],climate13[[x]][climate13[[x]]["ID1"]==20,  5] )$estimate  )
sapply(1:8, function(x)  cor.test(climate13[[x]][climate13[[x]]["ID1"]==21,  5],climate13[[x]][climate13[[x]]["ID1"]==20,  5] )$p.val  )

# yep. the correlation seems good. now proceed to the averages
# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo














climate15<-lapply(climate13,  function(x){x[order(x$ID1),]})

all.equal(climate13[[1]][2],climate13[[4]][1])

function(x)  { x[with(x, order("Year")),]
  return(x)}  )

climate13[[1]]<-climate13[[1]][with(climate13[[1]], order(ID1,Year,Month)),]

