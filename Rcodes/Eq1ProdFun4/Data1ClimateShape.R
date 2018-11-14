rm(list=ls())
WDuni<-c("/its/home/mn301/foodSystems/dataFS")  
WDhome<-c("/home/trennion/foodSystems/dataFS") # home. In comment cos cannot have errors if calling using function source

setwd(WDuni)
#  setwd(WDhome)   # home. In comment cos cannot have errors if calling using function source
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

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# now I will add the county which will be an averagefor Nandi. Probably this is best to do in this data format (climate10)
# for correlation tests, whether climate in Nandi South and Nandi North correlate see gettingData1




if(FALSE) { rbind( climate10[[1]], climate10[[1]][climate10[[1]]$ID1==50, ]  ) # just to put it in comment
climate10[[1]]$ADM2_NAME<-as.character(climate10[[1]]$ADM2_NAME)
  climate10[[1]][66,"ADM2_NAME"] <-"Nandi"   
  climate10[[1]]$ADM2_NAME<-as.factor(climate10[[1]]$ADM2_NAME)
  climate10[[1]][66,"ID1"] <-80          
  climate10[[1]][66,"ADM2_CODE"] <-NA   
  climate10[[1]][66,5:232] <-   apply( subset( climate10[[1]], ID1 %in% 50:51, select=5:232),2,mean)      

View(climate10[[1]])}
  

# now do this for all using lapply

test1<-climate10   # for later checking

climate10<-lapply(climate10, function(x) { rbind( x, x[x$ID1==50, ]  )
              x$ADM2_NAME<-as.character(x$ADM2_NAME)
              x[66,"ADM2_NAME"] <-"Nandi"   
              x$ADM2_NAME<-as.factor(x$ADM2_NAME)
              x[66,"ID1"] <-80          
              x[66,"ADM2_CODE"] <-NA   
              x[66,5:232] <-   apply( subset( x, ID1 %in% 50:51, select=5:232),2,mean) 
              return(x)}
)

#now the checking

sapply(climate10,dim)
mean(climate10[[5]][50:51,"X02.2015"])  # huuray, I may stop using 'which...names=='

mapply(function(x,y)  all.equal(x[1:65,],y, check.attributes=FALSE),          climate10,test1  )

# so far so good
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

  
  
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

# ending 31.7. 2018..Maybe do some checks for climate 13, then I can go into merging with Maize



mean(climate13[[3]][  climate13[[3]]$MonthYear %in% "X07.1999"  & climate13[[3]]$ID1 %in% 50:51 ,  5    ])
climate13[[3]][  climate13[[3]]$MonthYear %in% "X07.1999"  & climate13[[3]]$ID1 %in% 80 ,  5    ]



apply(as.matrix(1:8),1, function(i) { print( mean(climate13[[i]][  climate13[[i]]$MonthYear %in% "X07.1999"  & climate13[[i]]$ID1 %in% 50:51 ,  5    ]))
print(climate13[[i]][  climate13[[i]]$MonthYear %in% "X07.1999"  & climate13[[i]]$ID1 %in% 80 ,  5    ] )  })


#okkkkkkk some checking done. everything seems fine.

# to do:

  # Maize: take subset of years comfortable with the weather
  # add the weather to the dataset created in the point above

#... to be continued in another script...Data2mergeMaize
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

# save game

rm(list=setdiff(ls(), "climate13"))
#    save.image("~/foodSystems/dataFS/Main/climate13.RData")



