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
rm(climate11)
climate11<-lapply(climate10, function(x) melt(x, id.vars=c(1,2,3,4)))
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

climate13<-climate13[with(climate13, order(ID1,Year,Month)),]

climate13<-lapply(climate13,    function(x) climate13[with(climate13, order(ID1,Year,Month)),])

lapply(climate13,dim)
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

# 30.7.2018, END OF THE DAY:   OMG here I AM NOTICING THAT LIST 3 AND 4 HAVE SOME DIFFERENT COLUMNS FROM OTHERS. THEREFORE I HAD PROBLEMS WITH THIS ORDER!!!
# ACTUALLY, IT TURNS OUT THAT I DON'T HAVE THE COUNTY ID (ID1 IS SOMETHING DOFFERENT). SO I HAVE TO
# MATCH THE COUNTY ID EITHER USING PHASES (AS IN THE FILE GETTING DATA) OR MAYBE I CAN HAVE A LOOK AT MY NEW FILE FOR MAIZE.. MAYBE THIS WILL BE BETTER 

# so first to test if otherwise the same counties for the same variables:


sapply(climate13, names)

all.equal(!duplicated(climate13[[1]][c("ID1" ,"ADM2_CODE")]),!duplicated(climate13[[2]][c("ID1" ,"ADM2_CODE")]) )

lapply(climate13[c(2,5:8)],   function(x)  all.equal(climate13[[1]][c("ID1","ADM2_CODE","ADM2_NAME")],  x[c("ID1","ADM2_CODE","ADM2_NAME")]   ))



lapply(climate13[2:8], function(x) all.equal(   climate13[[1]][   !duplicated(climate13[[1]][c("ADM2_NAME" ,"ADM2_CODE")]),    c("ADM2_NAME" ,"ADM2_CODE")], 
                                                x[   !duplicated(x[c("ADM2_NAME" ,"ADM2_CODE")]),    c("ADM2_NAME" ,"ADM2_CODE")] 
)   ) 




lapply(climate13[3:4], function(x) all.equal(   climate13[[1]][   !duplicated(climate13[[1]][c("ID1" ,"ADM2_CODE")]),    c("ID1" ,"ADM2_CODE")], 
                                                x[   !duplicated(x[c("ADM2_NAME" ,"ADM2_CODE")]),    c("ADM2_NAME" ,"ADM2_CODE")] 
)   ) 


climate13[3:4]<-lapply(climate13[3:4], function(x) {names(x)[1]<-"ID1";x }  )

names(climate13[[3]])

lapply(climate13[3:4], function(x) all.equal(   climate13[[1]][   !duplicated(climate13[[1]][c("ID1" ,"ADM2_CODE")]),    c("ID1" ,"ADM2_CODE")], 
                                                x[   !duplicated(x[c("ID1" ,"ADM2_CODE")]),    c("ID1" ,"ADM2_CODE")] 
)   ) 


all.equal(  climate13[[3]]["ID1","ADM2_CODE"], climate13[[4]]["ID1","ADM2_CODE"] )


lapply(climate13[2:8], function(x)  all.equal(  climate13[[1]][!duplicated(climate13[[1]][c("ID1" ,"ADM2_CODE")]),    c("ID1" ,"ADM2_CODE")] ,
                                                x[!duplicated(x[c("ID1" ,"ADM2_CODE")]),    c("ID1" ,"ADM2_CODE")] 
))



!duplicated(climate13[[1]][c("ID1" ,"ADM2_CODE")])
  
unique(climate13[[1]][c("ID1" ,"ADM2_CODE","ADM2_NAME")])

all.equal(climate13[[1]][ !duplicated(climate13[[1]][c("ID1" ,"ADM2_CODE")]),   c("ID1" ,"ADM2_CODE")   ],  unique(climate13[[1]][c("ID1" ,"ADM2_CODE")   ]))

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# welp, I need to create all the lists same at this point. or earlier..
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

# ok, now I know how the climate13 dataset looks like. now I need create the county ID...

!duplicated(climate13[[1]][c("ID1" ,"ADM2_CODE")])

unique(climate13[[1]][c("ID1" ,"ADM2_CODE","ADM2_NAME")])

all.equal(climate13[[1]][ !duplicated(climate13[[1]][c("ID1" ,"ADM2_CODE")]),   c("ID1" ,"ADM2_CODE")   ],  unique(climate13[[1]][c("ID1" ,"ADM2_CODE")   ]))

# maybe the good strategy would be to learn my maize file and get the IDs from there


load("Main/CrMaize14.RData")

unique(climate13[[1]][c("ID1" ,"ADM2_CODE","ADM2_NAME")])

indexMaize14<-unique(CrMaize14[c("ID","county")])

length(intersect(CrMaize14$county,climate13[[1]]$ADM2_NAME ))  #good...

MatchingTest1<-merge(climate13[[1]],indexMaize14,by.x="ADM2_NAME",by.y = "county",all.x=TRUE)

# ordering and testing so that I can then check if merging ok

head(MatchingTest1)
    #checking
      rm(test2)
      test2<-climate13[[1]][order(climate13[[1]]["ID1"],climate13[[1]]["Year"],climate13[[1]]["Month"]),]
      all_equal(climate13[[1]],test2)

      all_equal(climate13[[1]],MatchingTest1[1:7])

      table(MatchingTest1$ID1,MatchingTest1$ID)
            # more checking
                test4<-MatchingTest1[is.na(MatchingTest1$ID)==FALSE,]
                table(test4$ID1,test4$ID)
                test4  [    which(   ! (test4$ID==test4$ID1)),]
                test4  [    which(    (test4$ID==test4$ID1)),]
          all.equal(test4, test4  [    which(    (test4$ID==test4$ID1)),]  )      
                
# more checking
  table(MatchingTest1$ID1[is.na(MatchingTest1$ID)==TRUE])
  table(droplevels(MatchingTest1$ADM2_NAME[is.na(MatchingTest1$ID)==TRUE]))

  
# and more checking
  
  unmatched<-MatchingTest1[is.na(MatchingTest1$ID)==TRUE,]
  
head(unmatched)

unique(unmatched[c("ADM2_NAME", "ID1")])
indexMaize14



intersect(unmatched$ID1, indexMaize14$ID)
setdiff(unmatched$ID1, indexMaize14$ID)
setdiff(indexMaize14$ID,MatchingTest1$ID1)

UniqueUnmatched<-unique(unmatched[c("ADM2_NAME", "ID1")])
UniqueUnmatched[UniqueUnmatched$ID1 %in% indexMaize14$ID,]  #  SO BASICALLY ID1 IN CLIMATE(14) IS CORRECT
# AN EXCEPTION IS NANDI AS IT HAS TWO ROWS IN CLIMATE AND JUST ONE (80) IN MAIZE. SO MAYBE AVERAGE??

# so why is Nandi county separately for North and South in the climate data?

# is there correlation in theather??
cor.test(climate13[[1]])


# to do:
  # make the columns same in all lists in climate 13
  # create average for Nandi South and North
  # Maize: take subset of years comfortable with the weather
  # add the weather to the dataset created in the point above

UniqueUnmatched[! UniqueUnmatched$ID1 %in% indexMaize14$ID,]
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo














climate15<-lapply(climate13,  function(x){x[order(x$ID1),]})

all.equal(climate13[[1]][2],climate13[[4]][1])

function(x)  { x[with(x, order("Year")),]
  return(x)}  )

climate13[[1]]<-climate13[[1]][with(climate13[[1]], order(ID1,Year,Month)),]
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#now match the data with Phase data
rm(climate4)
climate4<-merge(climate3[[1]],climate3[[2]])

for (i in 3:8) 
climate4<-merge(climate4,climate3[[i]])

climate4<-climate4[with(climate4, order(ID1,Year,Month)),]

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# checking

sapply(unique(climate4$ID1), function(x) summary(climate4$TmxZ[climate4$ID1==x]))

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# now i need to merge it with crops production data
Crops<-read.csv( "New31Jan2018/KE_Ag_Stats_2014.csv",header=TRUE, na.strings = c("NA","NC","NC.","-","#DIV/0!"),colClasses=c("factor","factor","factor","factor","factor","factor","factor","factor","numeric","numeric","numeric"))  #numeric,numeric,numeric))
CrMaize<-subset(Crops, Crop=="Maize" & !Admin2=="")
CrMaize2<-subset(Crops, Crop=="Maize" & !Admin2=="" & Season=='')
CrMaize4<-CrMaize2[as.numeric(as.character(CrMaize2$Year))>1998,]
CrMaize4$Admin0<-NULL
CrMaize4$Season<-NULL

rm(CropsCnties)
uniq<-!duplicated(climate4[c("ADM2_NAME" ,"ID1")])
climateID<-climate4[uniq,c(1,2)]
CropsCnties<-as.character(unique(CrMaize4$Admin2))


CropsCnties<-data.frame(name=CropsCnties,ID=rep(NA))
 
for (i in 1:nrow(CropsCnties))
  
  {if (as.character(CropsCnties$name[i]) %in% as.character(climateID$ADM2_NAME))
  
    {for (j in 1:nrow(climateID))
      
  { if (CropsCnties$name[i] == as.character(climateID$ADM2_NAME[j]))
    CropsCnties$ID[i]<-climateID$ID[j]
    }
}}

summary(CropsCnties$ID)

CropsCnties$ID[41]<-65
CropsCnties$ID[39]<-11
CropsCnties$ID[29]<-4
# CropsCnties$ID[30]<-4 Muranga duplicated for some years->>just keep one
CropsCnties$ID[5]<-41   
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# CropsCnties$name<-as.character(CropsCnties$name)
# CropsCnties<-rbind(CropsCnties,c("Nairobi",18))
# rm(list=setdiff(ls(),"CropsCnties"))
# save.image("Main/CropsCnties.RData")

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

#now put Ids back to counties>>merge

rm(CrMaize5)
CrMaize5<-merge(CropsCnties,CrMaize4,by.x="name",by.y="Admin2")
CrMaize5<-CrMaize5[is.na(CrMaize5$ID)==FALSE,]
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

# save game

rm(list=setdiff(ls(), "climate4"))
save.image("~/foodSystems/dataFS/Main/climate4.RData")

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


rm(list=setdiff(ls(), "CrMaize5"))
save.image("~/foodSystems/dataFS/Main/CrMaize5.RData")

