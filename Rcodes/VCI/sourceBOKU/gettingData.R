rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

filenames <- list.files("VCI/AllZones", pattern="*.csv", full.names=TRUE)
AllZones <- lapply(filenames, read.csv)
names(AllZones) <- substr(filenames, 14, nchar(filenames)-4)


# adding a column with a county name of county
AllZones<-lapply(AllZones,  function(x) cbind(x,NA))

AllZones <- lapply(seq(AllZones), function(i) {
  names(AllZones[[i]])[8] <- 'County'
  AllZones[[i]]})

names(AllZones) <- substr(filenames, 14, nchar(filenames)-4)

AllZones <- lapply(seq(AllZones), function(i) {
  AllZones[[i]][8] <- names(AllZones)[i]
  AllZones[[i]]})

names(AllZones) <- substr(filenames, 14, nchar(filenames)-4)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# now add county id using county identifier
# first obtain County identifier
PercZ<-read.csv( "CountyClimateM/pre_zscore_kenya_adm2_1999-2015.10thperc.csv",header=TRUE)

CoIdentifier<-as.data.frame(names(AllZones))

CoIdentifier<-cbind(CoIdentifier,NA)
names(CoIdentifier)<-c("Name","ID")

for (i in seq(nrow(CoIdentifier)))
{
  if (as.character(CoIdentifier$Name[i]) %in% PercZ$ADM2_NAME )
  {CoIdentifier$ID[i]<-PercZ$ID1[which(PercZ$ADM2_NAME==as.character(CoIdentifier$Name[i]) ) ]  }
  
}


CoIdentifier$ID[47]<-49
CoIdentifier$ID[44]<-48
CoIdentifier$ID[42]<-46
CoIdentifier$ID[41]<-65
CoIdentifier$ID[40]<-12
CoIdentifier$ID[39]<-11
CoIdentifier$ID[32]<-80  # in the climate data is north and south separately
CoIdentifier$ID[8]<-24 
CoIdentifier$ID[5]<-41 # I matched Elgeyo-Marakwet to Marakwer in the climate data. Probably the same one.

CoIdentifier


sum(is.na(CoIdentifier$ID))
length(unique(CoIdentifier$ID))


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
all.equal( names(AllZones), as.character(CoIdentifier$Name))

CountyID<-rep(NA,195 )

  
AllZones<-lapply(AllZones,  function(x) cbind(x,CountyID))



AllZones <- lapply(seq(AllZones), function(i) {
  AllZones[[i]][9] <- CoIdentifier$ID[i]
  AllZones[[i]]})



# now create a data frame 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
rm(AllZonesDF)
AllZonesDF<-do.call("rbind", AllZones)

summary(AllZonesDF)
head(AllZonesDF)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# now do the shape that pedram wants

VCImean<-AllZonesDF[,c(9,8,1,2,7)]

head(VCImean)
names(VCImean)[5]<-'value'

VCImean <- cast(VCImean, County~Year+Month)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
names(AllZones) <- substr(filenames, 14, nchar(filenames)-4)

write.csv(AllZonesDF, file = "VCI/VCIallzones.csv")

write.csv(VCImean, file = "VCI/VCImean.csv")

rm(list=setdiff(ls(), c('AllZonesDF','AllZones')))
save.image("~/foodSystems/dataFS/Main/VCI.RData")
