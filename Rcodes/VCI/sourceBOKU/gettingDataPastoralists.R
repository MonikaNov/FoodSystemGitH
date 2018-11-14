rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

filenames <- list.files("VCI/Pastoralists", pattern="*.csv", full.names=TRUE)
Pastoralists <- lapply(filenames, read.csv)
names(Pastoralists) <- substr(filenames, 14, nchar(filenames)-4)


# adding a column with a county name of county
Pastoralists<-lapply(Pastoralists,  function(x) cbind(x,NA))

Pastoralists <- lapply(seq(Pastoralists), function(i) {
  names(Pastoralists[[i]])[8] <- 'County'
  Pastoralists[[i]]})

names(Pastoralists) <- substr(filenames, 18, nchar(filenames)-4)

Pastoralists <- lapply(seq(Pastoralists), function(i) {
  Pastoralists[[i]][8] <- names(Pastoralists)[i]
  Pastoralists[[i]]})

names(Pastoralists) <- substr(filenames, 18, nchar(filenames)-4)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# now add county id using county identifier
# first obtain County identifier
PercZ<-read.csv( "CountyClimateM/pre_zscore_kenya_adm2_1999-2015.10thperc.csv",header=TRUE)

CoIdentifier<-as.data.frame(names(Pastoralists))

CoIdentifier<-cbind(CoIdentifier,NA)
names(CoIdentifier)<-c("Name","ID")

for (i in seq(nrow(CoIdentifier)))
{
  if (as.character(CoIdentifier$Name[i]) %in% PercZ$ADM2_NAME )
  {CoIdentifier$ID[i]<-PercZ$ID1[which(PercZ$ADM2_NAME==as.character(CoIdentifier$Name[i]) ) ]  }
  
}

CoIdentifier$ID[16]<-11
CoIdentifier$ID[17]<-12
CoIdentifier$ID[20]<-49



CoIdentifier


sum(is.na(CoIdentifier$ID))
length(unique(CoIdentifier$ID))


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
all.equal( names(Pastoralists), as.character(CoIdentifier$Name))

CountyID<-rep(NA,195 )

  
Pastoralists<-lapply(Pastoralists,  function(x) cbind(x,CountyID))



Pastoralists <- lapply(seq(Pastoralists), function(i) {
  Pastoralists[[i]][9] <- CoIdentifier$ID[i]
  Pastoralists[[i]]})



# now create a data frame 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
rm(PastoralistsDF)
PastoralistsDF<-do.call("rbind", Pastoralists)

summary(PastoralistsDF)
head(PastoralistsDF)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# now do the shape that pedram wants

VCImean<-PastoralistsDF[,c(9,8,1,2,7)]

head(VCImean)
names(VCImean)[5]<-'value'

VCImean <- cast(VCImean, County~Year+Month)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
write.csv(PastoralistsDF, file = "VCI/VCIPastoralists.csv")

write.csv(VCImean, file = "VCI/VCImeanPast.csv")

rm(list=setdiff(ls(), c('PastoralistsDF','Pastoralists')))
save.image("~/foodSystems/dataFS/Main/VCIpast.RData")
