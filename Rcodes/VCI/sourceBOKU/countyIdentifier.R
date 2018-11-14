unique(PercZ1$ADM2_NAME)
unique(AllZones$COunty)


intersect(unique(PercZ$ADM2_NAME),names(AllZones))

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

sum(is.na(CoIdentifier$ID))
length(unique(CoIdentifier$ID))