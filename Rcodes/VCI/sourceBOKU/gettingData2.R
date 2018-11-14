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