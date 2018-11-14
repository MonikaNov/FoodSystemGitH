rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)

load("Main/Phase22.RData")

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
head(Phase22)
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#precipitation

boxplot(PreMean~Month, data = Phase22,main=" PreMean",
        xlab="Months")

boxplot(PreMeanZ~Month, data = Phase22,main=" PreMean z score",
        xlab="Months")

boxplot(PreMed~Month, data = Phase22,main=" PreMed",
        xlab="Months")

boxplot(PreMedZ~Month, data = Phase22,main=" PreMed z score",
        xlab="Months")

#------------------------------------------------------------------------------
# temperature
boxplot(TemMean~Month, data = Phase22,main=" TemMean",
        xlab="Months")

boxplot(TemMeanZ~Month, data = Phase22,main=" TemMean z score",
        xlab="Months")

boxplot(TemMed~Month, data = Phase22,main=" TemMed",
        xlab="Months")

boxplot(TemMedZ~Month, data = Phase22,main=" TemMed z score",
        xlab="Months")

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#   mow YEAR:
#   precipitation

boxplot(PreMean~Year, data = Phase22,main=" PreMean",
        xlab="Years")

boxplot(PreMeanZ~Year, data = Phase22,main=" PreMean z score",
        xlab="Years")

boxplot(PreMed~Year, data = Phase22,main=" PreMed",
        xlab="Years")

boxplot(PreMedZ~Year, data = Phase22,main=" PreMed z score",
        xlab="Years")

#------------------------------------------------------------------------------
# temperature
boxplot(TemMean~Year, data = Phase22,main=" TemMean",
        xlab="Years")

boxplot(TemMeanZ~Year, data = Phase22,main=" TemMean z score",
        xlab="Years")

boxplot(TemMed~Year, data = Phase22,main=" TemMed",
        xlab="Years")

boxplot(TemMedZ~Year, data = Phase22,main=" TemMed z score",
        xlab="Years")


#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#   mow COUNTY:
#   precipitation

boxplot(PreMean~CountyID, data = Phase22,main=" PreMean",
        xlab="CountyIDs")

boxplot(PreMeanZ~CountyID, data = Phase22,main=" PreMean z score",
        xlab="CountyIDs")

boxplot(PreMed~CountyID, data = Phase22,main=" PreMed",
        xlab="CountyIDs")

boxplot(PreMedZ~CountyID, data = Phase22,main=" PreMed z score",
        xlab="CountyIDs")

#------------------------------------------------------------------------------
# temperature
boxplot(TemMean~CountyID, data = Phase22,main=" TemMean",
        xlab="CountyIDs")

boxplot(TemMeanZ~CountyID, data = Phase22,main=" TemMean z score",
        xlab="CountyIDs")

boxplot(TemMed~CountyID, data = Phase22,main=" TemMed",
        xlab="CountyIDs")

boxplot(TemMedZ~CountyID, data = Phase22,main=" TemMed z score",
        xlab="CountyIDs")


#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#   mow YEAR:
#   precipitation

boxplot(PreMean~Year, data = Phase22,main=" PreMean",
        xlab="Years")

boxplot(PreMeanZ~Year, data = Phase22,main=" PreMean z score",
        xlab="Years")

boxplot(PreMed~Year, data = Phase22,main=" PreMed",
        xlab="Years")

boxplot(PreMedZ~Year, data = Phase22,main=" PreMed z score",
        xlab="Years")

#------------------------------------------------------------------------------
# temperature
boxplot(TemMean~Year, data = Phase22,main=" TemMean",
        xlab="Years")

boxplot(TemMeanZ~Year, data = Phase22,main=" TemMean z score",
        xlab="Years")

boxplot(TemMed~Year, data = Phase22,main=" TemMed",
        xlab="Years")

boxplot(TemMedZ~Year, data = Phase22,main=" TemMed z score",
        xlab="Years")


#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#   mow NDMA phase:
#   precipitation

boxplot(PreMean~PhaseNum, data = Phase22,main=" PreMean",
        xlab="PhaseNum")

boxplot(PreMeanZ~PhaseNum, data = Phase22,main=" PreMean z score",
        xlab="PhaseNum")

boxplot(PreMed~PhaseNum, data = Phase22,main=" PreMed",
        xlab="PhaseNum")

boxplot(PreMedZ~PhaseNum, data = Phase22,main=" PreMed z score",
        xlab="PhaseNum")

#------------------------------------------------------------------------------
# temperature
boxplot(TemMean~PhaseNum, data = Phase22,main=" TemMean",
        xlab="PhaseNum")

boxplot(TemMeanZ~PhaseNum, data = Phase22,main=" TemMean z score",
        xlab="PhaseNum")

boxplot(TemMed~PhaseNum, data = Phase22,main=" TemMed",
        xlab="PhaseNum")

boxplot(TemMedZ~PhaseNum, data = Phase22,main=" TemMed z score",
        xlab="PhaseNum")