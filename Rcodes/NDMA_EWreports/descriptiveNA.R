rm(list=ls())

WDuni<-c("/home/m/mn/mn301/foodSystems/dataFS") # uni
WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
rm(EWreports)
EWreports<-read.csv( "NDMA_county_bulletins/my/All12.csv",header=TRUE)
KituiEWr<-EWreports[which(EWreports$County=='Kitui'),]
KituiEWr$Phase<-NA
KituiEWr$Phase[which(KituiEWr$Phase_county=='Normal')]<-1
KituiEWr$Phase[which(KituiEWr$Phase_county=='Alert')]<-2
KituiEWr$Phase[which(KituiEWr$Phase_county=='Alarm')]<-3
table(KituiEWr$Phase_county,KituiEWr$Phase)
summary(KituiEWr$Phase)
lines(KituiEWr$Phase)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# now add climate data

PercZ<-read.csv( "CountyClimateM/pre_zscore_kenya_adm2_1999-2015.10thperc.csv",header=TRUE)

PercZ1<-melt(PercZ, id.vars=c(1,2,3,4))
names(PercZ1)[5]<-'MonthYear'
PercZ1$Month<-substr(PercZ1$MonthYear,2,3)
PercZ1$Year<-substr(PercZ1$MonthYear,5,8)


KituiZ<-PercZ1[which(PercZ1$ADM2_NAME=='Kitui' & as.numeric(PercZ1$Year)>2012 ),]

#....................................................................................................................................................................................................................................................................................................
# now match pecipitation to the EW phase of NDMA

KituiEWr$PercZ<-NA
KituiEWr$date<-NA

for (i in 1: nrow(KituiEWr[which(as.numeric(KituiEWr$Year)<2016),]))
  
{KituiEWr$PercZ[i]<-KituiZ$value[which(as.numeric(KituiZ$Year)==KituiEWr$Year[i] & as.numeric(KituiZ$Month)==KituiEWr$Month[i])]
KituiEWr$date[i]<-as.character(KituiZ$MonthYear[which(as.numeric(KituiZ$Year)==KituiEWr$Year[i] & as.numeric(KituiZ$Month)==KituiEWr$Month[i])])
}

KituiEWr$date<-substr(KituiEWr$date,2,8)
#....................................................................................................................................................................................................................................................................................................
# now plots, graphs



par(bg=NA)

par(mar=c(5.1,4.1,4.1,2.1))
plot(KituiEWr$Phase[9:31], ylim=c(-2,2), type="S",xaxt = "n",yaxt = "n", bty="n", xlab='',
     ylab="                                                   Phase:  1 = Normal,  2 = Alert")

 # lines(KituiEWr$Phase[9:31], ylim=c(-2,2), type="p") #with or without points

axis(1, at=c(1,6,12,18,23), labels=c("Feb 2014","July 2014","Jan 2015","July 2015","Dec 2015"),lwd=3,lwd.ticks=3) 
axis(2, at=c(-2,-1,0,1,2), labels=c(-2,-1,0,1,2),lwd=3,lwd.ticks=0) 

lines(KituiEWr$PercZ[9:31],col=4)   #with or without points

legend(x=6,y= -0.5,legend=('Precipitation z-score'), col=4, text.col = 4,lty=1,pch=NA,bty='n',
       border = NULL)


###################################################################################################################################################################################################################################################################
###################################################################################################################################################################################################################################################################
###################################################################################################################################################################################################################################################################

labelYr<-c("2013","2014","2015","2016","2017","")

plot(KituiEWr$Phase,type="S",yaxt = "n", xaxt = "n", xlab='Time',ylab='Phase',bty="n")
#lines(KituiEWr$Phase,type="p")  #with or without points
axis(1,lwd=3,lwd.ticks=3,at=c(1,13,25,37,49,60), labels=labelYr)
axis(2, at=c(0,1,2,3),lwd=3,lwd.ticks=0, labels=c('','Normal','Alert','Alarm'))
