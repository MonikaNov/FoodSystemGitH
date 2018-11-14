rm(list=ls())

WDuni<-c("/home/m/mn/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
rm(EWreports)
EWreports<-read.csv( "NDMAcountyBulletins/my/All12.csv",header=TRUE)
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

plot(KituiEWr$PercZ[9:31], ylim=c(-3,3),type='l', xaxt = "n", xlab='Time')
axis(1, at=1:23, labels=KituiEWr$date[9:31])
lines(KituiEWr$Phase[9:31], type='h')


barplot(KituiEWr$Phase[9:31], ylim=c(-3,3))
lines(KituiEWr$PercZ[9:31], type='p',xlim=c(9,31))



df.bar <- barplot(KituiEWr$Phase[9:31], ylim=c(-2,2), xaxt = "n", xlab='Time')
axis(1, at=df.bar, labels=KituiEWr$date[9:31])
lines(x = df.bar, y = KituiEWr$PercZ[9:31])
points(x = df.bar, y = KituiEWr$PercZ[9:31])

# now replace NA with zero for a nicer plot of all
KituiEWr$Phase0<-KituiEWr$Phase
KituiEWr$Phase0[ is.na(KituiEWr$Phase0) ] <- 0 

par(bg=NA)

par(mar=c(5.1,4.1,4.1,2.1))
plot(KituiEWr$Phase0[9:31], ylim=c(-2,2), type="S",xaxt = "n",yaxt = "n", bty="n", xlab='',
     ylab="                          Phase: 0 = NA, 1 = Normal,  2 = Alert")
axis(1, at=c(1,6,12,18,23), labels=c("Feb 2014","July 2014","Jan 2015","July 2015","Dec 2015"),lwd=3,lwd.ticks=0) 
axis(2, at=c(-2,-1,0,1,2), labels=c(-2,-1,0,1,2),lwd=3,lwd.ticks=0) 

lines(KituiEWr$PercZ[9:31],col=4)

legend(x=6,y= -0.5,legend=('Precipitation z-score'), col=4, text.col = 4,lty=1,pch=NA,bty='n',
       border = NULL)


###################################################################################################################################################################################################################################################################

#alternative>>inverted scale of IPC phase

par(mar=c(5.1,4.1,4.1,2.1))
plot(2-KituiEWr$Phase0[9:31], ylim=c(-2,2), type="S",xaxt = "n",yaxt = "n", bty="n", xlab='',
     ylab="                          Phase: 0 = Alert, 1 = Normal,  2 = NA")
axis(1, at=c(1,6,12,18,23), labels=c("Feb 2014","July 2014","Jan 2015","July 2015","Dec 2015"),lwd=3,lwd.ticks=0) 
axis(2, at=c(-2,-1,0,1,2), labels=c(-2,-1,0,1,2),lwd=3,lwd.ticks=0) 

lines(KituiEWr$PercZ[9:31],col=4)

legend(x=6,y=2,legend=('Precipitation z-score'), col=4, text.col = 4,lty=1,pch=NA,bty='n',
       border = NULL)











###################################################################################################################################################################################################################################################################
###################################################################################################################################################################################################################################################################

labelYr<-c("2013","2014","2015","2016","2017","")

plot(KituiEWr$Phase0,type="S",yaxt = "n", xaxt = "n", xlab='Time',ylab='Phase',bty="n")
axis(1,lwd=3,lwd.ticks=0,at=c(1,13,25,37,49,60), labels=labelYr)
axis(2, at=c(0,1,2,3),lwd=3,lwd.ticks=0, labels=c('','Normal','Alert','Alarm'))
###################################################################################################################################################################################################################################################################
'',
#alternative>>inverted scale of IPC phase
labelYr<-c("2013","2014","2015","2016","2017","")

plot(4-KituiEWr$Phase0,type="S",yaxt = "n", xaxt = "n", xlab='Time',ylab='Phase',bty="n")
axis(1,lwd=3,lwd.ticks=0,at=c(1,13,25,37,49,60), labels=labelYr)
axis(2, at=c(1,2,3,4),lwd=3,lwd.ticks=0, labels=c('Alarm','Alert','Normal',''))