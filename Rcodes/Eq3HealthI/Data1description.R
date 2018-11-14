rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

rm(Phase04)
Phase04<-read.csv( "NDMA_county_bulletins/my/All04.csv",header=TRUE)

Phase04$PhaseNum<-NA
Phase04$PhaseNum[which(Phase04$Phase=='Normal')]<-1
Phase04$PhaseNum[which(Phase04$Phase=='Alert')]<-2
Phase04$PhaseNum[which(Phase04$Phase=='Alarm')]<-3
Phase04$PhaseNum[which(Phase04$Phase=='Alarm/Alert')]<-2.5
Phase04$PhaseNum[which(Phase04$Phase=='Alert/Normal')]<-1.5
Phase04$PhaseNum[which(Phase04$Phase=='noreport')]<-NA
Phase04$PhaseNum[which(Phase04$Phase=='Recovery')]<-5

Phase04$PhaseOrd<-NA
Phase04$PhaseOrd[which(Phase04$PhaseNum==1)]<-1
Phase04$PhaseOrd[which(Phase04$PhaseNum==2)]<-2
Phase04$PhaseOrd[which(Phase04$PhaseNum==3)]<-3
Phase04$PhaseOrd[which(Phase04$PhaseNum==2.5)]<-NA
Phase04$PhaseOrd[which(Phase04$PhaseNum==1.5)]<-NA
Phase04$PhaseOrd[which(Phase04$PhaseNum=='noreport')]<-NA
Phase04$PhaseOrd[which(Phase04$Phase=='Recovery')]<-NA

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Phase20<-Phase04
Phase20$MUACn<-as.numeric(as.character(Phase04$MUAC)); Phase20$MUACn[Phase04$MUAC=='noreport']<-NA
Phase20$CSIn<-as.numeric(as.character(Phase04$CSI)); Phase20$CSIn[Phase04$CSI=='noreport']<-NA

summary(Phase04)
summary(Phase20)
all.equal(Phase04,Phase20[1:10])  # goot
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# correlation MUAC and CSI
par(mar=c(4.5,4.5,2.5,1))
plot(Phase20$CSIn,Phase20$MUACn,xlab = "CSI (Coping Strategies Index)",
     ylab="Percentage of children under five with MUAC  < 135mm",main="Correlation CSI and MUAC")
plot(Phase20$MUACn,Phase20$CSIn)
cor.test(Phase20$MUACn,Phase20$CSIn)
plot(lm(MUACn~CSIn, data=Phase20))


#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

# Is there a significant difference in MUAC for counties in Alert and those in Alarm?  (question arised at the meeting 4.9.2018)

t.test(Phase20$MUACn[Phase20$PhaseNum==2],Phase20$MUACn[Phase20$PhaseNum==3])
t.test(Phase20$CSIn[Phase20$PhaseNum==2],Phase20$MUACn[Phase20$CSIn==3])
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo


hist(Phase20$MUACn) ; summary(Phase20$MUACn)
hist(Phase20$MUACn,60,main="Histogram: MUAC (Mid-Upper Arm Circumference)",
     xlab=" Percentage of children under five with the Mid-Upper Arm Circumference  < 135mm")
#---------------------
par(mar=c(2.5,4,2.5,1))
hist(Phase20$CSIn)
hist(Phase20$CSIn,60)
hist(Phase20$CSIn,40,     main="Histogram: CSI  (Coping Strategies Index)", xlab=" ")

hist(subset(Phase20,CSIn<1,select=CSIn )[,1],40)
hist(subset(Phase20,CSIn>1,select=CSIn )[,1],40,
     main="Histogram: CSI (Coping Strategy Index)", xlab=" ")

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# patterns over months, years?
#         b    l   t   r
# par(mar=c(5.1,4.1,4.1,2.1))

summary(Phase20$MUACn)
boxplot(MUACn~Month, data = Phase20,main=" MUAC",
        xlab="Months")
mtext("Percentage of children under five with the Mid-Upper Arm Circumference  < 135mm",cex=0.9)

summary(Phase20$CSIn)

par(mar=c(4.5,3,2.5,1))
boxplot(CSIn~Month, data = Phase20,main="CSI (Coping Strategies Index)", xlab="Months")



summary(Phase20$MUACn)
boxplot(MUACn~Year, data = Phase20,main=" MUAC",
        xlab="Year")
mtext("Percentage of children under five with the Mid-Upper Arm Circumference  < 135mm",cex=0.9)


summary(Phase20$CSIn)
boxplot(CSIn~Year, data = Phase20,main="CSI  (Coping Strategies Index)", xlab="Year") # a bit strange - so much bigger for 2017?
aggregate(CSIn~Year, data=Phase20,mean)
aggregate(CSIn~Year, data=Phase20,median)
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# patterns over counties

summary(Phase20$MUACn); 
boxplot(MUACn~CountyID, data = Phase20,main=" MUAC",xlab="County ID")
mtext("Percentage of children under five with the Mid-Upper Arm Circumference  < 135mm",cex=0.9)

par(mfrow=c(1,1))
summary(Phase20$CSIn)
boxplot(CSIn~CountyID, data = Phase20, 
        main="CSI  (Coping Strategies Index)", xlab="County ID")


# now with the county names as labels..
unique(Phase20[c("County","CountyID")])
# Nyeri county has 3 different names and Embu2. Need to fix this.
Phase20$CountyName<-Phase20$County
Phase20$CountyName[Phase20$CountyID==6]<-"Nyeri"
Phase20$CountyName[Phase20$CountyID==63]<-"Embu"
Phase20$CountyName<-droplevels(Phase20$CountyName)
length(table(Phase20$CountyID))
length(table(Phase20$CountyName))
unique(Phase20[c("County","CountyName","CountyID")])

par(mar=c(6,4,2.5,1))
boxplot(MUACn~CountyID, data = Phase20, 
        main="Mid-Upper Arm Circumference (MUAC)", xaxt="n", xlab=" ",ylab="Percentage of children under five with MUAC  < 135mm")
axis(side=1, at=seq(1,24), labels=unique(Phase20$CountyName)[-9],las=2)




par(mar=c(6.5,4,2.5,1))
boxplot(CSIn~CountyID, data = Phase20, 
        main="CSI  (Coping Strategies Index)", xaxt="n", xlab=" ",ylab="CSI")
axis(side=1, at=seq(1,24), labels=unique(Phase20$CountyName)[-9],las=2)

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# patterns over phases

summary(Phase20$MUACn)
par(mar=c(3,2.5,3.5,1))
boxplot(MUACn~PhaseNum, data = Phase20,main=" MUAC",xlab="NDMA EW phase",xaxt="n")
mtext("Percentage of children under five with the Mid-Upper Arm Circumference  < 135mm",cex=0.9)
axis(side=1,labels=c("Normal","Alert","Alarm","Recovery"),at=1:4)

summary(Phase20$CSIn)
boxplot(CSIn~PhaseNum, data = Phase20, 
        main="CSI  (Coping Strategies Index)", xlab="NDMA EW phase",xaxt="n")
axis(side=1,labels=c("Normal","Alert","Alarm","Recovery"),at=1:4)

boxplot(MUACn~PhaseOrd, data = Phase20); summary(Phase20$MUACn)
boxplot(CSIn~PhaseOrd, data = Phase20); summary(Phase20$CSIn)

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# how many MUACs (and CSI)  <1?? is it possible, that it should actually be *100%?, which are those?

sum(Phase20$MUACn<1,na.rm=TRUE); sum(Phase20$MUACn<2,na.rm=TRUE)

subset(Phase20, MUACn<1)
subset(Phase20, MUACn<2)
summary(Phase20$CSIn); summary(Phase20$CSIn[Phase20$CountyID==6])
summary(Phase20$MUACn); summary(Phase20$MUACn[Phase20$CountyID==6])
# MUAC seems to be a really good measure, actually

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# how many CSIs  <1?? is it possible, that it should actually be *100%?, which are those?
summary(Phase20$CSIn);
hist(Phase20$CSIn,plot=FALSE)[1:2]
subset(Phase20, CSIn<1)
subset(Phase20, CSIn<25 & CSIn>20)
subset(Phase20, CSIn<1 & MUACn<1)
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Anova

anova(lm(MUACn ~ Month, data = Phase20))
anova(lm(MUACn ~ Year, data = Phase20))
anova(lm(MUACn ~ CountyID, data = Phase20))
anova(lm(MUACn ~ T, data = Phase20))
anova(lm(MUACn ~ as.factor(PhaseNum), data = Phase20))
anova(lm(MUACn ~ PhaseNum, data = Phase20))
anova(lm(CSIn ~ Month, data = Phase20))
anova(lm(CSIn ~ Year, data = Phase20))
anova(lm(CSIn ~ CountyID, data = Phase20))
anova(lm(CSIn ~ T, data = Phase20))
anova(lm(CSIn ~ as.factor(PhaseNum), data = Phase20))
anova(lm(CSIn ~ PhaseNum, data = Phase20))
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# save game:
rm(list=setdiff(ls(),"Phase20"))
save.image("Main/Phase20.RData")