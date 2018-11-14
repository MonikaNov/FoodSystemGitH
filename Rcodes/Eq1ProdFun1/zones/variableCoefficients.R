rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

load("~/foodSystems/dataFS/Main/CrMaize8.RData")
library(plm)
#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# CONCLUSION (spoiler alert):
# SO THERE ARE COUPLE OF COUNTIES WHERE PREC. SINIFICANT EVEN IF SMALL SAMPLE REGRESSIONS FOR INDIVIDUAL COUNTIES..
#---------------------------------------------------------------------------------------------------------------------------------------------------------------

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
 
# Probably better no weights (ANnemie strobgly not in favor of them) so Dale32 the best for now

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

Dale30<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"), data=CrMaize8)
summary(Dale30)

Dale32<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"), effect="twoway",model="random",data=CrMaize8)
summary(Dale32)

Dale321<-plm(Yield~PrecZones+TempZones,index=c("ID","Year"), effect="twoway",model="random",data=CrMaize8)
summary(Dale321)

Kimmy1<-pggls(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"),effect="individual", data=CrMaize8)
summary(Kimmy1)

Kimmy1<-pggls(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"),effect="individual",model="random", data=CrMaize8)
summary(Kimmy1)

Kimmy1<-pggls(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"),effect="time",model="within", data=CrMaize8)
summary(Kimmy1)

Kimmy1<-pggls(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"),effect="time",model="random", data=CrMaize8)
summary(Kimmy1)


#---------------------------------------------------------------------------------------------------------------------------------------------------------------
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo


# now variable coefficient random = all regressors in fixed effects and in random effects. Pe liked it

Kimmy2<-pvcm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"),model=c("within"), data=CrMaize8)
summary(Kimmy2)

# is this same as running regressions for each county separately? and are the estimates significant?

Kimmy2$coefficients
Kimmy21<-lm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,subset= c(ID==1), data=CrMaize8)
summary(Kimmy21)


Kimmy2$coefficients[1,]

Kimmy21<-lm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,subset= c(ID==36), data=CrMaize8)
summary(Kimmy21)
Kimmy2$coefficients['36',]
Kimmy2$std.error['36',]

rownames(Kimmy2$coefficients)
# so it seems that both coefficients and standard errors in this pvcm are the  same as estimating the regressions separately for each county

# It could also be good to look at some descriptive of the coefficients 

Kimmy2$coefficients[,2]


plot(Kimmy2$coefficients[,4],type='l')

lines(Kimmy2$coefficients[,2],type='l',col=2)

plot(Kimmy2$coefficients[,2],Kimmy2$coefficients[,3])

lines(Kimmy2$coefficients[,2],Kimmy2$coefficients[,3])
plot(Kimmy2$coefficients[,2],Kimmy2$coefficients[,3],type='l')

plot(Kimmy2$coefficients[,2],Kimmy2$coefficients[,4])
lines(Kimmy2$coefficients[,2],Kimmy2$coefficients[,4])

plot(Kimmy2$coefficients[,3],Kimmy2$coefficients[,4])
lines(Kimmy2$coefficients[,3],Kimmy2$coefficients[,4])

# now just get p-values from all the county-specific regression >> I guess some apply and lists will be needed

Kimmy21<-lm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,subset= c(ID==1), data=CrMaize8)
summary(Kimmy21)
rownames(Kimmy2$coefficients)
summary(Kimmy21)$coeff[,4]
#---------------------------------------------------------------------------------------------------------------------------------------------------------------
pvals<-function(x) summary(lm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,subset= c(ID==x), data=CrMaize8))$coeff[,4]

MyPvals<-lapply(rownames(Kimmy2$coefficients),pvals )

significance<-sapply(MyPvals, function(x)  sum(x<0.1) )

signif<-rownames(Kimmy2$coefficients)[significance>0]


LMsignif<-lapply(signif,  function(x) lm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,subset= c(ID==x), data=CrMaize8))

lapply(LMsignif,summary)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# I want to see it!!!
signif
Kimmy21<-lm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,subset= c(ID==16), data=CrMaize8)
summary(Kimmy21)
myNames<-sapply(signif,function(x)  as.character(unique(CrMaize8$name[which(CrMaize8$ID==x)])))

names(LMsignif)<-myNames

lapply(LMsignif,summary)
signif
as.character(unique(CrMaize8$name[which(CrMaize8$ID==16)]))
#---------------------------------------------------------------------------------------------------------------------------------------------------------------
#now I want to see for how many p-val for prec signif

PrecPval<-function(x) summary(lm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,subset= c(ID==x), data=CrMaize8))$coeff[2,4]

myPrecPval<-lapply(rownames(Kimmy2$coefficients),PrecPval )

signPval<-sapply(myPrecPval, function(x)  sum(x<0.05) )
aa<-rownames(Kimmy2$coefficients)[signPval>0]
Precsignif<-lapply(aa,  function(x) lm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,subset= c(ID==x), data=CrMaize8))
myNames<-sapply(aa,function(x)  as.character(unique(CrMaize8$name[which(CrMaize8$ID==x)])))

names(Precsignif)<-myNames
lapply(Precsignif,summary)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# CONCLUSION:
# SO THERE ARE COUPLE OF COUNTIES WHERE PREC. SINIFICANT EVEN IF SMALL SAMPLE REGRESSIONS FOR INDIVIDUAL COUNTIES..
#---------------------------------------------------------------------------------------------------------------------------------------------------------------
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

Kimmy3<-pvcm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"),model=c("random"), data=CrMaize8)
summary(Kimmy3)