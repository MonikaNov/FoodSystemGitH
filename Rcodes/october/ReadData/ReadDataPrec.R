# rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma
library('dplyr')
library('purrr')
library("reshape")
setwd(WDuni)
setwd(WDhome)

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# 1.Temp

rm(coefVarP)

MyPath<-"/home/trennion/foodSystems/dataFS/climateOct/Kenya_stats/Precip"
#or
MyPath<-"/its/home/mn301/foodSystems/dataFS/climateOct/Kenya_stats/Precip"
#or
coefVarP<- list.files(pattern="*coef_var.csv",recursive=TRUE,path= MyPath, full.names=FALSE)
coefVarP

rm(spell10P)
spell10P<- list.files(pattern="*drought_spell_10.csv",recursive=TRUE,path= MyPath, full.names=FALSE)
spell10P

rm(spell20P)
spell20P<- list.files(pattern="*drought_spell_20.csv",recursive=TRUE,path=MyPath, full.names=FALSE)
spell20P

rm(drySpellP)
drySpellP<- list.files(pattern="*dry_spell.csv",recursive=TRUE,path= MyPath, full.names=FALSE)
drySpellP

rm(maxRainP)
maxRainP<- list.files(pattern="*max_rain.csv",recursive=TRUE,path= MyPath, full.names=FALSE)
maxRainP

rm(seasCumulP)
seasCumulP<- list.files(pattern="*seas_cumul.csv",recursive=TRUE,path= MyPath, full.names=FALSE)
seasCumulP

rm(stDevP)
stDevP<- list.files(pattern="*st_dev.csv",recursive=TRUE,path=MyPath, full.names=FALSE)
stDevP

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

setwd("/home/trennion/foodSystems/dataFS/climateOct/Kenya_stats/Precip")

#or 
setwd(MyPath)

PcoefVar<-lapply(coefVarP, function(x) read.csv2(x,na.strings="-999",dec="."))
PcoefVar2<-lapply( seq(1,24,2) , function(x)    rbind(PcoefVar[[x]],PcoefVar[[x+1]])   )

Pspell10<-lapply(spell10P, function(x) read.csv2(x,na.strings="-999",dec="."))
Pspell102<-lapply( seq(1,24,2) , function(x)    rbind(Pspell10[[x]],Pspell10[[x+1]])   )

Pspell20<-lapply(spell20P, function(x) read.csv2(x,na.strings="-999",dec="."))
Pspell202<-lapply( seq(1,24,2) , function(x)    rbind(Pspell20[[x]],Pspell20[[x+1]])   )

PdrySpell<-lapply(drySpellP, function(x) read.csv2(x,na.strings="-999",dec="."))
PdrySpell2<-lapply( seq(1,24,2) , function(x)    rbind(PdrySpell[[x]],PdrySpell[[x+1]])   )

PmaxRain<-lapply(maxRainP, function(x) read.csv2(x,na.strings="-999",dec="."))
PmaxRain2<-lapply( seq(1,24,2) , function(x)    rbind(PmaxRain[[x]],PmaxRain[[x+1]])   )

PseasCumul<-lapply(seasCumulP, function(x) read.csv2(x,na.strings="-999",dec="."))
PseasCumul2<-lapply( seq(1,24,2) , function(x)    rbind(PseasCumul[[x]],PseasCumul[[x+1]])   )

PstDev<-lapply(stDevP, function(x) read.csv2(x,na.strings="-999",dec="."))
PstDev2<-lapply( seq(1,24,2) , function(x)    rbind(PstDev[[x]],PstDev[[x+1]])   )

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

setwd(WDhome) #or
setwd(WDuni)

IDdict<-read.csv("ClimateAggregM/IDdict.csv")
IDdict<-read.csv("IDdict.csv")

PcoefVar2
Pspell102
Pspell202
PdrySpell2
PmaxRain2
PseasCumul2
PstDev2

PcoefVar2<-lapply(PcoefVar2, function(xx) merge(xx,IDdict, all.x=TRUE))
Pspell102<-lapply(Pspell102, function(xx) merge(xx,IDdict, all.x=TRUE))
Pspell202<-lapply(Pspell202, function(xx) merge(xx,IDdict, all.x=TRUE))
PdrySpell2<-lapply(PdrySpell2, function(xx) merge(xx,IDdict, all.x=TRUE))
PmaxRain2<-lapply(PmaxRain2, function(xx) merge(xx,IDdict, all.x=TRUE))
PseasCumul2<-lapply(PseasCumul2, function(xx) merge(xx,IDdict, all.x=TRUE))
PstDev2<-lapply(PstDev2, function(xx) merge(xx,IDdict, all.x=TRUE))

names(PstDev2[[1]])[39]
PcoefVar2<-lapply(PcoefVar2, function(xx)  {xx=xx[-39];return(xx)})
Pspell102<-lapply(Pspell102, function(xx)  {xx=xx[-39];return(xx)})
Pspell202<-lapply(Pspell202, function(xx)  {xx=xx[-39];return(xx)})
PdrySpell2<-lapply(PdrySpell2, function(xx)  {xx=xx[-39];return(xx)})
PmaxRain2<-lapply(PmaxRain2, function(xx)  {xx=xx[-39];return(xx)})
PseasCumul2<-lapply(PseasCumul2, function(xx)  {xx=xx[-39];return(xx)})
PstDev2<-lapply(PstDev2, function(xx)  {xx=xx[-39];return(xx)})


PcoefVar3<-lapply(PcoefVar2, function(xx) melt(xx,id.vars=c("code","ADM2_NAME","ID1")))
Pspell103<-lapply(Pspell102, function(xx) melt(xx,id.vars=c("code","ADM2_NAME","ID1")))
Pspell203<-lapply(Pspell202, function(xx) melt(xx,id.vars=c("code","ADM2_NAME","ID1")))
PdrySpell3<-lapply(PdrySpell2, function(xx) melt(xx,id.vars=c("code","ADM2_NAME","ID1")))
PmaxRain3<-lapply(PmaxRain2, function(xx) melt(xx,id.vars=c("code","ADM2_NAME","ID1")))
PseasCumul3<-lapply(PseasCumul2, function(xx) melt(xx,id.vars=c("code","ADM2_NAME","ID1")))
PstDev3<-lapply(PstDev2, function(xx) melt(xx,id.vars=c("code","ADM2_NAME","ID1")))



PcoefVar3<-   lapply (PcoefVar3, function(x){ names(x)[4] ="Year";return(x)} )
Pspell103<-   lapply (Pspell103, function(x){ names(x)[4] ="Year";return(x)} )
Pspell203<-   lapply (Pspell203, function(x){ names(x)[4] ="Year";return(x)} )
PdrySpell3<-   lapply (PdrySpell3, function(x){ names(x)[4] ="Year";return(x)} )
PmaxRain3<-   lapply (PmaxRain3, function(x){ names(x)[4] ="Year";return(x)} )
PseasCumul3<-   lapply (PseasCumul3, function(x){ names(x)[4] ="Year";return(x)} )
PstDev3<-   lapply (PstDev3, function(x){ names(x)[4] ="Year";return(x)} )

names<-c("April","August","December","February","January","July","June","March","May","November","October","September")

PcoefVar3<-   mapply ( function(x,y){ names(x)[5] =y;return(x)}, PcoefVar3, names,SIMPLIFY=FALSE )
Pspell103<-   mapply ( function(x,y){ names(x)[5] =y;return(x)}, Pspell103, names,SIMPLIFY=FALSE )
Pspell203<-   mapply ( function(x,y){ names(x)[5] =y;return(x)}, Pspell203, names,SIMPLIFY=FALSE )
PdrySpell3<-   mapply ( function(x,y){ names(x)[5] =y;return(x)}, PdrySpell3, names,SIMPLIFY=FALSE )
PmaxRain3<-   mapply ( function(x,y){ names(x)[5] =y;return(x)}, PmaxRain3, names,SIMPLIFY=FALSE )
PseasCumul3<-   mapply ( function(x,y){ names(x)[5] =y;return(x)}, PseasCumul3, names,SIMPLIFY=FALSE )
PstDev3<-   mapply ( function(x,y){ names(x)[5] =y;return(x)}, PstDev3, names,SIMPLIFY=FALSE )


PcoefVar4<-reduce(PcoefVar3, merge )
Pspell104<-reduce(Pspell103, merge )
Pspell204<-reduce(Pspell203, merge )
PdrySpell4<-reduce(PdrySpell3, merge )
PmaxRain4<-reduce(PmaxRain3, merge )
PseasCumul4<-reduce(PseasCumul3, merge )
PstDev4<-reduce(PstDev3, merge )


PcoefVar4[,"Year"]<-substr(PcoefVar4[,"Year"],2,5)
Pspell104[,"Year"]<-substr(Pspell104[,"Year"],2,5)
Pspell204[,"Year"]<-substr(Pspell204[,"Year"],2,5)
PdrySpell4[,"Year"]<-substr(PdrySpell4[,"Year"],2,5)
PmaxRain4[,"Year"]<-substr(PmaxRain4[,"Year"],2,5)
PseasCumul4[,"Year"]<-substr(PseasCumul4[,"Year"],2,5)
PstDev4[,"Year"]<-substr(PstDev4[,"Year"],2,5)


allPrep<-list("PrecCV"=PcoefVar4,"Spell10"=Pspell104,"Spell20"=Pspell204,"DrySpell"= PdrySpell4,
              "MaxRain"=PmaxRain4,"CumulPrec"=PseasCumul4,"PrecStDev"=PstDev4)

names(allPrep)
allPrep2<-lapply(1:7, function(xx) {names(allPrep[[xx]])[5:16]<-paste0(names(allPrep)[xx],names(allPrep[[xx]])[5:16]); return(allPrep[[xx]])} )

allPrep3<-reduce(allPrep2, merge )



load("Main/CrMaize15.RData") # the last (and the best available) dataset before I cut it timewise..
rm(CrMaize20)
CrMaize20<-subset(CrMaize15,Year %in% (1981:2014),select=c("Admin1","county","Admin2","Year","Area","Yield","MT","ID","west1"))
names(CrMaize20)[8]<-"ID1"

dataAll<-merge(allPrep3,CrMaize20,all.x=TRUE)
dataAll<-dataAll[order(dataAll$ID1,dataAll$Year ),]

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

rm(list=setdiff(ls(), c("dataAll"))) #cool, I have checked it at the uni and all seems to be fine..

# save.image("~/foodSystems/dataFS/Main/dataOctober.RData")