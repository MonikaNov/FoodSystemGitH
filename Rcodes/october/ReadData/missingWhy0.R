# rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma
library('dplyr')
library('purrr')
library("reshape")
setwd(WDuni)
setwd(WDhome)

load("Main/dataOct.RData")
load("~/foodSystems/Rcodes/october/ReadData/missingWhy0.RData")
#-------------------------------------------------------------------------------------------------------------------------------------------

names(dataAll)
foo<-lm(Yield ~.-Yield,data=dataAll[-c(3,4,89:92,94,95)])
summary(foo)


names(dataAll)
foo<-lm(Yield ~.-Yield-Area-Admin1-Admin2-Yield-MT-west1-county-ADM2_NAME-code-ID1-Year,data=dataAll)
summary(foo)
nobs(foo)

#-------------------------------------------------------------------------------------------------------------------------------------------

names(dataAll)
foo2<-lm(Yield ~.-Yield,data=dataAll[-c(1,2,3,4,149:152,154:155)])
summary(foo2)

# okk. still, too small number of observations

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# Now I will try to test it into Peggy

Peggy45<-lmer(Yield ~ I((SeasRain_MAM_L1+SeasRain_OND_L1)*(1-west1)) + I((SeasRain_MAM_L1)*west1)+ I(AvgTemp_MarSep_L1*(1-west1)) + I(AvgTemp_MarSep_L1*(west1))
              +I(SDtemp_OctMar_L1*(1-west1))+    +I(SDtemp_OctMar_L1*west1)
              
              +   I(MaxRain_OND*(1-west1)) +   I(MaxRain_OND*west1) 
              
              +(1+MaxRain_OND+I(SeasRain_MAM_L1+SeasRain_OND_L1) +I(AvgTemp_MarSep_L1)|ID1) ,data=dataScTS )
summary(Peggy45)  
nobs(Peggy45)
Peggy44<-update(Peggy44,data=dataScTS)
anova(Peggy44,Peggy45)  


#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
baz1<-lmer(Yield ~ .-Yield-ID1 +(1|ID1) ,data=dataAllScTS[c(1:2,5:148,153,155)] )
summary(baz1)  
nobs(baz1)  

baz1<-lm(Yield ~ .-Yield-ID1 ,data=dataAllScTS[c(1:2,5:148,153,155)] )
summary(baz1)  
nobs(baz1)  

baz1<-lmer(Yield ~ .-Yield-ID1 +(1|ID1) ,data=dataAllScTS[c(1:2,5:14,153)] )
summary(baz1)  
nobs(baz1)  


baz1<-lmer(Yield ~ .-Yield-ID1 +(1|ID1) ,data=dataAllScTS[c(1:2,5:30,153)] )
summary(baz1)  
nobs(baz1)  


baz1<-lmer(Yield ~ .-Yield-ID1 +(1|ID1) ,data=dataAllScTS[c(1:2,153)] )
summary(baz1)  
nobs(baz1)  


baz1<-lmer(Yield ~ .-Yield-ID1 +(1|ID1) ,data=dataAllScTS[c(1:2,5,153)] )
summary(baz1)  
nobs(baz1)  

baz1<-lmer(Yield ~ .-Yield-ID1 +(1|ID1) ,data=dataAllScTS[c(1:2,5:7,153)] )
summary(baz1)  
nobs(baz1)  


rm(aa)
aa<-sapply(seq(6,40),function(x) c(x,nobs(lmer(Yield ~ .-Yield-ID1 +(1|ID1) ,data=dataAllScTS[c(1:2,5:x,153)] ))  ))

rm(bb)
bb<-sapply(seq(6,45),function(x) c(x,nobs(lm(Yield ~ .-Yield ,data=dataAllScTS[c(1:2,5:x,153)] ))  ))

rm(cc)
cc<-sapply(seq(6,148),function(x) c(x,nobs(lm(Yield ~ .-Yield-ID1 ,data=dataAll[c(1:2,5:x,153)] ))  ))

rm(dd)
dd<-sapply(c(seq(6,148),seq(156,179)),function(x) c(x,nobs(lm(Yield ~ .-Yield-ID1 ,data=dataAll[c(1:2,5:x,153)] ))  ))

rm(ddd)
ddd<-sapply(c(seq(6,148),seq(156,179)),function(x) nobs(lm(Yield ~ .-Yield-ID1 ,data=dataAll[c(1:2,5:x,153)] ))  )

rm(complete)
complete<-sapply(c(seq(6,148),seq(156,179)),function(x) sum(complete.cases(dataAll[c(5:x,153)] ))  )


varI<-c(seq(5,148),seq(156,179) )

table(dataAll[ is.na(dataAll[5]),"Year"] )
table(dataAll[ is.na(dataAll[5]),"ADM2_NAME"] )

rm(myNAS)
myNAS<- lapply(varI, function(x) {return(list(name=names(dataAll)[x], 
                                      YrsNA=table(dataAll[ is.na(dataAll[x]),"Year"] ),
                                      cntyNA=table(dataAll[ is.na(dataAll[x]),"ADM2_NAME"] ))  )  } )



rm(aa)
aa<-sapply(seq(6,148),function(x) c(x,nobs(lmer(Yield ~ .-Yield-ID1 +(1|ID1) ,data=dataAll[c(1:2,5:x,153)] ))  ))

#ok, nice
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

#     save.image("~/foodSystems/Rcodes/october/ReadData/missingWhy0.RData")






