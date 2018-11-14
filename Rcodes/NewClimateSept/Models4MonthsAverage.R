rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)

library('dplyr')
library('tseries')
library(plm)
library(lme4)
library(lattice)
library(car)
library(lmerTest)
library(optimx)

source("/its/home/mn301/foodSystems/Rcodes/NewClimateSept/Models3GettingFrames.R")

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# just training..how dooes formula work...
names(FrameMonths)

foo0<-lmer(Yield ~AprilP +AugustP+(1|ID1),data=FrameMonths)
summary(foo0)

foo<-lmer(Yield ~(1|ID1)+.-ID1,data=FrameMonths[c(1,3,4,5)])
summary(foo)

foo0<-lmer(Yield ~AprilP +AugustP+DecemberP+(1+AprilP +AugustP+DecemberP|ID1),data=FrameMonths)
summary(foo0)

foo<-lmer(Yield ~(1+.-ID1-Yield|ID1)+.-ID1,data=FrameMonths[c(1,3,4,5,6)])
summary(foo)

n<-names(FrameMonths)

paste(n[4:6], collapse = " + ")

foo<-lmer(Yield ~(1+.-ID1-Yield|ID1)+.-ID1,data=FrameMonths[c(1,3,4,5,6)])
summary(foo)


n<-names(FrameMonths[c(1,3,4,5,6)])
f <- as.formula(   paste("Yield ~", paste(n[!n %in% c("Yield","ID1") ], collapse = " + ") ,"+(1+",     paste(n[!n %in% c("Yield","ID1") ], collapse = " + "),"|ID1)"   )      )   
foo2<-lmer(f,data=FrameMonths )
summary(foo2)
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------

foobar0<-lmer(log(Yield) ~(1+.-ID1-Yield|ID1)+.-ID1,data=FrameMonthsSc[c(1,3,4:15)])
summary(foobar0)




foobar<-lmer(Yield ~(1+.-ID1-Yield|ID1)+.-ID1,data=FrameMonthsSc[c(1,3,4:15)])
summary(foobar)

# unfortunatelly, I have to write the models as follows (foob is the same as foobar) as there is a bug in the Step function...:
n<-names(FrameMonthsSc[c(1,3,4:15)])
f <- as.formula(   paste("Yield ~", paste(n[!n %in% c("Yield","ID1") ], collapse = " + ") ,"+(1+",     paste(n[!n %in% c("Yield","ID1") ], collapse = " + "),"|ID1)"   )      )   
foob<-lmer(f,data=FrameMonthsSc )
summary(foob)


foobarStep<-step(foob)
summary(get_model(step(foobar)))

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

save.image("~/foodSystems/Rcodes/NewClimateSept/Models4.RData")