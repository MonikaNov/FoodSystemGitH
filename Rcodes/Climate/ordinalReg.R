rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)

load("Main/Phase06.RData")

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(plm)
library(pglm)
library("MASS")
library("AER")
library("nnet")
library("lmtest")
library("erer")
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Dague<-lm(PhaseInt~Tmx+Prec+SPEI3, Phase06) 
summary(Dague)

Dague<-lm(PhaseInt~Tmx+SPEI3, Phase06) 
summary(Dague)

Dague<-lm(PhaseInt~SPEI3, Phase06) 
summary(Dague)
# first I need to turn the phase into ordered factor

Phase06$PhaseOF<- factor(Phase06$PhaseInt, levels=c(1,2,3), ordered=TRUE)
summary(Phase06$PhaseOF)
table(Phase06$PhaseInt)

Dague<-polr(PhaseOF~SPEI3,data= Phase06, model = TRUE,
            method = c("logistic")) 
summary(Dague)

(ctableb <- coef(summary(Dague)))
q        <- pnorm(abs(ctableb[, "t value"]), lower.tail=FALSE) * 2
(ctableb <- cbind(ctableb, "p value"=q))

coeftest(Dague)

mlm <- multinom(PhaseOF ~ SPEI3, data = Phase06)
summary(mlm)
M1 <- logLik(Dague)
M2 <- logLik(mlm)
(G <- -2*(M1[1] - M2[1]))
pchisq(G,2,lower.tail = FALSE)

DagueMrg<-ocME(Dague,rev.dum = FALSE)

DagueMrg$out
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Phase07<-pdata.frame(Phase06,index=c("CountyID","T"))
Dague2<-polr(PhaseOF~SPEI3+T, Phase06, model = TRUE,
            method = c("logistic")) 
summary(Dague2)

summary(Dague)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Gia<-plm(PhaseNum~Tmxz+SPEI18, Phase06, index= c("CountyID","T"), model = c("pooling"))
summary(Gia)

Gia2<-plm(PhaseNum~Tmxz+SPEI3, Phase06, index= c("CountyID","T"), model = c("within"))
summary(Gia2)

gia3<-pglm(PhaseOF~SPEI3, Phase06,  index= c("CountyID","T"),
     effect = c("individual"),model = c("pooling"),  family=ordinal('probit'))

summary(gia3)$coeff
gia3$coefficients

gia3<-pglm(PhaseOF~SPEI3, Phase07,  index= c("CountyID","T"),
           effect = c("individual"),model = c("pooling"),  family=ordinal('probit'))

summary(gia3)