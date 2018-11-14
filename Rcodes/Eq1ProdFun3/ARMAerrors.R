rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

load("Main/CrMaize8.RData")
library(nlme)
library(lme4)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------


Kyle12<-lme(Yield~1+PrecZscore +TempZscore,random= ~PrecZscore+TempZscore|ID 
            ,data=CrMaize8,control = lmeControl(opt = "optim"),correlation=corAR1(0,form= ~ as.numeric(Year)|ID))
summary(Kyle12) 
anova(Kyle12)

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

Kyle212<-lme(Yield~1+PrecZscore +TempZscore,random= ~PrecZscore+TempZscore|ID 
            ,data=CrMaize8,control = lmeControl(opt = "optim"),correlation=corARMA(form = ~ as.numeric(Year)|ID, p=0,q=1))
summary(Kyle212) 

anova(Kyle12,Kyle212)


Kyle213<-lme(Yield~1+PrecZscore +TempZscore,random= ~PrecZscore+TempZscore|ID 
             ,data=CrMaize8,control = lmeControl(opt = "optim"),correlation=corARMA(form = ~ as.numeric(Year)|ID, p=0,q=3))
summary(Kyle213) 

anova(Kyle12,Kyle213)


#      !!!!!!!!!!!!!           THE BEST Kyle213          !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# more tests:
#

Kyle129<-gls(Yield~1+PrecZscore +TempZscore ,data=CrMaize8,control = lmeControl(opt = "optim"))
summary(Kyle129) 

Kyle1296<-gls(Yield~1+PrecZscore +TempZscore ,data=CrMaize8,control = lmeControl(opt = "optim")
              ,correlation=corARMA(form = ~ as.numeric(Year)|ID, p=0,q=3))
summary(Kyle1296) 

anova(Kyle1296,Kyle129)   # 1)

Kyle012<-lme(Yield~PrecZscore +TempZscore,random= ~1+PrecZscore+TempZscore|ID ,data=CrMaize8,control = lmeControl(opt = "optim"))
summary(Kyle012) 

anova(Kyle213,Kyle012)   #   2)

anova(Kyle129,Kyle012)   #   3)
anova(Kyle1296,Kyle213)   #   4)
#--------------------------

KyGls02<-gls(Yield~1+PrecZscore +TempZscore ,data=CrMaize8,control = lmeControl(opt = "optim")
              ,correlation=corARMA(form = ~ as.numeric(Year)|ID, p=0,q=2))
summary(KyGls02) 

anova(Kyle1296,KyGls02)   # 6) my
#--------------------------
Ky02<-lme(Yield~1+PrecZscore +TempZscore,random= ~PrecZscore+TempZscore|ID 
             ,data=CrMaize8,control = lmeControl(opt = "optim"),correlation=corARMA(form = ~ as.numeric(Year)|ID, p=0,q=2))
summary(Ky02) 

anova(Ky02,Kyle213)   #   7)  my

# Kyle213 also good, but probably will be abandoned in the name of parsimony

