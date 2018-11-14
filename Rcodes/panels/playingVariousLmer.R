library(lme4)  # load library
library(arm)  # convenience functions for regression in R
lmm.data <- read.table("http://www.unt.edu/rss/class/Jon/R_SC/Module9/lmm.data.txt", 
                       header = TRUE, sep = ",", na.strings = "NA", dec = ".", strip.white = TRUE)
 head(lmm.data)
 
 
 Dale30<-lm(Yield~PrecZones+PrecZonVar+as.factor(ID)-1+TempZones+TempZonVar, data=CrMaize8)
 summary(Dale30)

 
 Dale30<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"), data=CrMaize8)
 summary(Dale30)
 
 
 
 Dale88<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,effect="twoway",index=c("ID","Year"), data=CrMaize8)
 summary(Dale88)
 
 Dale87<-plm(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar,effect="individual",index=c("ID","Year"), data=CrMaize8)
 summary(Dale87)
 
 pFtest(Dale88,Dale87)
 
 pFtest(Dale88)
 
 Malcolm<- lmer(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar+(1|ID)+(1|Year), data=CrMaize8)
 summary(Malcolm)
 Malcolm2<-lmer(Yield~(1+PrecZones+PrecZonVar+TempZones+TempZonVar|ID), data=CrMaize8)
 summary(Malcolm2)
 
 Malcolm2<-lmer(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar+(1|ID), data=CrMaize8)
 summary(Malcolm2)
 
 Malcolm2<-gls(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar+(1|ID), data=CrMaize8)
 summary(Malcolm2)
 
 
 Malcolm4<-lmer(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar+(PrecZones+PrecZonVar+TempZones+TempZonVar+1|ID), data=CrMaize8)
 summary(Malcolm4)
#################################################################################################################################################################################################################################################################################################################################################################################################################################################################
 
 Dale309<-plm(Yield~PrecZones+(PrecZones:factor(ID))+PrecZonVar+TempZones+TempZonVar,index=c("ID","Year"), data=CrMaize8)
 summary(Dale309)
 
 Malcolm2<-lmer(Yield~PrecZones+PrecZonVar+TempZones+TempZonVar+(1+PrecZones|ID), data=CrMaize8)
 summary(Malcolm2)
 
 Malcolm5<-lmer(Yield~+PrecZonVar+TempZonVar+(1+TempZones+PrecZones|ID),data=CrMaize8)
 summary(Malcolm5)
 Malcolm6<-lmer(Yield~+PrecZonVar+TempZonVar+(1+TempZones+PrecZones|ID),weights=Area, data=CrMaize8)
 summary(Malcolm6)
 Malcolm6<-lmer(Yield~+TempZones+PrecZones+TempZonVar+PrecZonVar+(TempZones+PrecZones+TempZonVar+PrecZonVar|ID),weights=Area, data=CrMaize8)
 summary(Malcolm6)
 
#################################################################################################################################################################################################################################################################################################################################################################################################################################################################
 
 
 
Dale733<-plm(Yield~PrecZones,index=c("ID","Year"),model="random",data=CrMaize8)
summary(Dale733)
 

Malcolm2<-lme(Yield~PrecZones,random= ~1|ID, data=CrMaize8)
summary(Malcolm2)

Malcolm2<-lmer(Yield~PrecZones,random= ~1|ID, data=CrMaize8)
summary(Malcolm2)