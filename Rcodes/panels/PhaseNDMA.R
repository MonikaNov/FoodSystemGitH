rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)

load("Main/Phase06.RData")

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(plm)
library(pglm)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
rm(Phase1)
Phase1<-list()
Phase1[[1]]<-plm(PhaseInt~Prec, Phase06, index= c("CountyID","T"))
Phase1[[2]]<-plm(PhaseInt~PrecZ, Phase06, index= c("CountyID","T"))
Phase1[[3]]<-plm(PhaseInt~Tmx, Phase06, index= c("CountyID","T"))
Phase1[[4]]<-plm(PhaseInt~Tmxz, Phase06, index= c("CountyID","T"))
Phase1[[5]]<-plm(PhaseInt~SPEI3, Phase06, index= c("CountyID","T"))
Phase1[[6]]<-plm(PhaseInt~SPEI10, Phase06, index= c("CountyID","T"))
Phase1[[7]]<-plm(PhaseInt~SPEI12, Phase06, index= c("CountyID","T"))
Phase1[[8]]<-plm(PhaseInt~SPEI18, Phase06, index= c("CountyID","T"))
lapply(Phase1,summary)


rm(Phase1vc)
Phase1vc<-list()
Phase1vc[[1]]<-pvcm(PhaseInt~Prec, Phase06, index= c("CountyID","T"),model="within")
Phase1vc[[2]]<-pvcm(PhaseInt~PrecZ, Phase06, index= c("CountyID","T"),model="within")
Phase1vc[[3]]<-pvcm(PhaseInt~Tmx, Phase06, index= c("CountyID","T"),model="within")
Phase1vc[[4]]<-pvcm(PhaseInt~Tmxz, Phase06, index= c("CountyID","T"),model="within")
Phase1vc[[5]]<-pvcm(PhaseInt~SPEI3, Phase06, index= c("CountyID","T"),model="within")
Phase1vc[[6]]<-pvcm(PhaseInt~SPEI10, Phase06, index= c("CountyID","T"),model="within")
Phase1vc[[7]]<-pvcm(PhaseInt~SPEI12, Phase06, index= c("CountyID","T"),model="within")
Phase1vc[[8]]<-pvcm(PhaseInt~SPEI18, Phase06, index= c("CountyID","T"),model="within")
lapply(Phase1vc,summary)


for (i in 1:length(Phase1))
  print(pooltest(Phase1[[i]],Phase1vc[[i]]))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
rm(Phase2)
Phase2<-list()
Phase2[[1]]<-plm(PhaseInt~Prec+I(Prec^2), Phase06, index= c("CountyID","T"))
Phase2[[2]]<-plm(PhaseInt~Tmx+I(Tmx^2), Phase06, index= c("CountyID","T"))
Phase2[[3]]<-plm(PhaseInt~Prec+I(Prec^2)+Tmx+I(Tmx^2)+I(Prec*Tmx), Phase06, index= c("CountyID","T"))
Phase2[[4]]<-plm(PhaseInt~Prec+Tmx, Phase06, index= c("CountyID","T")) #rel.good
Phase2[[5]]<-plm(PhaseInt~Prec+Tmx+I(Prec*Tmx), Phase06, index= c("CountyID","T"))

Phase2[[6]]<-plm(PhaseInt~Prec+lag(Prec), Phase06, index= c("CountyID","T")) 
Phase2[[7]]<-plm(PhaseInt~Prec+lag(Prec,2), Phase06, index= c("CountyID","T")) 
Phase2[[8]]<-plm(PhaseInt~Prec+lag(Prec,1)+lag(Prec,2)+lag(Prec,3)+Tmx+lag(Tmx,1)+lag(Tmx,2)+lag(Tmx,3), Phase06, index= c("CountyID","T")) 

lapply(Phase2,summary)

aa<-plm(PhaseInt~Prec+lag(Prec,1)+lag(Prec,2)+lag(Prec,3)+Tmx+lag(Tmx,1)+lag(Tmx,2)+lag(Tmx,3), Phase06, index= c("CountyID","T")) 
summary(aa)

aa<-plm(PhaseInt~Prec+lag(Prec,1)+lag(Prec,2)+lag(Prec,3)+lag(Prec,4)+lag(Prec,5)+Tmx+lag(Tmx,1)+lag(Tmx,2)+lag(Tmx,3)+lag(Tmx,4)+lag(Tmx,5)+lag(Tmx,6), Phase06, index= c("CountyID","T")) 
summary(aa)

aa<-plm(PhaseInt~Prec+lag(Prec,1)+lag(Prec,2)+lag(Prec,3)+lag(Prec,4)+lag(Prec,5)+lag(Prec,6)+lag(Prec,7)+lag(Prec,8)+lag(Prec,9)+lag(Prec,10)+lag(Prec,11)+lag(Prec,12)+Tmx+lag(Tmx,1)+lag(Tmx,2)+lag(Tmx,3)+lag(Tmx,4)+lag(Tmx,5)+lag(Tmx,6), Phase06, index= c("CountyID","T")) 
summary(aa)

aa<-plm(PhaseInt~Prec+lag(Prec,1)+lag(Prec,2)+lag(Prec,3)+lag(Prec,4)+lag(Prec,5)+lag(Prec,6)+lag(Prec,7)+lag(Prec,8)+lag(Prec,9)+lag(Prec,10)+lag(Prec,11)+lag(Prec,12)
        +Tmx+lag(Tmx,1)+lag(Tmx,2)+lag(Tmx,3)+lag(Tmx,4)+lag(Tmx,5)+lag(Tmx,6)+lag(Tmx,7)+lag(Tmx,8)+lag(Tmx,9)+lag(Tmx,10)+lag(Tmx,11)+lag(Tmx,12), Phase06, index= c("CountyID","T")) 
summary(aa)

aa<-plm(PhaseInt~Prec+lag(Prec,1)+lag(Prec,2)+lag(Prec,3)+lag(Prec,6)
        +Tmx+lag(Tmx,1), Phase06, index= c("CountyID","T")) 
summary(aa)


aa<-plm(PhaseInt~Prec+lag(Prec,1)+lag(Prec,3)+lag(Prec,6)
        +Tmx+lag(Tmx,1), Phase06, index= c("CountyID","T")) 
summary(aa)
#333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333
#333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333
#vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
#333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333
#333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333
#vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

theBest<-plm(PhaseInt~lag(Prec,1)+lag(Prec,3)+lag(Prec,6)
        +Tmx, Phase06, index= c("CountyID","T")) 
summary(theBest)

theBestVC<-pvcm(PhaseInt~lag(Prec,1)+lag(Prec,3)+lag(Prec,6)
                     +Tmx, Phase06, index= c("CountyID","T"),model="within")
summary(theBestVC)
pooltest(theBest,theBestVC)

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

theBest<-plm(PhaseInt~lag(Prec,1)+lag(Prec,3)
             +Tmx, Phase06, index= c("CountyID","T")) 
summary(theBest)

theBestVC<-pvcm(PhaseInt~lag(Prec,1)+lag(Prec,3)
                +Tmx, Phase06, index= c("CountyID","T"),model="within")

pooltest(theBest,theBestVC)

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------




good<-plm(PhaseInt~lag(Prec,1)+lag(Prec,3)+lag(Prec,6)
        +Tmx+SPEI10, Phase06, index= c("CountyID","T")) 
summary(good)

goodVC<-pvcm(PhaseInt~lag(Prec,1)+lag(Prec,3)+lag(Prec,6)
          +Tmx+SPEI10, Phase06, index= c("CountyID","T"),model="within") 

pooltest(good,goodVC)

good<-plm(PhaseInt~lag(Prec,1)+lag(Prec,3)+lag(Prec,6)
          +Tmx+SPEI10, Phase06, index= c("CountyID","T")) 
summary(good)


for (i in 1:length(Phase1))
  print(pooltest(Phase1[[i]],Phase1vc[[i]]))
#AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
#AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||


aa<-plm(PhaseInt~I(Prec*Tmx)+lag(Prec,1)+lag(Prec,3)+lag(Prec,6)+I(lag(Prec)*lag(Tmx))
             +Tmx, Phase06, index= c("CountyID","T")) 
summary(aa)


aa<-plm(PhaseInt~lag(Prec,1)+lag(Prec,3)+lag(Prec,6)+I(Prec^2)
             +Tmx+I(Prec^2)+I(lag(Prec,6)^2), Phase06, index= c("CountyID","T")) 
summary(aa)
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

rm(Phase2vc)
Phase2vc<-list()
Phase2vc[[1]]<-pvcm(PhaseInt~Prec+I(Prec^2),Phase06, index= c("CountyID","T"),model="within")
Phase2vc[[2]]<-pvcm(PhaseInt~Tmx+I(Tmx^2), Phase06, index= c("CountyID","T"),model="within")
Phase2vc[[3]]<-pvcm(PhaseInt~Prec+I(Prec^2)+Tmx+I(Tmx^2)+I(Prec*Tmx), Phase06,  index= c("CountyID","T"),model="within")
Phase2vc[[4]]<-pvcm(PhaseInt~Prec+Tmx, Phase06, index= c("CountyID","T"),model="within")
Phase2vc[[5]]<-pvcm(PhaseInt~Prec+Tmx+I(Prec*Tmx), Phase06,index= c("CountyID","T"),model="within")

lapply(Phase2vc,summary)

pooltest(Phase2[[4]],Phase2vc[[4]])

for (i in 1:length(Phase2))
  print(pooltest(Phase2[[i]],Phase2vc[[i]]))