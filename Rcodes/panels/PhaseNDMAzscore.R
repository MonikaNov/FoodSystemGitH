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
Phase2[[1]]<-plm(PhaseInt~PrecZ+I(PrecZ^2), Phase06, index= c("CountyID","T"))
Phase2[[2]]<-plm(PhaseInt~Tmxz+I(Tmxz^2), Phase06, index= c("CountyID","T"))
Phase2[[3]]<-plm(PhaseInt~PrecZ+I(PrecZ^2)+Tmxz+I(Tmxz^2)+I(PrecZ*Tmxz), Phase06, index= c("CountyID","T"))
Phase2[[4]]<-plm(PhaseInt~PrecZ+Tmxz, Phase06, index= c("CountyID","T")) #rel.good
Phase2[[5]]<-plm(PhaseInt~PrecZ+Tmxz+I(PrecZ*Tmxz), Phase06, index= c("CountyID","T"))

Phase2[[6]]<-plm(PhaseInt~PrecZ+lag(PrecZ), Phase06, index= c("CountyID","T")) 
Phase2[[7]]<-plm(PhaseInt~PrecZ+lag(PrecZ,2), Phase06, index= c("CountyID","T")) 
Phase2[[8]]<-plm(PhaseInt~PrecZ+lag(PrecZ,1)+lag(PrecZ,2)+lag(PrecZ,3)+Tmxz+lag(Tmxz,1)+lag(Tmxz,2)+lag(Tmxz,3), Phase06, index= c("CountyID","T")) 

lapply(Phase2,summary)

aa<-plm(PhaseInt~PrecZ+lag(PrecZ,1)+lag(PrecZ,2)+lag(PrecZ,3)+Tmxz+lag(Tmxz,1)+lag(Tmxz,2)+lag(Tmxz,3), Phase06, index= c("CountyID","T")) 
summary(aa)

aa<-plm(PhaseInt~PrecZ+lag(PrecZ,1)+lag(PrecZ,2)+lag(PrecZ,3)+lag(PrecZ,4)+lag(PrecZ,5)+Tmxz+lag(Tmxz,1)+lag(Tmxz,2)+lag(Tmxz,3)+lag(Tmxz,4)+lag(Tmxz,5)+lag(Tmxz,6), Phase06, index= c("CountyID","T")) 
summary(aa)

aa<-plm(PhaseInt~PrecZ+lag(PrecZ,1)+lag(PrecZ,2)+lag(PrecZ,3)+lag(PrecZ,4)+lag(PrecZ,5)+lag(PrecZ,6)+lag(PrecZ,7)+lag(PrecZ,8)+lag(PrecZ,9)+lag(PrecZ,10)+lag(PrecZ,11)+lag(PrecZ,12)+Tmxz+lag(Tmxz,1)+lag(Tmxz,2)+lag(Tmxz,3)+lag(Tmxz,4)+lag(Tmxz,5)+lag(Tmxz,6), Phase06, index= c("CountyID","T")) 
summary(aa)

aa<-plm(PhaseInt~PrecZ+lag(PrecZ,1)+lag(PrecZ,2)+lag(PrecZ,3)+lag(PrecZ,4)+lag(PrecZ,5)+lag(PrecZ,6)+lag(PrecZ,7)+lag(PrecZ,8)+lag(PrecZ,9)+lag(PrecZ,10)+lag(PrecZ,11)+lag(PrecZ,12)
        +Tmxz+lag(Tmxz,1)+lag(Tmxz,2)+lag(Tmxz,3)+lag(Tmxz,4)+lag(Tmxz,5)+lag(Tmxz,6)+lag(Tmxz,7)+lag(Tmxz,8)+lag(Tmxz,9)+lag(Tmxz,10)+lag(Tmxz,11)+lag(Tmxz,12), Phase06, index= c("CountyID","T")) 
summary(aa)

aa<-plm(PhaseInt~PrecZ+lag(PrecZ,1)+lag(PrecZ,2)+lag(PrecZ,3)+lag(PrecZ,6)
        +Tmxz+lag(Tmxz,1), Phase06, index= c("CountyID","T")) 
summary(aa)


aa<-plm(PhaseInt~PrecZ+lag(PrecZ,1)+lag(PrecZ,2)+lag(PrecZ,3)
        +Tmxz, Phase06, index= c("CountyID","T")) 
summary(aa)
#333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333
#333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333
#vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
#333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333
#333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333
#vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

maybe<-plm(PhaseInt~PrecZ+lag(PrecZ,1)+lag(PrecZ,2)+lag(PrecZ,3)
             +Tmxz, Phase06, index= c("CountyID","T")) 
summary(maybe)

maybeVC<-pvcm(PhaseInt~PrecZ+lag(PrecZ,1)+lag(PrecZ,2)+lag(PrecZ,3)
                              +Tmxz, Phase06, index= c("CountyID","T"),model="within")
summary(maybeVC)
pooltest(maybe,maybeVC)




#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
maybe<-plm(PhaseInt~PrecZ+lag(PrecZ,1)+lag(PrecZ,2)+lag(PrecZ,3)
           , Phase06, index= c("CountyID","T")) 
summary(maybe)

maybeVC<-pvcm(PhaseInt~PrecZ+lag(PrecZ,1)+lag(PrecZ,2)+lag(PrecZ,3)
              , Phase06, index= c("CountyID","T"),model="within")
summary(maybeVC)
pooltest(maybe,maybeVC)

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

maybe<-plm(PhaseInt~lag(PrecZ,1)+lag(PrecZ,2)+lag(PrecZ,3)
           +Tmxz, Phase06, index= c("CountyID","T")) 
summary(maybe)

maybeVC<-pvcm(PhaseInt~lag(PrecZ,1)+lag(PrecZ,2)+lag(PrecZ,3)
              +Tmxz, Phase06, index= c("CountyID","T"),model="within")
summary(maybeVC)
pooltest(maybe,maybeVC)
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

maybe<-plm(PhaseInt~lag(PrecZ,1)+lag(PrecZ,2)+lag(PrecZ,3),
            Phase06, index= c("CountyID","T")) 
summary(maybe)

maybeVC<-pvcm(PhaseInt~lag(PrecZ,1)+lag(PrecZ,2)+lag(PrecZ,3),
           Phase06, index= c("CountyID","T"),model="within")
summary(maybeVC)
pooltest(maybe,maybeVC)

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

maybe<-plm(PhaseInt~lag(PrecZ,1)+lag(PrecZ,3),
           Phase06, index= c("CountyID","T")) 
summary(maybe)

maybeVC<-pvcm(PhaseInt~lag(PrecZ,1)+lag(PrecZ,3),
              Phase06, index= c("CountyID","T"),model="within")
summary(maybeVC)
pooltest(maybe,maybeVC)

#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo



aa<-plm(PhaseInt~PrecZ+lag(PrecZ,1)+lag(PrecZ,2)+lag(PrecZ,3)
        +Tmxz+I(PrecZ^2)+I(lag(PrecZ)^2+I(lag(PrecZ,2)^2))+I(lag(PrecZ,3)^2)+I(Tmxz^2)+I(lag(Tmxz)^2)
        +I(lag(Tmxz,2)^2)+I(lag(Tmxz,3)^2), Phase06, index= c("CountyID","T")) 
summary(aa)


aa<-plm(PhaseInt~PrecZ+lag(PrecZ,1)+lag(PrecZ,2)+lag(PrecZ,3)
        +Tmxz+I(PrecZ^2)+I(lag(PrecZ)^2)+I(lag(PrecZ,2)^2)+I(lag(PrecZ,3)^2)+I(Tmxz^2)+I(lag(Tmxz)^2)
        +I(lag(Tmxz,2)^2)+I(lag(Tmxz,3)^2), Phase06, index= c("CountyID","T")) 
summary(aa)


interesting<-plm(PhaseInt~PrecZ+I(PrecZ^2)+lag(PrecZ,1)+lag(PrecZ,3)
        +I(lag(Tmxz,2)^2), Phase06, index= c("CountyID","T")) 
summary(interesting)


interesting<-plm(PhaseInt~PrecZ+I(PrecZ^2)+I(lag(PrecZ)*lag(Tmxz))+lag(PrecZ,1)+lag(PrecZ,3)
                 +I(lag(Tmxz,2)^2), Phase06, index= c("CountyID","T")) 
summary(interesting)


interesting<-plm(PhaseInt~I(PrecZ^2)+I(lag(PrecZ)*lag(Tmxz))+lag(PrecZ,1)+lag(PrecZ,3)
                 +I(lag(Tmxz,2)^2), Phase06, index= c("CountyID","T")) 
summary(interesting)


#hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
#hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh

interesting<-plm(PhaseInt~I(lag(PrecZ)*lag(Tmxz))+lag(PrecZ,1)+lag(PrecZ,3)
                 +I(lag(Tmxz,2)^2), Phase06, index= c("CountyID","T")) 
summary(interesting)

interestingVC<-pvcm(PhaseInt~I(lag(PrecZ)*lag(Tmxz))+lag(PrecZ,1)+lag(PrecZ,3)
                 +I(lag(Tmxz,2)^2), Phase06, index= c("CountyID","T"),model="within")
summary(interestingVC)

pooltest(interesting,interestingVC)
#hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
#hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh

aa<-plm(PhaseInt~PrecZ+lag(PrecZ,1)+lag(PrecZ,2)+lag(PrecZ,3)
        +Tmxz+I(Tmxz^2)+I(lag(Tmxz)^2)
        +I(lag(Tmxz,2)^2), Phase06, index= c("CountyID","T")) 
summary(aa)

theBest<-plm(PhaseInt~lag(Prec,1)+lag(Prec,3)
             +Tmx, Phase06, index= c("CountyID","T")) 
summary(theBest)

theBestVC<-pvcm(PhaseInt~lag(Prec,1)+lag(Prec,3)
                +Tmx, Phase06, index= c("CountyID","T"),model="within")

pooltest(theBest,theBestVC)

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


