MUACz<-plm(PhaseNum~PrecZ+Tmxz, Phase06, index= c("CountyID","T"))
summary(MUACz)

MUAC<-plm(PhaseNum~Prec+Tmx, Phase06, index= c("CountyID","T"))
summary(MUAC)

MUAC<-plm(PhaseNum~lag(Prec)+Prec+Tmx, Phase06, index= c("CountyID","T"))
summary(MUAC)