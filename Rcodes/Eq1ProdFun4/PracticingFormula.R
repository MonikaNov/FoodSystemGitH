test1<-lm(Yield~(PreMedZ + TemMed+PreMedCV)^3,data=CrMaize16)
summary(test1)

test3<-lm(Yield~PreMedZ * TemMed ,data=CrMaize16)
summary(test3)
test4<-lm(Yield~PreMedZ+TemMed+PreMedZ : TemMed,data=CrMaize16)
summary(test4)

test4<-lm(Yield~PreMedZ+TemMed+I(PreMedZ * TemMed),data=CrMaize16)
summary(test4)