wls0<-lm(Yield~i_JanPrecZ,
         data=CrMaize5)
summary(wls0)



wls1<-lm(Yield~i_JanPrecZ,weights=Area,
                     data=CrMaize5)
summary(wls1)


wls2<-lm(I(CrMaize5$Yield*(CrMaize5$Area^(1/2)))~-1+I(CrMaize5$Area^(1/2))+I(i_JanPrecZ*(CrMaize5$Area^(1/2))),
               data=CrMaize5)

 summary(wls2)