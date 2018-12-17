
KEN11d_ML<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
               +AvgTemp + CVTempK, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
               data=ScaledTS,na.action=na.exclude,method="ML"); summary(KEN11d_ML)


KEN11l_ML<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
               +AvgTemp + CVTempK+HWDays+MaxT, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
               data=ScaledTS,na.action=na.exclude,method="ML"); summary(KEN11l_ML)
anova(KEN11d_ML,KEN11l_ML)

KEN11d<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
               +AvgTemp + CVTempK, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
               data=ScaledTS,na.action=na.exclude); summary(KEN11d)


KEN11l<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
            +AvgTemp + CVTempK+HWDays+MaxT, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
            data=ScaledTS,na.action=na.exclude); summary(KEN11l)

vif(KEN11l)

cor.test(ScaledTS$AvgTemp,ScaledTS$MaxT)
cor.test(ScaledTS$AvgTemp,ScaledTS$HWDays)

KEN11l<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
            +AvgTemp + CVTempK+HWDays, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
            data=ScaledTS,na.action=na.exclude); summary(KEN11d)

vif(KEN11l)
anova(KEN11d,KEN11l)