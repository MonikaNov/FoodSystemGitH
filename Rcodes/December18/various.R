KEN11j<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4 +AvgTemp+I(AvgTemp^2) + CVTempK,
            random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
            data=ScaledTS,na.action=na.exclude); summary(KEN11j);

KEN11jML<-update(KEN11j,.~.,method="ML")
extractAIC(KEN11jML)
anova(KEN11jML,KEN11h)
anova(KEN11jML.lme,KEN11h.lme)

