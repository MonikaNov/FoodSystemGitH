aa<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
            +AvgTemp + CVTempK, random= ~1 | ID1 
        
        KEN11d<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
                    +AvgTemp + CVTempK, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
                    data=ScaledTS,na.action=na.exclude); summary(KEN11d); exp(summary(KEN11d)$coef[[1]])
        
        extractAIC(update(KEN11d,.~.,method="ML"))
        
        
        ff<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
                    +AvgTemp + CVTempK, random= ~CVPrec+1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
                    data=ScaledTS,na.action=na.exclude,method="ML"); summary(ff)
        anova(ff,update(KEN11d,.~.,method="ML"))