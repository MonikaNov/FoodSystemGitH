rm(list=ls())
library(ggeffects);library(ggplot2);library(dplyr); library(tseries); library(plm); library(nlme); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)
load("dataFS/Main/DaTS.RData")

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# these models are the best ones for now:
ScaledTS$foo=0.25942373
KEN11a<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
            +AvgTemp + SDTemp, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1) ,
            data=ScaledTS,na.action=na.exclude); summary(KEN11a)
# and subsamples
KEN11a_ASAL<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
                 +AvgTemp + SDTemp, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
                 data=ScaledTS[ScaledTS$ASAL==1,],na.action=na.exclude); summary(KEN11a_ASAL)

KEN11a_nonASAL<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
                    +AvgTemp + SDTemp, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
                    data=ScaledTS[ScaledTS$ASAL==0,],na.action=na.exclude); summary(KEN11a_nonASAL)
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
exp(summary(KEN11a)$tTable[,1])

AllPrec<- ggpredict(KEN11a,terms=c("SeasPr [n=20]"))
AllPrec[,c(2,4,5)]<-AllPrec[,c(2,4,5)]/exp(summary(KEN11a_ASAL)$tTable[,1])[["(Intercept)"]]

AllTemp<- ggpredict(KEN11a,terms=c("AvgTemp [n=20]"),condition)
AllTemp[,c(2,4,5)]<-AllTemp[,c(2,4,5)]/exp(summary(KEN11a)$tTable[,1])[["(Intercept)"]]

ASALPrec<- ggpredict(KEN11a_ASAL,terms=c("SeasPr [n=20]"),condition=c(AvgTemp=0,CVPrec = 0, Spell = 0,Spell4=0,SDTemp =0))
ASALPrec[,c(2,4,5)]<-ASALPrec[,c(2,4,5)]/exp(summary(KEN11a_ASAL)$tTable[,1])[["(Intercept)"]]

ASALTemp<- ggpredict(KEN11a_ASAL,terms=c("AvgTemp [n=20]"))
ASALTemp[,c(2,4,5)]<-ASALTemp[,c(2,4,5)]/exp(summary(KEN11a_ASAL)$tTable[,1])[["(Intercept)"]]

nonASALPrec<- ggpredict(KEN11a_nonASAL,terms=c("SeasPr [n=20]"),condition=c(AvgTemp=0,CVPrec = 0, Spell = 0,Spell4=0,SDTemp =0))
nonASALPrec[,c(2,4,5)]<-nonASALPrec[,c(2,4,5)]/exp(summary(KEN11a_nonASAL)$tTable[,1])[["(Intercept)"]]

nonASALTemp<- ggpredict(KEN11a_nonASAL,terms=c("AvgTemp [n=20]"),condition=c(SeasPr=0,CVPrec = 0, Spell = 0,Spell4=0,SDTemp =0))
nonASALTemp[,c(2,4,5)]<-nonASALTemp[,c(2,4,5)]/exp(summary(KEN11a_nonASAL)$tTable[,1])[["(Intercept)"]]

#----------------------------------------------------------------------------------------
plot1<-plot(AllPrec)+ylab("Yield")+xlab("Seasonal precipitation in multiples of SD")+ggtitle("Predicted values, all counties")
plot2<-plot(AllTemp)+ylab("Yield")+xlab("Average temperature in multiples of SD")+ggtitle("Predicted values, all counties")
plot3<-plot(ASALPrec)+ylab("Yield")+xlab("Seasonal precipitation in multiples of SD")+ggtitle("Predicted values, ASAL counties")
plot4<-plot(ASALTemp)+ylab("Yield")+xlab("Average temperature in multiples of SD")+ggtitle("Predicted values, ASAL counties")
plot5<-plot(nonASALPrec)+ylab("Yield")+xlab("Seasonal precipitation in multiples of SD")+ggtitle("Predicted values, non-ASAL counties")
plot6<-plot(nonASALTemp)+ylab("Yield")+xlab("Average temperature in multiples of SD")+ggtitle("Predicted values, non-ASAL counties")


require(gridExtra)
pdf("presentations/ggeffects/marginalEffects.pdf")
grid.arrange(plot1, plot2, plot3, plot4,plot5, plot6,ncol=2)
dev.off()
