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
par(mfrow=c(3,2))
ff=function(x) x/1.296
ggpredict(KEN11a,terms=c("SeasPr [n=10]"));exp(summary(KEN11a)$tTable[,1])
ggpredict(KEN11a,terms=c("SeasPr") )/1.2961829
ggpredict(KEN11a,terms=c("SeasPr"),typical=function(x) mean(x,na.rm=TRUE)/1.2961829)

plot(ggpredict(KEN11a,terms=c("SeasPr [n=10]")))+ylab("Yield")+xlab("Seasonal precipitation in multiples of SD")
plot(ggpredict(KEN11a,terms=c("AvgTemp")))+ylab("Yield")+xlab("Average temperature in multiples of SD")


plot(ggpredict(KEN11a_ASAL,terms=c("SeasPr")))+ylab("Yield")+xlab("Seasonal precipitation in multiples of SD")
plot(ggpredict(KEN11a_ASAL,terms=c("AvgTemp")))+ylab("Yield")+xlab("Average temperature in multiples of SD")

plot(ggpredict(KEN11a_nonASAL,terms=c("SeasPr"))  )+ylab("Yield")+xlab("Seasonal precipitation in multiples of SD")
plot(ggpredict(KEN11a_nonASAL,terms=c("AvgTemp [all]")))+ylab("Yield")+xlab("Average temperature in multiples of SD")

require(gridExtra)


plot1<-plot(ggpredict(KEN11a,terms=c("SeasPr") )/1.2961829)+ylab("Yield")+xlab("Seasonal precipitation in multiples of SD")+ggtitle("Predicted values, all counties")
plot2<-plot(ggpredict(KEN11a,terms=c("AvgTemp")))+ylab("Yield")+xlab("Average temperature in multiples of SD")+ggtitle("Predicted values, all counties")
plot3<-plot(ggpredict(KEN11a_ASAL,terms=c("SeasPr")))+ylab("Yield")+xlab("Seasonal precipitation in multiples of SD")+ggtitle("Predicted values, ASAL counties")
plot4<-plot(ggpredict(KEN11a_ASAL,terms=c("AvgTemp")))+ylab("Yield")+xlab("Average temperature in multiples of SD")+ggtitle("Predicted values, ASAL counties")
plot5<-plot(ggpredict(KEN11a_nonASAL,terms=c("SeasPr"))  )+ylab("Yield")+xlab("Seasonal precipitation in multiples of SD")+ggtitle("Predicted values, non-ASAL counties")
plot6<-plot(ggpredict(KEN11a_nonASAL,terms=c("AvgTemp [all]")))+ylab("Yield")+xlab("Average temperature in multiples of SD")+ggtitle("Predicted values, non-ASAL counties")
pdf("presentations/ggeffects/KEN11a.pdf")
grid.arrange(plot1, plot2, plot3, plot4,plot5, plot6,ncol=2)
dev.off()


plot1<-plot(ggpredict(KEN11a,terms=c("SeasPr") ))+ylab("Yield")+xlab("Seasonal precipitation in multiples SD")+ggtitle("All counties")
plot2<-plot(ggpredict(KEN11a,terms=c("AvgTemp")))+ylab("Yield")+xlab("Average temperature in multiples SD")+ggtitle("All counties")
plot3<-plot(ggpredict(KEN11a_ASAL,terms=c("SeasPr")))+ylab("Yield")+xlab("Seasonal precipitation in multiples SD")+ggtitle("ASAL counties")
plot4<-plot(ggpredict(KEN11a_ASAL,terms=c("AvgTemp")))+ylab("Yield")+xlab("Average temperature in multiples SD")+ggtitle("ASAL counties")
plot5<-plot(ggpredict(KEN11a_nonASAL,terms=c("SeasPr"))  )+ylab("Yield")+xlab("Seasonal precipitation in multiples SD")+ggtitle("Non-ASAL counties")
plot6<-plot(ggpredict(KEN11a_nonASAL,terms=c("AvgTemp [all]")))+ylab("Yield")+xlab("Average temperature in multiples SD")+ggtitle("Non-ASAL counties")
pdf("presentations/ggeffects/marginalEffects.pdf")
grid.arrange(plot1, plot2, plot3, plot4,plot5, plot6,ncol=2)
dev.off()
