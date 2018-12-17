rm(list=ls())
library(ggeffects);library(ggplot2);library(dplyr); library(tseries); library(plm); library(nlme); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)
load("dataFS/Main/DaTS.RData")

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# these models are the best ones for now:

KEN11d<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
            +AvgTemp + CVTempK, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1) ,
            data=ScaledTS,na.action=na.exclude); summary(KEN11d)
# and subsamples
KEN11d_ASAL<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
                 +AvgTemp + CVTempK, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
                 data=ScaledTS[ScaledTS$ASAL==1,],na.action=na.exclude); summary(KEN11d_ASAL)

KEN11d_nonASAL<-lme(log(Yield0)~SeasPr+I(SeasPr^2)+CVPrec+Spell+Spell4
                    +AvgTemp + CVTempK, random= ~1 | ID1,correlation=corARMA(form = ~ as.numeric(Year)|ID1, p=1,q=1),
                    data=ScaledTS[ScaledTS$ASAL==0,],na.action=na.exclude); summary(KEN11d_nonASAL)
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
exp(summary(KEN11d)$tTable[,1]); exp(summary(KEN11d_ASAL)$tTable[,1]);exp(summary(KEN11d_nonASAL)$tTable[,1])

AllPrec<- ggpredict(KEN11d,terms=c("SeasPr [n=20]"))
  AllPrec[,c(2,4,5)]<-AllPrec[,c(2,4,5)]/exp(summary(KEN11d)$tTable[,1])[["(Intercept)"]]
AllTemp<- ggpredict(KEN11d,terms=c("AvgTemp [n=20]"))
  AllTemp[,c(2,4,5)]<-AllTemp[,c(2,4,5)]/exp(summary(KEN11d)$tTable[,1])[["(Intercept)"]]

  ASALPrec<- ggpredict(KEN11d_ASAL,terms=c("SeasPr [n=20]"),condition=c(AvgTemp=0,CVPrec = 0, Spell = 0,Spell4=0,CVTempK =0))
  ASALPrec[,c(2,4,5)]<-ASALPrec[,c(2,4,5)]/exp(summary(KEN11d_ASAL)$tTable[,1])[["(Intercept)"]]

ASALTemp<- ggpredict(KEN11d_ASAL,terms=c("AvgTemp [n=20]"),condition=c(SeasP=0,CVPrec = 0, Spell = 0,Spell4=0,CVTempK =0) )
  ASALTemp[,c(2,4,5)]<-ASALTemp[,c(2,4,5)]/exp(summary(KEN11d_ASAL)$tTable[,1])[["(Intercept)"]]
nonASALPrec<- ggpredict(KEN11d_nonASAL,terms=c("SeasPr [n=20]"),condition=c(AvgTemp=0,CVPrec = 0, Spell = 0,Spell4=0,CVTempK =0))
  nonASALPrec[,c(2,4,5)]<-nonASALPrec[,c(2,4,5)]/exp(summary(KEN11d_nonASAL)$tTable[,1])[["(Intercept)"]]
nonASALTemp<- ggpredict(KEN11d_nonASAL,terms=c("AvgTemp [n=20]"),condition=c(SeasPr=0,CVPrec = 0, Spell = 0,Spell4=0,CVTempK =0))
  nonASALTemp[,c(2,4,5)]<-nonASALTemp[,c(2,4,5)]/exp(summary(KEN11d_nonASAL)$tTable[,1])[["(Intercept)"]]
#----------------------------------------------------------------------------------------
plot1<-plot(AllPrec)+ylab("Yield")+xlab(NULL)+xlab("(a)")+ggtitle("Seasonal precipitation")+scale_x_continuous(breaks = c(-2,-1,0,1,2,3,4,5))
plot2<-plot(AllTemp)+ylab(NULL)+xlab(NULL)+xlab("(b)")+ggtitle("Average temperature")
plot3<-plot(ASALPrec)+ylab("Yield")+xlab(NULL)+xlab("(c)")+scale_x_continuous(breaks = c(-2,-1,0,1,2,3,4,5))+ggtitle(NULL)
plot4<-plot(ASALTemp)+ylab(NULL)+xlab(NULL)+xlab("(d)")+ggtitle(NULL)
plot5<-plot(nonASALPrec)+ylab("Yield")+xlab(NULL)+xlab("(e)")+ggtitle(NULL)
plot6<-plot(nonASALTemp)+ylab(NULL)+xlab(NULL)+xlab("(f)")+ggtitle(NULL)+scale_x_continuous(breaks = c(-2,-1.5,-1,-0.5,0,0.5))

require(gridExtra)
pdf("writing/draft3/Figure1a_1f.pdf")
grid.arrange(plot1, plot2, plot3, plot4,plot5, plot6,ncol=2)
dev.off()

require(gridExtra)
cairo_ps("writing/draft3/Figure1a_1f.eps")
grid.arrange(plot1, plot2, plot3, plot4,plot5, plot6,ncol=2)
dev.off()
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# other variables

AllPrecCV<- ggpredict(KEN11d,terms=c("CVPrec [n=20]"))
AllPrecCV[,c(2,4,5)]<-AllPrecCV[,c(2,4,5)]/exp(summary(KEN11d)$tTable[,1])[["(Intercept)"]]
AllTempCV<- ggpredict(KEN11d,terms=c("CVTempK [n=20]"))
AllTempCV[,c(2,4,5)]<-AllTempCV[,c(2,4,5)]/exp(summary(KEN11d)$tTable[,1])[["(Intercept)"]]
ASALPrecCV<- ggpredict(KEN11d_ASAL,terms=c("CVPrec [n=20]"),condition=c(AvgTemp=0,SeasPr = 0, Spell = 0,Spell4=0,CVTempK =0))
ASALPrecCV[,c(2,4,5)]<-ASALPrecCV[,c(2,4,5)]/exp(summary(KEN11d_ASAL)$tTable[,1])[["(Intercept)"]]
ASALTempCV<- ggpredict(KEN11d_ASAL,terms=c("CVTempK [n=20]"),condition=c(AvgTemp=0,CVPrec = 0, Spell = 0,Spell4=0,SeasPr =0) )
ASALTempCV[,c(2,4,5)]<-ASALTempCV[,c(2,4,5)]/exp(summary(KEN11d_ASAL)$tTable[,1])[["(Intercept)"]]
nonASALPrecCV<- ggpredict(KEN11d_nonASAL,terms=c("CVPrec [n=20]"),condition=c(AvgTemp=0,SeasPr = 0, Spell = 0,Spell4=0,CVTempK =0))
nonASALPrecCV[,c(2,4,5)]<-nonASALPrecCV[,c(2,4,5)]/exp(summary(KEN11d_nonASAL)$tTable[,1])[["(Intercept)"]]
nonASALTempCV<- ggpredict(KEN11d_nonASAL,terms=c("CVTempK [n=20]"),condition=c(SeasPr=0,CVPrec = 0, Spell = 0,Spell4=0,AvgTemp =0))
nonASALTempCV[,c(2,4,5)]<-nonASALTempCV[,c(2,4,5)]/exp(summary(KEN11d_nonASAL)$tTable[,1])[["(Intercept)"]]
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plot1<-plot(AllPrecCV)+ylab("All")+xlab(NULL)+xlab("(a)")+ggtitle("Effects of precipitation variability on yields")+scale_x_continuous(breaks = c(-2,-1,0,1,2,3,4,5))+theme(plot.title=element_text(size=11))
plot2<-plot(AllTempCV)+ylab(NULL)+xlab(NULL)+xlab("(b)")+ggtitle("Effects of temperature variability on yields")+theme(plot.title=element_text(size=11))
plot3<-plot(ASALPrecCV)+ylab("ASAL")+xlab(NULL)+xlab("(c)")+scale_x_continuous(breaks = c(-2,-1,0,1,2,3,4,5))+ggtitle(NULL)
plot4<-plot(ASALTempCV)+ylab(NULL)+xlab(NULL)+xlab("(d)")+ggtitle(NULL)
plot5<-plot(nonASALPrecCV)+ylab("non-ASAL")+xlab(NULL)+xlab("(e)")+ggtitle(NULL)
plot6<-plot(nonASALTempCV)+ylab(NULL)+xlab(NULL)+xlab("(f)")+ggtitle(NULL)

require(gridExtra)
pdf("writing/draft3/Figure2a_2f.pdf")
grid.arrange(plot1, plot2, plot3, plot4,plot5, plot6,ncol=2)
dev.off()
require(gridExtra)
cairo_ps("writing/draft3/Figure2a_2f.eps")
grid.arrange(plot1, plot2, plot3, plot4,plot5, plot6,ncol=2)
dev.off()

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# other variables

AllSpell<- ggpredict(KEN11d,terms=c("Spell [n=20]"))
AllSpell[,c(2,4,5)]<-AllSpell[,c(2,4,5)]/exp(summary(KEN11d)$tTable[,1])[["(Intercept)"]]
AllSpell4<- ggpredict(KEN11d,terms=c("Spell4 [n=20]"))
AllSpell4[,c(2,4,5)]<-AllSpell4[,c(2,4,5)]/exp(summary(KEN11d)$tTable[,1])[["(Intercept)"]]
ASALSpell<- ggpredict(KEN11d_ASAL,terms=c("Spell [n=20]"),condition=c(AvgTemp=0,SeasPr = 0, CVPrec = 0,CVTempK=0,Spell4 =0))
ASALSpell[,c(2,4,5)]<-ASALSpell[,c(2,4,5)]/exp(summary(KEN11d_ASAL)$tTable[,1])[["(Intercept)"]]
ASALSpell4<- ggpredict(KEN11d_ASAL,terms=c("Spell4 [n=20]"),condition=c(AvgTemp=0,SeasPr = 0, CVPrec = 0,CVTempK=0,Spell =0))
ASALSpell4[,c(2,4,5)]<-ASALSpell4[,c(2,4,5)]/exp(summary(KEN11d_ASAL)$tTable[,1])[["(Intercept)"]]
nonASALSpell<- ggpredict(KEN11d_nonASAL,terms=c("Spell [n=20]"),condition=c(AvgTemp=0,SeasPr = 0, Spell4 = 0, CVPrec = 0,CVTempK=0))
nonASALSpell[,c(2,4,5)]<-nonASALSpell[,c(2,4,5)]/exp(summary(KEN11d_nonASAL)$tTable[,1])[["(Intercept)"]]
nonASALSpell4<- ggpredict(KEN11d_nonASAL,terms=c("Spell4 [n=20]"),condition=c(SeasPr=0,Spell = 0,  CVPrec = 0,CVTempK=0,AvgTemp =0))
nonASALSpell4[,c(2,4,5)]<-nonASALSpell4[,c(2,4,5)]/exp(summary(KEN11d_nonASAL)$tTable[,1])[["(Intercept)"]]
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plot1<-plot(AllSpell)+ylab("All")+xlab(NULL)+xlab("(a)")+ggtitle("Effects of length of max dry spell on yields")+theme(plot.title=element_text(size=11))+scale_x_continuous(breaks = c(-2,0,2,4,6,8))+scale_y_continuous(breaks = c(0.4,0.6,0.8,1,1.2))
plot2<-plot(AllSpell4)+ylab(NULL)+xlab(NULL)+xlab("(b)")+ggtitle("Effects of number of dry spells on yields")+theme(plot.title=element_text(size=11))+scale_x_continuous(breaks = c(-3,-2,-1,0,1,2))
plot3<-plot(ASALSpell)+ylab("ASAL")+xlab(NULL)+xlab("(c)")+scale_x_continuous(breaks = c(-2,-1,0,1,2,3,4,5))+ggtitle(NULL)+ggtitle(NULL)+scale_y_continuous(breaks = c(0.5,1,1.5))
plot4<-plot(ASALSpell4)+ylab(NULL)+xlab(NULL)+xlab("(d)")
plot5<-plot(nonASALSpell)+ylab("non-ASAL")+xlab(NULL)+xlab("(e)")+ggtitle(NULL)+scale_x_continuous(breaks = c(-2,0,2,4,6,8))
plot6<-plot(nonASALSpell4)+ylab(NULL)+xlab(NULL)+xlab("(f)")+ggtitle(NULL)+scale_x_continuous(breaks = c(-3,-2,-1,0,1,2))

require(gridExtra)
pdf("writing/draft3/Figure3a_3f.pdf")
grid.arrange(plot1, plot2, plot3, plot4,plot5, plot6,ncol=2)
dev.off()
require(gridExtra)
cairo_ps("writing/draft3/Figure3a_3f.eps")
grid.arrange(plot1, plot2, plot3, plot4,plot5, plot6,ncol=2)
dev.off()

# COOL >> NEW PLOTS CHECKED

