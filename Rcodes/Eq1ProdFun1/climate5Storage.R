rm(list=ls())

WDuni<-c("/home/m/mn/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)

#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
load("~/foodSystems/dataFS/Main/CrMaize5.RData")
load("~/foodSystems/dataFS/Main/climate4.RData")
#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
#wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
# I am going to create some better way how to store all the climate variables - lists I think

Prec<-list()
PrecZ<-list()
Tmx<-list()
TmxZ<-list()
SPEI3<-list()
SPEI10<-list()
SPEI12<-list()
SPEI18<-list()


for(i in 1:65)
  {Prec[[i]]<-list(y1999=numeric(12),y2000=numeric(12),y2001=numeric(12),y2002=numeric(12),y2003=numeric(12),y2004=numeric(12),
                  y2005=numeric(12),y2006=numeric(12),y2007=numeric(12),y2008=numeric(12),y2009=numeric(12),y2010=numeric(12),
                  y2011=numeric(12),y2012=numeric(12),y2013=numeric(12),y2014=numeric(12),y2015=numeric(12))

  PrecZ[[i]]<-list(y1999=numeric(12),y2000=numeric(12),y2001=numeric(12),y2002=numeric(12),y2003=numeric(12),y2004=numeric(12),
                  y2005=numeric(12),y2006=numeric(12),y2007=numeric(12),y2008=numeric(12),y2009=numeric(12),y2010=numeric(12),
                  y2011=numeric(12),y2012=numeric(12),y2013=numeric(12),y2014=numeric(12),y2015=numeric(12))

  Tmx[[i]]<-list(y1999=numeric(12),y2000=numeric(12),y2001=numeric(12),y2002=numeric(12),y2003=numeric(12),y2004=numeric(12),
                    y2005=numeric(12),y2006=numeric(12),y2007=numeric(12),y2008=numeric(12),y2009=numeric(12),y2010=numeric(12),
                    y2011=numeric(12),y2012=numeric(12),y2013=numeric(12),y2014=numeric(12),y2015=numeric(12))

  TmxZ[[i]]<-list(y1999=numeric(12),y2000=numeric(12),y2001=numeric(12),y2002=numeric(12),y2003=numeric(12),y2004=numeric(12),
                    y2005=numeric(12),y2006=numeric(12),y2007=numeric(12),y2008=numeric(12),y2009=numeric(12),y2010=numeric(12),
                    y2011=numeric(12),y2012=numeric(12),y2013=numeric(12),y2014=numeric(12),y2015=numeric(12))
  
 SPEI3[[i]]<-list(y1999=numeric(12),y2000=numeric(12),y2001=numeric(12),y2002=numeric(12),y2003=numeric(12),y2004=numeric(12),
                    y2005=numeric(12),y2006=numeric(12),y2007=numeric(12),y2008=numeric(12),y2009=numeric(12),y2010=numeric(12),
                    y2011=numeric(12),y2012=numeric(12),y2013=numeric(12),y2014=numeric(12),y2015=numeric(12))
  
 SPEI10[[i]]<-list(y1999=numeric(12),y2000=numeric(12),y2001=numeric(12),y2002=numeric(12),y2003=numeric(12),y2004=numeric(12),
                  y2005=numeric(12),y2006=numeric(12),y2007=numeric(12),y2008=numeric(12),y2009=numeric(12),y2010=numeric(12),
                  y2011=numeric(12),y2012=numeric(12),y2013=numeric(12),y2014=numeric(12),y2015=numeric(12))
 
 SPEI12[[i]]<-list(y1999=numeric(12),y2000=numeric(12),y2001=numeric(12),y2002=numeric(12),y2003=numeric(12),y2004=numeric(12),
                  y2005=numeric(12),y2006=numeric(12),y2007=numeric(12),y2008=numeric(12),y2009=numeric(12),y2010=numeric(12),
                  y2011=numeric(12),y2012=numeric(12),y2013=numeric(12),y2014=numeric(12),y2015=numeric(12))
 
 SPEI18[[i]]<-list(y1999=numeric(12),y2000=numeric(12),y2001=numeric(12),y2002=numeric(12),y2003=numeric(12),y2004=numeric(12),
                  y2005=numeric(12),y2006=numeric(12),y2007=numeric(12),y2008=numeric(12),y2009=numeric(12),y2010=numeric(12),
                  y2011=numeric(12),y2012=numeric(12),y2013=numeric(12),y2014=numeric(12),y2015=numeric(12))

}
 
 

for(i in 1:65)
{
  k<-unique(climate4$ID1)[i]
  Prec[[i]]<- lapply(seq(1:17), function(j) {climate4$Prec[which(climate4$Year== (1998+j) & climate4$ID1== k)]})
  PrecZ[[i]]<- lapply(seq(1:17), function(j) {climate4$PrecZ[which(climate4$Year== (1998+j) & climate4$ID1== k)]})
  Tmx[[i]]<- lapply(seq(1:17), function(j) {climate4$Tmx[which(climate4$Year== (1998+j) & climate4$ID1== k)]})
  TmxZ[[i]]<- lapply(seq(1:17), function(j) {climate4$TmxZ[which(climate4$Year== (1998+j) & climate4$ID1== k)]})
  SPEI3[[i]]<- lapply(seq(1:17), function(j) {climate4$SPEI3[which(climate4$Year== (1998+j) & climate4$ID1== k)]} )
SPEI10[[i]]<- lapply(seq(1:17), function(j) {climate4$SPEI10[which(climate4$Year== (1998+j) & climate4$ID1== k)]}) 
SPEI12[[i]]<- lapply(seq(1:17), function(j) {climate4$SPEI12[which(climate4$Year== (1998+j) & climate4$ID1== k)]})  
SPEI18[[i]]<- lapply(seq(1:17), function(j) {climate4$SPEI18[which(climate4$Year== (1998+j) & climate4$ID1== k)]})

}

rm(climate5)
climate5<-list(Prec=Prec,PrecZ=PrecZ,Tmx=Tmx,TmxZ=TmxZ,SPEI3=SPEI3,SPEI10=SPEI10,SPEI12=SPEI12,SPEI18=SPEI18)

for (i in 1:8)
{climate5[[i]]<-lapply(climate5[[i]], function(x) {names(x)<-as.character(seq(1999:2015)+1998); return(x)} )
names(climate5[[i]]) <- unique(climate4$ID1)}

#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

# save game

rm(list=setdiff(ls(), "climate5"))
save.image("~/foodSystems/dataFS/Main/climate5.RData")

