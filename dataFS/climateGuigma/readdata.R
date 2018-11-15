rm(list=ls())

WDuni<-c("/its/home/mn301/foodSystems/dataFS") # uni
WDhome<-c("/home/trennion/foodSystems/dataFS") # doma

setwd(WDuni)
setwd(WDhome)
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

avgTemp<-read.csv2("climateGuigma/avg_Temp_Oct_Mar.csv", header = TRUE, sep = ";", quote = "\"",
          dec = ".", fill = TRUE, comment.char = "")

maxRain<-read.csv2("climateGuigma/max_rain_OND.csv", header = TRUE, sep = ";", quote = "\"",
                   dec = ".", fill = TRUE, comment.char = "")