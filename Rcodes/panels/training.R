rm(list=ls())

library(foreign)
Panel <-read.dta("http://dss.princeton.edu/training/Panel101.dta")
library(car)
scatterplot(y~year|country, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=Panel)

fixed.dum <-lm(y ~ x1 + factor(country) -1, data=Panel)
summary(fixed.dum)

fixed.dum <-lm(y ~ x1 + factor(country), data=Panel)
summary(fixed.dum)

aa<-plm(y ~ x1 + factor(country),index=c("country", "year"), model="within", data=Panel)


coplot(y ~ year|country, type="l", data=Panel) # Lines
coplot(y ~ year|country, type="b", data=Panel)

coplot(Yield~Year|ID, type="l",data=CrMaize8)
 library(apsrtable)
apsrtable(ols,fixed.dum, model.names= c("OLS", "OLS_DUM"))