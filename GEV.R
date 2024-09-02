library(ismev)
library(evd)
library(extRemes)
library(ggplot2)
library(ggpubr)
library(tseries)
# install.packages("urca")
library(urca)
# install.packages('trend')
library(trend)
setwd('C:\\Users\\ShrirangP\\Documents\\GitHub\\ISPS2024\\city')
data<-read.csv('Pune.csv',header=FALSE)
values<-unlist(data[1,], use.names=F)
x<-ts(values,freq=365)
year<-c(1:73)
annual_maximum <- as.numeric(aggregate(x, FUN=max))
plot(values,type='l')
abline(h=115.6)
#Fitting various Stationary and Non-Stationary GEV models
time <- matrix(ncol=2,nrow=73)
time[,1] <- seq(1,73,1)
time[,2] <- seq(1,73,1)^2



# GEV<- gev.fit(annual_maximum,ydat=NULL)
# nllh00 <- GEV00$nllh
# 
# GEV10 <- gev.fit(annual_maximum,ydat=time,mul=1)
# nllh10 <- GEV10$nllh
# 
# GEV01 <- gev.fit(annual_maximum,ydat=time,sigl=1,siglink=exp)
# nllh01 <- GEV01$nllh
# 
# GEV11 <- gev.fit(annual_maximum,ydat=time,mul=1,mulink=identity,sigl=1,siglink=exp)
# nllh11 <- GEV11$nllh
# 
# GEV20 <- gev.fit(annual_maximum,ydat=time,mul=c(1,2),mulink=identity)
# nllh20 <- GEV20$nllh
# 
# GEV02 <- gev.fit(annual_maximum,ydat=time,sigl=c(1,2),siglink=exp)
# nllh02 <- GEV02$nllh
# 
# GEV12 <- gev.fit(annual_maximum,ydat=time,mul=1,mulink=identity,sigl=c(1,2),siglink=exp)
# nllh12 <- GEV12$nllh
# 
# GEV21 <- gev.fit(annual_maximum,ydat=time,mul=c(1,2),mulink=identity,sigl=1,siglink=exp)
# nllh21 <- GEV21$nllh
# 
# GEV22 <- gev.fit(annual_maximum,ydat=time,mul=c(1,2),mulink=identity,sigl=c(1,2),siglink=exp)
# nllh22 <- GEV22$nllh


#ADF (augmented Dickey–Fuller for stationary) for Annual Maxima
adf.test(annual_maximum)
# Dickey-Fuller = -4.5165, Lag order = 4, p-value = 0.01
# alternative hypothesis: stationary
# We reject Null hypothesis -> Stationary series

#KPSS (Kwiatkowski–Phillips–Schmidt–Shin for stationary) for Annual Maxima
summary(ur.kpss(annual_maximum))
kpss.test(annual_maximum,null='Trend')
#KPSS Level = 0.38158, Truncation lag parameter = 3, p-value = 0.08509
#Conforming with the results of ADF test.
#Annual maxima series is trend stationary

# #The Pettitt's test is a nonparametric test that requires no assumption about the distribution of data. 
# The Pettitt's test is an adaptation of the tank-based Mann-Whitney test that allows identifying the time at which the shift occurs. 
# In his article of 1979 Pettitt describes the null hypothesis as being that the T variables follow the same distribution F, 
# and the alternative hypothesis as being that at a time t there is a change of distribution. 
# Nevertheless, the Pettitt test does not detect a change in distribution if there is no change of location. 
# For example, if before the time t, the variables follow a normal N(0,1) distribution and from time t a N (0,3) distribution, 
# the Pettitt test will not detect a change in the same way a Mann-Whitney would not detect a change of position in such a case.
pettitt.test(annual_maximum)
# U* = 362, p-value = 0.2723
# alternative hypothesis: two.sided
#Indicating no probable change in location parameter of distribution(GEV).



GEV00 <- fevd(annual_maximum, type=c('GEV'), units='mm',use.phi=F)
GEV00
plot(GEV00)

GEV10 <- fevd(annual_maximum, location.fun=~time[,1], type=c('GEV'), units='mm',use.phi=F)
GEV10
plot(GEV10)
plot(GEV10,type=c('density'), rperiods = c(5, 10, 20,50),period='year',ylim=c(0,0.5),main='Density Plot')
plot(GEV10,type=c('rl'), rperiods = c(5, 10, 20,50),period='year', main='Return Level')

GEV01 <- fevd(annual_maximum, scale.fun=~time[,1], type=c('GEV'), units='mm',use.phi=TRUE)
GEV01
plot(GEV01,type=c('density'), rperiods = c(5, 10, 20,50),period='year',ylim=c(0,0.5),main='Density Plot')
plot(GEV01,type=c('rl'), rperiods = c(5, 10, 20,50),period='year',main='Return Level')

GEV11 <- fevd(annual_maximum, scale.fun=~time[,1], location.fun=~time[,1], type=c('GEV'), units='mm',use.phi=TRUE)
GEV11
plot(GEV11,type=c('density'), rperiods = c(5, 10, 20,50),period='year',ylim=c(0,0.5),main='Density Plot')
plot(GEV11,type=c('rl'), rperiods = c(5, 10, 20,50),period='year',main='Return Level')

GEV20 <- fevd(annual_maximum,location.fun=~time[,1] + time[,2], type=c('GEV'), units='mm')
GEV20
plot(GEV20,type=c('density'), rperiods = c(5, 10, 20,50),period='year',ylim=c(0,0.5),main='Density Plot')
plot(GEV20,type=c('rl'), rperiods = c(5, 10, 20,50),period='year',main='Return Level')

GEV02 <- fevd(annual_maximum,scale.fun=~time[,1] + time[,2], type=c('GEV'), units='mm',use.phi=TRUE)
GEV02


GEV12 <- fevd(annual_maximum,location.fun=~time[,1],scale.fun=~time[,1] + time[,2], type=c('GEV'), units='mm',use.phi=TRUE)
GEV12

GEV21 <- fevd(annual_maximum,location.fun=~time[,1] + time[,2],scale.fun=~time[,1], type=c('GEV'), units='mm',use.phi=TRUE)
GEV21

GEV22 <- fevd(annual_maximum,location.fun=~time[,1] + time[,2],scale.fun=~time[,1] + time[,2], type=c('GEV'), units='mm',use.phi=TRUE)
GEV22
