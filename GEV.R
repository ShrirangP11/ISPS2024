library(ismev)
library(evd)
library(extRemes)
library(ggplot2)
library(ggpubr)
setwd('C:\\Users\\ShrirangP\\Documents\\GitHub\\ISPS2024')
data<-read.csv('daily_rainfall_pune.csv',header=FALSE)
values<-unlist(data[1,], use.names=F)
x<-ts(values,freq=365)
year<-c(1:73)
annual_maximum <- as.numeric(aggregate(x, FUN=max))

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




GEV00 <- fevd(annual_maximum, type=c('GEV'), units='mm')
plot(GEV00)

GEV10 <- fevd(annual_maximum, location.fun=~time[,1], type=c('GEV'), units='mm')
plot(GEV10,type=c('density'), rperiods = c(5, 10, 20,50),period='year',ylim=c(0,0.5),main='Density Plot')
plot(GEV10,type=c('rl'), rperiods = c(5, 10, 20,50),period='year', main='Return Level')

GEV01 <- fevd(annual_maximum, scale.fun=~time[,1], type=c('GEV'), units='mm',use.phi=TRUE)
plot(GEV01,type=c('density'), rperiods = c(5, 10, 20,50),period='year',ylim=c(0,0.5),main='Density Plot')
plot(GEV01,type=c('rl'), rperiods = c(5, 10, 20,50),period='year',main='Return Level')

GEV11 <- fevd(annual_maximum, scale.fun=~time[,1], location.fun=~time[,1], type=c('GEV'), units='mm',use.phi=TRUE)
plot(GEV11,type=c('density'), rperiods = c(5, 10, 20,50),period='year',ylim=c(0,0.5),main='Density Plot')
plot(GEV11,type=c('rl'), rperiods = c(5, 10, 20,50),period='year',main='Return Level')

GEV20 <- fevd(annual_maximum,location.fun=~time[,1] + time[,2], type=c('GEV'), units='mm')
plot(GEV20,type=c('density'), rperiods = c(5, 10, 20,50),period='year',ylim=c(0,0.5),main='Density Plot')
plot(GEV20,type=c('rl'), rperiods = c(5, 10, 20,50),period='year',main='Return Level')

GEV02 <- fevd(annual_maximum,scale.fun=~time[,1] + time[,2], type=c('GEV'), units='mm',use.phi=TRUE)

GEV12 <- fevd(annual_maximum,location.fun=~time[,1],scale.fun=~time[,1] + time[,2], type=c('GEV'), units='mm',use.phi=TRUE)

GEV21 <- fevd(annual_maximum,location.fun=~time[,1] + time[,2],scale.fun=~time[,1], type=c('GEV'), units='mm',use.phi=TRUE)

GEV22 <- fevd(annual_maximum,location.fun=~time[,1] + time[,2],scale.fun=~time[,1] + time[,2], type=c('GEV'), units='mm',use.phi=TRUE)
