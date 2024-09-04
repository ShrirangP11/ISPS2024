library(ismev)
library(evd)
library(extRemes)
library(tseries)

date_series <- seq(as.Date("1951-01-01"), as.Date("2023-12-31"), by = "day")
data = read.csv("D:/Bharat/College/SPPU Material/SPPU Stuff/ISPS Competiton/EVT/daily_rainfall_pune.csv",header = FALSE)
d = as.data.frame(t(data))
rf_series <- data.frame(
  Date = date_series,
  Value = d$V1
)

time <- matrix(ncol=2,nrow=length(rf_series$Value))
time[,1] <- seq(1,length(rf_series$Value),1)
time[,2] <- seq(1,length(rf_series$Value),1)^2

GP1 <- fevd(rf_series$Value,threshold = 204.5,scale.fun=~time[,1], type=c('GP'), units='mm',use.phi=TRUE)
plot(GP1,type=c('rl'), rperiods = c(5, 10, 20,50),period='year',main='Return Level')


GP2 <- fevd(rf_series$Value,threshold = 204.5,scale.fun=~time[,1] + time[,2], type=c('GP'), units='mm',use.phi=TRUE)
plot(GP2,type=c('rl'), rperiods = c(5, 10, 20,50),period='year',main='Return Level')










setwd('C:\\Users\\Admin\\Documents\\GitHub\\ISPS2024\\city')
data<-read.csv('Pune.csv',header=FALSE)
values<-unlist(data[1,], use.names=F)
x<-ts(values,freq=365)
year<-c(1:73)
annual_maximum <- as.numeric(aggregate(x, FUN=max))
plot(values,type='l')
abline(h=204.5)
#Fitting various Stationary and Non-Stationary GEV models
time <- matrix(ncol=2,nrow=26663)
time[,1] <- seq(1,26663,1)
time[,2] <- seq(1,26663,1)^2


length(values[values>115.6])

plot(values[values>115.6])

adf.test(values[values>115.6])
kpss.test(values[values>115.6])
pettitt.test(values[values>115.6])
plot(values,type='l')
abline(h=115.6)


df <- data.frame(values)
head(df)
GP1 <- fevd(values,threshold = 115.6,type=c('GP'), units='mm',use.phi=F)
GP1
plot(GP1, type=c('rl'),main='Return levels')
# plot(GP1,type=c('rl'), rperiods = c(5, 10, 20,50),period='year',main='Return Level')

NSGP1 <- fevd(values, threshold=115.6, scale.fun=~time[,1],use.phi=T,units='mm',time.unit='days',type=c('GP'),period.basis='year',span=73)
NSGP1

lr.test(GP1,NSGP1)
plot(NSGP1, type=c('rl'),rperiods=c(2,5,10),main='RL PUNE')

NSGP2 <- fevd(values, threshold=115.6, scale.fun=~time[,1]+time[,2],use.phi=T,units='mm',time.unit='days',type=c('GP'),period.basis='year',span=73)
NSGP2
plot(NSGP2, type=c('rl'),rperiods=c(2,5,10),main='RL2 PUNE')
