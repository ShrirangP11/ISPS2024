library(ismev)
library(evd)
library(extRemes)
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
