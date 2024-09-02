# modelling for values till 2013, stationary POt
library(ismev)
library(evd)
library(extRemes)
library(ggplot2)
library(ggpubr)
library(dplyr)
setwd('C:\\Users\\admin\\Documents\\GitHub\\ISPS2024')
data = read.csv("daily_rainfall.csv",header = FALSE)

rf_series <- data.frame(Date = date_series, Value = t(data))

date_series <- seq(as.Date("1951-01-01"), as.Date("2023-12-31"), by = "day")

date_series2 <- seq(as.Date("2014-01-01"), as.Date("2015-12-31"), by = "day")

date_series5 <- seq(as.Date("2014-01-01"), as.Date("2018-12-31"), by = "day")

date_series10 <- seq(as.Date("2014-01-01"), as.Date("2023-12-31"), by = "day")

# POT Stationary Model
GP00 <- fevd(rf_series$Value[1:23011] ,threshold = 204.5, type=c('GP'), units='mm', use.phi = FALSE)
plot(GP00,type=c('rl'), rperiods = c(2,5,10),period='year',main='Return Level')

(GP00_rl = return.level(GP00, return.period = c(2,5,10)))
prb <- c(1/2, 1/5, 1/10)

#for 2 year return level
pot2 <- rf_series$Value[
  rf_series$Date %in% date_series2 &
  rf_series$Value >= 204.5]

rv2 <- pot2[pot2 >= 351.2727]

(emp2 <- length(rv2)/length(pot2))

# for 5 year return level
pot5 <- rf_series$Value[
  rf_series$Date %in% date_series5 &
    rf_series$Value >= 204.5]
pot5

rv5 <- pot5[pot5 >= 393.0517]

(emp5 <- length(rv5)/length(pot5))

# for 10 year return level
pot10 <- rf_series$Value[
  rf_series$Date %in% date_series10 &
    rf_series$Value >= 204.5]
pot10

rv10 <- pot10[pot10 >= 393.0517]

(emp10 <- length(rv10)/length(pot10))


