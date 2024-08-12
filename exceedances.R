require(forecast)
library(tscount)
library(ggplot2)
library(parameters)
library(tidyverse)
# library(nlme)
# library(scales)
# library(ggforce)
# library(kableExtra)

setwd('C:\\Users\\admin\\Documents\\GitHub\\ISPS2024')
data <- read.csv("daily_rainfall_pune.csv",header = FALSE)
date_series <- seq(as.Date("1951-01-01"), as.Date("2023-12-31"), by = "day")
daily <- data.frame(Date = date_series, Value = t(data))
View(daily)
exc <- c()
for(i in 1:73){
  year <- i + 1950
  count <- daily %>% filter(between(Date, as.Date(paste0(year,'-01-01')), as.Date(paste0(year,'-12-31')))) %>% filter(Value>204.5) %>% count()
  exc[i] <- count[1,1]
}
par(mfrow=c(1,1))
plot(exc,type='l')


# par(mfrow=c(1,2))
# resultACF<-acf(exc,xlim=c(0,15),ylim=c(-.4,1),main="ACF Plot ts1")
# resultPACF<-pacf(exc,xlim=c(1,14),ylim=c(-.4,1),main="PACF Plot ts1")
# 
# arima.model1<-auto.arima(exc)
# arima.model1.tab<-arima.model1%>%parameters()%>%mutate_if(is.numeric,round,2)
# arima.model1.tab
# 
# decompose(ts(exc,freq=1))
