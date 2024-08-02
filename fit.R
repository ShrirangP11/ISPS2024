#Modelling non-stationarity in Block maxima
# install.packages('ismev')
library(ismev)
library(evd)
library(extRemes)
library(ggplot2)
library(ggpubr)
setwd('C:\\Users\\Admin\\Documents\\GitHub\\ISPS2024')
data<-read.csv('daily_rainfall.csv',header=FALSE)
values<-unlist(data[1,], use.names=F)
x<-ts(values,freq=365)
year<-c(1:73)
annual_maximum <- as.numeric(aggregate(x, FUN=max))
summary(lm(annual_maximum~year))
  
df<-data.frame(x=year,y=annual_maximum)
ggplot(data=df, aes(x=x, y=y)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  geom_smooth(method="lm",formula=y~x) +
  geom_line() +
  xlab('Year') +
  ylab('Annual Max Rainfall(mm)') +
  stat_regline_equation(label.x=20, label.y=600,size=6) +
  stat_cor(aes(label=..rr.label..), label.x=20, label.y=570,size=6)


#Stationary model
stat<-gev.fit(annual_maximum,ydat=NULL)
gev.diag(stat) 
#nllh = 400


#Non-Stationary model 1
ti<-matrix(ncol=2, nrow=73)
ti[,1]<-seq(1,73,1) #mu
# ti[,2]<-seq(1,73,1)^ #variance
nbm<-gev.fit(annual_maximum,ydat=ti,mul=1)
gev.diag(nbm)
#nllh = 399.5154

qchisq(0.95,1)

#POT stationary
spot<-gpd.fit(values,threshold = 205)
gpd.diag(spot)
#nllh = 2528.335

#POT non-stationary
t<-matrix(ncol=2,nrow=26663)
t[,1]<-seq(1,26663,1)
t[,2] <- seq(1,26663,1)^2
npot<-gpd.fit(values,threshold = 205,ydat=t,sigl=1,siglink=exp)
gpd.diag(npot)
#nllh = 2524.758


plot(year,annual_maximum,type='l')
abline(a=289,b=0.43,col='red')
abline(lm(annual_maximum~year),col='blue')




  
#Return level expression for GEV(nbm):
Z = function(t,r){
  return((283 + 2.87*t) + (46.031/0.114)*(((-log(1-1/r))^(-0.114))-1))
}
time=seq(73,100,1)
r=seq(20,40,10)

plot(x=seq(2024,2051,1), y=Z(time,50),type='l',ylim=c(500,800),xlab='Year',ylab='Annual Maxima(mm)')
lines(x=seq(2024,2051,1),y=Z(time,10),type='l',col='red')
lines(x=seq(2024,2051,1),y=Z(time,20),type='l',col='blue')
lines(x=seq(2024,2051,1),y=Z(time,30),type='l',col='green')
lines(x=seq(2024,2051,1),y=Z(time,50),type='l',col='brown')
for(i in r){
  lines(x=time, y=Z(time,r),col=i)
}

#Return level








f<-fevd(values,threshold=205,scale.fun=~t[,1] + t[,2],type=c('GP'),verbose=TRUE,use.phi=TRUE)
f
plot(f, 'probprob')
plot(f, 'rl',rperiods = c(2, 5, 10),period='year',a=100)


#This is for push.