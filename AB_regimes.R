library(readr)
library(dplyr)
library(IncDTW)
library(ggplot2)
# Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)
path="C:/Users/ShrirangP/Desktop/normalized_1951-2007_July-August.csv"
values <- read_csv(path, col_names = FALSE, trim_ws = FALSE)
# View(values)
count = values[1,] %>% as.numeric() 

localMaxima <- function(x) {
  # Use -Inf instead if x is numeric (non-integer)
  y <- diff(c(-.Machine$integer.max, x)) > 0L
  rle(y)$lengths
  y <- cumsum(rle(y)$lengths)
  y <- y[seq.int(1L, length(y), 2L)]
  if (x[[1]] == x[[2]]) {
    y <- y[-1]
  }
  y
}


localMinima <- function(x) {
  # Use -Inf instead if x is numeric (non-integer)
  y <- diff(c(-.Machine$integer.max, x)) < 0L
  rle(y)$lengths
  y <- cumsum(rle(y)$lengths)
  y <- y[seq.int(1L, length(y), 2L)]
  if (x[[1]] == x[[2]]) {
    y <- y[-1]
  }
  y
}

#Uses functions defined above
maxima = count[localMaxima(count)]
Gt1 = which(count %in% maxima[which(maxima>1)])
minima = count[localMinima(count)]
Lt1 = which(count %in% minima[which(minima*(-1)>1)])

#Uses IncDTW library
max = count[find_peaks(count, 2, get_min = F, strict = F)]
Gt1_ = which(count %in% max[which(max>1)])
min = count[find_peaks(count, 2, get_min = T, strict = F)]
Lt1_ = which(count %in% min[which(min*(-1)>1)])




#--------------------------------------------

plot(count,type='l')
abline(h=c(-1,1),col='red')
# points(Gt1,count[Gt1],col='blue')
# points(Lt1,count[Lt1],col='green')
points(Gt1_,count[Gt1_],col='blue')
points(Lt1_,count[Lt1_],col='green')

#-------------------------------------------

setwd('C:\\Users\\ShrirangP\\Documents\\GitHub\\ISPS2024\\photos')
days = 62
years = length(count)/days;years
for(i in 1:years){
  j=i-1
  x=c((62*j+1):(62*i))
  y=count[x]
  jpeg(file=paste0((1950+i),'.jpeg'),width=1550,height=700,unit='px')
  plot(x,y,type='l')
  abline(h=c(-1,1),col='red')
  text(x,y,round(y,2),cex=1,)
  dev.off()
}

#-------------------------------------------



breaks <- count
breaks[breaks<= (-1)] <- (-1)
breaks[breaks>(-1)] <- 0
breaks

break_lengths=with(rle(breaks), lengths[values == -1])
table <- table(break_lengths)
print(table)
barplot(table,xlab='Break lengths',ylab='Frequency')



hist(break_lengths,freq=F,ylim=c(0,0.5),main='Histogram of Break lengths')
lines(density(break_lengths))

















?text()







n=length(count)
dat <- data.frame(
  x = 1:n,
  y = count
)

approxData <- data.frame(
  with(dat, 
       approx(x, y, xout = seq(1, n, by = 10), method = "linear")
  ),
  method = "approx()"
)

splineData <- data.frame(
  with(dat, 
       spline(x, y, xout = seq(1, n, by = 10))
  ),
  method = "spline()"
)

smoothData <- data.frame(
  x = 1:n,
  y = as.vector(smooth(dat$y)),
  method = "smooth()"
)

loessData <- data.frame(
  x = 1:n,
  y = predict(loess(y~x, dat, span = 0.1)),
  method = "loess()"
)


ggplot(rbind(approxData, splineData, smoothData, loessData), aes(x, y)) + 
  geom_point(dat = dat, aes(x, y), alpha = 0.2, col = "red") +
  geom_line(col = "blue") +
  facet_wrap(~method) +
  ggtitle("Interpolation and smoothing functions in R") +
  theme_bw(16)

