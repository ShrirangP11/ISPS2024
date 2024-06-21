library(readr)
library(dplyr)
library(IncDTW)
# Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)
path="C:/Users/ShrirangP/Desktop/values.csv"
values <- read_csv(path, col_names = FALSE, trim_ws = FALSE)
View(values)
count = values[1,] %>% as.numeric() %>% scale()

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


plot(count,ylim=c(-3,5),type='l')
abline(h=c(-1,1),col='red')
points(Gt1,count[Gt1],col='blue')
points(Lt1,count[Lt1],col='green')
points(Gt1_,count[Gt1_],col='blue')
points(Lt1_,count[Lt1_],col='green')





