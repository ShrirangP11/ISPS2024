df = read.csv("C:/Users/Bharat Jambhulkar/ISPS2024/yearwise_exc_115.6.csv")


years <- as.numeric(df$Year)

decades <- list(
  "1953-1963" = 1953:1963,
  "1964-1973" = 1964:1973,
  "1974-1983" = 1974:1983,
  "1984-1993" = 1984:1993,
  "1994-2003" = 1994:2003,
  "2004-2013" = 2004:2013,
  "2014-2023" = 2014:2023
)

proportion_zeros <- list()

for (city in colnames(df[-1])) {
  city_proportions <- numeric(length(decades))
  for (i in seq_along(decades)) {
    
    decade_indices <- which(years %in% decades[[i]])
    
    
    decade_data <- df[decade_indices, city]
    
    
    city_proportions[i] <- mean(decade_data == 0)
  }
  proportion_zeros[[city]] <- city_proportions
}

proportion_zeros_df <- as.data.frame(proportion_zeros, row.names = names(decades))

View(proportion_zeros_df)
