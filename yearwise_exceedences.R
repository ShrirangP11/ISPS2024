library(pacman)
pacman::p_load(pacman, rio)

d = read.csv("C:/Users/Bharat Jambhulkar/ISPS2024/city/Pune.csv",header=FALSE)
td = t(d)
View(td)

lat_long_cities <- import("C:/Users/Bharat Jambhulkar/ISPS2024/city_lat_long.csv")
names <- c(lat_long_cities$City)
date_series1 <- seq(as.Date("1951-01-01"), as.Date("2023-12-31"), by = "day")

years <- seq(1954, 2023)
date_series2 <- seq(as.Date("1954-01-01"), as.Date("2023-12-31"), by = "day")

yearly_exceedance_df <- data.frame(Year = years)
dim(yearly_exceedance_df)

for (name in names){
  city <- read.csv(paste0("C:/Users/Bharat Jambhulkar/ISPS2024/city/",name,".csv"), header=F, sep=",")
  values1 <-unlist(city, use.names=F)
  values2 <- values1[1097:length(values1)]
  
  rf_series <- data.frame(Date = date_series2, Rainfall = values2)
  rf_series$Year <- as.numeric(format(rf_series$Date, "%Y"))
  
  exceedances2 <- rf_series[rf_series$Rainfall >= 115.6, ]
  all_years <- unique(rf_series$Year)
  
  exceedance_counts <- tapply(exceedances2$Rainfall, exceedances2$Year, length)
  
  exceedance_counts_full <- sapply(all_years, function(year) {
    ifelse(is.na(exceedance_counts[as.character(year)]), 0, exceedance_counts[as.character(year)])
  })
  
  yearly_exceedance_df[[name]] <- exceedance_counts_full
}

#write.csv(yearly_exceedance_df,"C:/Users/Bharat Jambhulkar/ISPS2024/yearwise_exc_115.6.csv",row.names = FALSE)



