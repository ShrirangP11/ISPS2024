library(pacman)
pacman::p_load(pacman, rio)
colaba <- import("C:\\Users\\admin\\Desktop\\colaba.csv")
pune <- import("C:\\Users\\admin\\Documents\\GitHub\\ISPS2024\\daily_rainfall_pune.csv")
date_series1 <- seq(as.Date("1951-01-01"), as.Date("2023-12-31"), by = "day")
date_series2 <- seq(as.Date("1954-01-01"), as.Date("2023-12-31"), by = "day")

city_data <- list(colaba = colaba, pune = pune)

decades = c("1954-1963", "1964-1973", "1974-1983", 
            "1984-1993", "1994-2004", "2004-2013", 
            "2014-2023")
exceedance_df <- data.frame(Decade = decades)

for (city_name in names(city_data)) {
  print(city_name)
  city <- city_data[[city_name]]
  
  values1 <-unlist(city, use.names=F)
  values2 <- values1[1097:length(values1)]
  
  rf_series2 <- data.frame(Date = date_series2, Rainfall = values2)
  
  rf_series2$Decade <- cut(as.numeric(format(rf_series2$Date, "%Y")), 
                           breaks = seq(1954, 2024, by = 10), 
                           labels = c("1954-1963", "1964-1973", "1974-1983", 
                                      "1984-1993", "1994-2004", "2004-2013", 
                                       "2014-2023"), 
                           right = FALSE)
  
  
  exceedances2 <- rf_series2[rf_series2$Rainfall >= 204.5, ]
  
  exceedance_counts <- tapply(exceedances2$Rainfall, exceedances2$Decade, length)
  
  #exceedance_counts <- as.integer(sapply(decades, function(d) exceedance_counts[d]))
  
  exceedance_df[[city_name]] <- exceedance_counts
  
}

exceedance_df


#This is for push
#pune_values <-unlist(pune, use.names=F)
#plot(pune_values, type = "l", xlab = "Year", ylab = "Rainfall (mm)")
#axis(1, at = seq(as.Date("1950-01-01"), as.Date("2023-12-31"), by = "10 years"), labels = format(seq(as.Date("1950-01-01"), as.Date("2023-12-31"), by = "10 years"), "%Y"))
#abline(h = 204.5, col = "red", lwd = 2)


