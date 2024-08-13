library(pacman)
pacman::p_load(pacman, rio)

lat_long_cities <- import("C:\\Users\\admin\\Documents\\GitHub\\ISPS2024\\city_lat_long.xlsx")
names <- c(lat_long_cities$City)




date_series1 <- seq(as.Date("1951-01-01"), as.Date("2023-12-31"), by = "day")
date_series2 <- seq(as.Date("1954-01-01"), as.Date("2023-12-31"), by = "day")

decades = c("1954-1963", "1964-1973", "1974-1983", 
            "1984-1993", "1994-2004", "2004-2013", 
            "2014-2023")
exceedance_df <- data.frame(Decade = decades)


for (name in names){
  city <- read.csv(paste0("C:\\Users\\admin\\Documents\\GitHub\\ISPS2024\\city\\",name,".csv"), header=F, sep=",")
  
  
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
  
  exceedance_df[[name]] <- exceedance_counts
  
  
  }

exceedance_df



#pune_values <-unlist(pune, use.names=F)
#plot(pune_values, type = "l", xlab = "Year", ylab = "Rainfall (mm)")
#axis(1, at = seq(as.Date("1950-01-01"), as.Date("2023-12-31"), by = "10 years"), labels = format(seq(as.Date("1950-01-01"), as.Date("2023-12-31"), by = "10 years"), "%Y"))
#abline(h = 204.5, col = "red", lwd = 2)


