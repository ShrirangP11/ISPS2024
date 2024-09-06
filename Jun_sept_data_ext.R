library(pacman)
pacman:: p_load(pacman, rio)

data <- import("C:\\Users\\sharv\\Documents\\GitHub\\ISPS2024\\city\\Mumbai.csv")
date_series <- seq(as.Date("1951-01-01"), as.Date("2023-12-31"), by = "day")

df <- data.frame(Rainfall = t(data), Date = date_series)
View(df)

# Extract the month from  "Date" Column
df$Month <- format(df$Date, "%m")

df_filtered <- df[df$Month %in% c("06", "07", "08", "09"), ]
colnames(df_filtered)[1] = "Rainfall"
View(df_filtered)

values <- df_filtered$Rainfall

write.csv(values, "C:\\Users\\sharv\\Documents\\GitHub\\ISPS2024\\2cities\\Mumbai\\Jun_Sept_Mum.csv", 
          row.names = FALSE)


