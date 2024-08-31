df = read.csv("C:/Users/Bharat Jambhulkar/ISPS2024/exc_115.csv")
y <- df$Mumbai
index <- 1:7  # Assuming you have 7 decades

# Custom labels for the decades
decade_labels <- c("1953-1963", "1964-1973", "1974-1983", 
                   "1984-1993", "1994-2003", "2004-2013", "2014-2023")

# Creating the plot
plot(index, y,
     col = "blue",                  # Line color
     pch = 19,                      # Point type (filled circle)
     lwd = 2,                       # Line width
     ylab = "Exceedance",           # Y-axis label
     xlab = "Decade",               # X-axis label
     main = "Mumbai",                # Main title
     cex.main = 1.5,                # Size of main title
     cex.lab = 1.2,                 # Size of axis labels
     cex.axis = 1.1,                # Size of axis text
     xaxt = "n")                    # Suppress default x-axis labels

# Adding custom x-axis labels
axis(1, at = index, labels = decade_labels, cex.axis = 1.1)

# Adding a regression line
reg <- lm(y ~ index)
abline(reg, col = "red", lwd = 2, lty = 2)  # Dashed red line for regression

# Adding confidence intervals
conf.int <- predict(reg, interval = "confidence")
lines(index, conf.int[,2], col = "green", lwd = 2, lty = 2)  # Lower CI
lines(index, conf.int[,3], col = "green", lwd = 2, lty = 2)  # Upper CI

# Adding gridlines
grid(col = "lightgray", lty = "dotted")

# Adding a legend
legend("bottomleft", legend = c("Observed", "Regression Line", "95% Confidence Interval"),
       col = c("blue", "red", "green"), lty = c(1, 2, 2), lwd = 2, pch = 19)
