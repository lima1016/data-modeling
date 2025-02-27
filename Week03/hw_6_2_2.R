temps_data <- read.table("C:/Users/HW/Week03/hw3/temps.txt", header = TRUE, sep = "\t")

print("Data structure check")
str(temps_data)

calculate_cusum <- function(x) {
  x <- x[!is.na(x)] 
  if(length(x) == 0) return(NA)
  mean_val <- mean(x)
  deviations <- x - mean_val
  cumsum(deviations)
}

results <- data.frame(
  year = 1996:2015,
  summer_end = NA,
  avg_temp = NA
)

for(i in 1:nrow(results)) {
  year <- results$year[i]
  year_col <- paste0("X", year)
  
  if(year_col %in% names(temps_data)) {
    temps <- temps_data[[year_col]]
    
    cusum <- calculate_cusum(temps)
    
    if(!all(is.na(cusum)) && length(cusum) > 0) {
      max_idx <- which.max(cusum)
      results$summer_end[i] <- as.character(temps_data$DAY[max_idx])
      results$avg_temp[i] <- mean(temps, na.rm = TRUE)
    }
  }
}

results$temp_deviation <- results$avg_temp - mean(results$avg_temp, na.rm = TRUE)
results$temp_cusum <- cumsum(results$temp_deviation)

par(mfrow = c(2,1), mar = c(4,4,3,2))

valid_data <- results[!is.na(results$avg_temp) & !is.na(results$temp_cusum), ]

if(nrow(valid_data) > 0) {
  plot(valid_data$year, 1:nrow(valid_data), 
       type = "b", 
       main = "Summer End Date Change (1996-2015)",
       xlab = "Year", 
       ylab = "End Date Order",
       pch = 16)
  grid()
}

if(nrow(valid_data) > 0) {
  plot(valid_data$year, valid_data$temp_cusum, 
       type = "b",
       main = "Temperature Change Trend (CUSUM)",
       xlab = "Year", 
       ylab = "Cumulative Temperature Deviation",
       pch = 16)
  grid()
}

early_period <- mean(results$avg_temp[1:10], na.rm = TRUE)
late_period <- mean(results$avg_temp[11:20], na.rm = TRUE)

cat("\nFirst period (1996-2005) average:", round(early_period, 1), "°F\n")
cat("\nSecond period (2006-2015) average:", round(late_period, 1), "°F\n")
cat("\nTemperature change:", round(late_period - early_period, 1), "°F\n")
