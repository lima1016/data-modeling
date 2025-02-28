# Install and load required libraries
if (!require("tidyr")) install.packages("tidyr", dependencies=TRUE)
if (!require("dplyr")) install.packages("dplyr", dependencies=TRUE)
if (!require("ggplot2")) install.packages("ggplot2", dependencies=TRUE)
if (!require("forecast")) install.packages("forecast", dependencies=TRUE)

library(tidyr)
library(dplyr)
library(ggplot2)
library(forecast)

# Read the data
temps <- read.table("C:/Users/kimyo/Desktop/LIMA/OMSCS/강의/2025Spring/ISYE6501-IntroductionToAnalyticsModeling/HW/Week04/temps.txt", 
                    header=TRUE, sep="\t")

# Reshape data to long format
temps_long <- temps %>%
  gather(year, temperature, -DAY) %>%
  mutate(
    year = as.numeric(gsub("X", "", year)),  # Remove 'X' if present in year names
    month_day = as.Date(paste("2000", DAY), format="%Y %d-%b"),
    date = as.Date(paste(year, format(month_day, "%m-%d")), format="%Y %m-%d")
  )

# Function to calculate exponential smoothing
calculate_smoothing <- function(temperatures) {
  temperatures <- as.numeric(temperatures)
  if(all(is.na(temperatures))) return(rep(NA, length(temperatures)))
  
  model <- ses(temperatures, alpha=0.3, h=1)
  return(model$fitted)
}

# Apply smoothing
yearly_smoothed <- temps_long %>%
  group_by(year) %>%
  arrange(date) %>%
  mutate(smoothed_temp = calculate_smoothing(temperature)) %>%
  ungroup()

# Function to find the first 5-day period below 85°F after August 1st
find_summer_end <- function(data) {
  fall_data <- data %>% 
    filter(format(date, "%m") >= "08") %>%  # After August 1st
    filter(!is.na(smoothed_temp))
  
  if(nrow(fall_data) < 5) return(NA)
  
  for(i in 1:(nrow(fall_data)-4)) {
    if(all(fall_data$smoothed_temp[i:(i+4)] < 85)) {
      return(fall_data$date[i])
    }
  }
  return(NA)
}

# Calculate summer end dates
summer_ends <- yearly_smoothed %>%
  group_by(year) %>%
  summarise(
    end_date = find_summer_end(cur_data()),
    end_day = ifelse(!is.na(end_date), as.numeric(format(end_date, "%j")), NA)
  ) %>%
  ungroup()

# Remove NA values before analysis
summer_ends_clean <- summer_ends %>%
  filter(!is.na(end_date), !is.na(end_day))

# Check if we have valid data
if(nrow(summer_ends_clean) > 0) {
  # Create visualization with ggplot
  summer_plot <- ggplot(summer_ends_clean, aes(x=year, y=end_day)) +
    geom_point(size=3, color="darkred") +  # Data points
    geom_smooth(method="lm", se=TRUE, color="blue") +  # Regression trend
    labs(
      title="End of Summer Trend in Atlanta (1996-2015)",
      subtitle="Based on Exponential Smoothing of Daily Temperatures",
      x="Year",
      y="Day of Year",
      caption="Summer end defined as first 5-day period below 85°F after August 1st"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size=14, face="bold"),
      plot.subtitle = element_text(size=12),
      axis.title = element_text(size=11)
    )
  
  # Display the plot
  print(summer_plot)
  
  # Save the plot as an image file
  # ggsave("summer_trend.png", plot=summer_plot, width=8, height=6, dpi=300)
  
  # Perform regression analysis
  summer_trend <- lm(end_day ~ year, data=summer_ends_clean)
  summary_stats <- summary(summer_trend)
  
  # Print statistical analysis results
  print("Statistical Analysis of Summer End Date Trend:")
  print(summary_stats)
  
  # Calculate average change per decade
  slope <- coef(summer_trend)[2]
  decade_change <- slope * 10
  print(paste("Average change in summer end date per decade:", 
              round(decade_change, 1), "days"))
} else {
  print("No valid data points found after processing")
}
