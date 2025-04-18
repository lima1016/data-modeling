library(forecast)
set.seed(123)
activity_data <- ts(c(50, 60, 80, 70, 90), frequency = 24)
# Fit ARIMA model
model <- auto.arima(activity_data)
forecast_data <- forecast(model, h = 5)
# Visualization
plot(forecast_data, main = "Hourly Show Attendance Forecast",
     xlab = "Hour", ylab = "Attendance")
