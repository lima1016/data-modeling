# Sample data: age and spending
set.seed(123)
passenger_data <- data.frame(
  age = c(25, 30, 45, 50, 35, 28),
  spending = c(150, 200, 500, 600, 300, 180)
)
# K-Means clustering
clusters <- kmeans(passenger_data, centers = 3)
# Visualization
plot(passenger_data$age, passenger_data$spending, col = clusters$cluster, pch = 19,
     xlab = "Age", ylab = "Spending ($)", main = "Passenger Clusters")
points(clusters$centers, col = 1:3, pch = 8, cex = 2)
