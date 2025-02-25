# Load the dataset
data <- read.table("C:/Users/kimyo/Desktop/LIMA/OMSCS/강의/2025Spring/ISYE6501-IntroductionToAnalyticsModeling/HW/Week02/hw2/iris.txt", header = TRUE)

# Select relevant predictors
selected_data <- data[, c("Petal.Length", "Petal.Width")]

# Perform k-means clustering
set.seed(123)  # For reproducibility
kmeans_result <- kmeans(selected_data, centers = 3, nstart = 20)

# Add cluster assignments to the data
data$Cluster <- as.factor(kmeans_result$cluster)

# Evaluate clustering performance
table(data$Cluster, data$Species)

# Plot the clustering results
library(ggplot2)
ggplot(data, aes(x = Petal.Length, y = Petal.Width, color = Cluster)) +
  geom_point(size = 3) +
  ggtitle("K-means Clustering Results") +
  theme_minimal()
