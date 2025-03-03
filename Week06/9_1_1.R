# Load required libraries
library(ggplot2)
library(caret)

# Load dataset
data <- read.table("C:/Users/kimyo/Desktop/LIMA/OMSCS/강의/2025Spring/ISYE6501-IntroductionToAnalyticsModeling/Lecture/Week05/hw5/uscrime.txt", header=TRUE)

# Separate dependent variable (Crime) and independent variables
crime_data <- data$Crime
independent_vars <- data[, -16]  # Exclude 'Crime'

# ==================================
# Standard Regression Model (8.2)
# ==================================
model_8_2 <- lm(Crime ~ ., data = data)
summary_8_2 <- summary(model_8_2)

# ==================================
# PCA-Based Regression Model (9.1)
# ==================================

# Scale the data
scaled_data <- scale(independent_vars)

# Perform PCA
pca_result <- prcomp(scaled_data, center=TRUE, scale=TRUE)

# Select principal components explaining at least 90% variance
explained_variance <- summary(pca_result)$importance[2,]
cumulative_variance <- cumsum(explained_variance)
selected_components <- which(cumulative_variance >= 0.9)[1]

# Create dataset with selected principal components
pca_data <- as.data.frame(pca_result$x[, 1:selected_components])
pca_data$Crime <- crime_data

# Train regression model using selected principal components
model_pca <- lm(Crime ~ ., data=pca_data)
summary_pca <- summary(model_pca)

# ==================================
# Convert PCA Regression Model to Original Variables
# ==================================

# Extract PCA regression coefficients
pca_coefficients <- model_pca$coefficients[-1]  # Exclude intercept

# Convert PCA coefficients to original variable coefficients
original_coefficients <- pca_result$rotation[, 1:selected_components] %*% pca_coefficients

# Unscale the coefficients to match original scale
original_coefficients <- original_coefficients / attr(scaled_data, "scaled:scale")

# Print transformed coefficients (in terms of original variables)
cat("\n=== Transformed Regression Model (Original Variables) ===\n")
print(original_coefficients)

# ==================================
# Compare Models
# ==================================

# Compute RMSE and MAE
rmse <- function(actual, predicted) sqrt(mean((actual - predicted)^2))
mae <- function(actual, predicted) mean(abs(actual - predicted))

# Predictions for training data
pred_8_2 <- predict(model_8_2, newdata=data)
pred_pca <- predict(model_pca, newdata=pca_data)

# Performance comparison
performance_metrics <- data.frame(
  Model = c("Standard Regression (8.2)", "PCA Regression (9.1)"),
  R_squared = c(summary_8_2$r.squared, summary_pca$r.squared),
  Adjusted_R_squared = c(summary_8_2$adj.r.squared, summary_pca$adj.r.squared),
  RMSE = c(rmse(crime_data, pred_8_2), rmse(crime_data, pred_pca)),
  MAE = c(mae(crime_data, pred_8_2), mae(crime_data, pred_pca))
)

print(performance_metrics)
