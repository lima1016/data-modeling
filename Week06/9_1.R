# Load required libraries
library(ggplot2)
library(caret)
library(dplyr)

# Load datasets for Question 8.2 and 9.1
data_8_2 <- read.table("C:/Users/kimyo/Desktop/LIMA/OMSCS/강의/2025Spring/ISYE6501-IntroductionToAnalyticsModeling/Lecture/Week05/hw5/uscrime.txt", header=TRUE)
data_9_1 <- read.table("C:/Users/kimyo/Desktop/LIMA/OMSCS/강의/2025Spring/ISYE6501-IntroductionToAnalyticsModeling/Lecture/Week06/hw6/uscrime.txt", header=TRUE)

# Separate dependent (Crime) and independent variables
crime_data_8_2 <- data_8_2$Crime
independent_vars_8_2 <- data_8_2[, -16]  # Exclude 'Crime' from predictors

crime_data_9_1 <- data_9_1$Crime
independent_vars_9_1 <- data_9_1[, -16]  # Exclude 'Crime' from predictors

# ==================================
# Train Standard Regression Model (8.2)
# ==================================
model_8_2 <- lm(Crime ~ ., data = data_8_2)
summary_8_2 <- summary(model_8_2)

# ==================================
# Train PCA-based Regression Model (9.1)
# ==================================

# Normalize the data before PCA
scaled_data_9_1 <- scale(independent_vars_9_1)

# Perform Principal Component Analysis (PCA)
pca_result_9_1 <- prcomp(scaled_data_9_1, center=TRUE, scale=TRUE)

# Determine the number of principal components that explain at least 90% of the variance
explained_variance <- summary(pca_result_9_1)$importance[2,]
cumulative_variance <- cumsum(explained_variance)
selected_components <- which(cumulative_variance >= 0.9)[1]  # Select minimum components covering 90% variance

# Create new dataset with selected principal components
pca_data_9_1 <- as.data.frame(pca_result_9_1$x[, 1:selected_components]) 
pca_data_9_1$Crime <- crime_data_9_1

# Train regression model using selected principal components
model_pca_9_1 <- lm(Crime ~ ., data=pca_data_9_1)
summary_pca_9_1 <- summary(model_pca_9_1)

# ==================================
# Compare Model Performance
# ==================================

# Compute RMSE (Root Mean Square Error) and MAE (Mean Absolute Error)
rmse <- function(actual, predicted) sqrt(mean((actual - predicted)^2))
mae <- function(actual, predicted) mean(abs(actual - predicted))

# Predictions for training data
pred_8_2_train <- predict(model_8_2, newdata=data_8_2)
pred_pca_train <- predict(model_pca_9_1, newdata=pca_data_9_1)

# Model performance comparison
performance_metrics <- data.frame(
  Model = c("Standard Regression (8.2)", "PCA Regression (9.1)"),
  R_squared = c(summary_8_2$r.squared, summary_pca_9_1$r.squared),
  Adjusted_R_squared = c(summary_8_2$adj.r.squared, summary_pca_9_1$adj.r.squared),
  RMSE = c(rmse(crime_data_8_2, pred_8_2_train), rmse(crime_data_9_1, pred_pca_train)),
  MAE = c(mae(crime_data_8_2, pred_8_2_train), mae(crime_data_9_1, pred_pca_train))
)

print(performance_metrics)

# ==================================
# Residual Analysis for Model Evaluation
# ==================================
par(mfrow=c(2,2))
plot(model_8_2, which=1:2, main="Residuals & Normality (8.2)")
plot(model_pca_9_1, which=1:2, main="Residuals & Normality (9.1)")
par(mfrow=c(1,1))

# ==================================
# Predict Crime Rate for a New City
# ==================================

# Define new city data
new_city <- data.frame(matrix(c(14.0, 0, 10.5, 7.1, 6.8, 0.6, 100, 10, 2.5, 0.1, 3, 5000, 20, 0.05, 25), nrow=1))
colnames(new_city) <- colnames(independent_vars_8_2)

# Standard Regression Model Prediction (8.2)
pred_8_2_new_city <- predict(model_8_2, newdata=new_city)

# Apply PCA Transformation for New City (9.1)
new_city_scaled <- scale(new_city, center=attr(scaled_data_9_1, "scaled:center"), scale=attr(scaled_data_9_1, "scaled:scale"))
new_city_pca <- predict(pca_result_9_1, newdata=new_city_scaled)

# Ensure new city data matches selected PCA components
actual_components <- min(selected_components, length(new_city_pca))
new_city_pca_df <- as.data.frame(new_city_pca[, 1:actual_components, drop=FALSE])
colnames(new_city_pca_df) <- paste0("PC", 1:actual_components)

# PCA Regression Model Prediction (9.1)
pred_pca_new_city <- predict(model_pca_9_1, newdata=new_city_pca_df)

# Print predictions
cat("\n=== Crime Rate Predictions for New City ===\n")
cat("Standard Regression Model (8.2) Prediction:", pred_8_2_new_city, "\n")
cat("PCA Regression Model (9.1) Prediction:", pred_pca_new_city, "\n")

# ==================================
# Visualization of PCA Variance Explained
# ==================================
variance_plot <- data.frame(PC = 1:length(explained_variance), Variance = explained_variance, Cumulative = cumulative_variance)

ggplot(variance_plot, aes(x=PC)) +
  geom_bar(aes(y=Variance), stat="identity", fill="blue", alpha=0.7) +
  geom_line(aes(y=Cumulative), color="red", size=1) +
  geom_point(aes(y=Cumulative), color="red") +
  labs(title="Explained Variance by Principal Components", x="Principal Components", y="Variance Explained") +
  theme_minimal()
