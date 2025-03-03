# Load required libraries
library(ggplot2)
library(caret)
library(dplyr)
library(reshape2)

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

# ==================================
# Visualizations
# ==================================

# 1. Explained Variance Plot (PCA Effectiveness)
variance_plot <- data.frame(PC = 1:length(explained_variance), 
                            Variance = explained_variance, 
                            Cumulative = cumulative_variance)

ggplot(variance_plot, aes(x=PC)) +
  geom_bar(aes(y=Variance), stat="identity", fill="blue", alpha=0.7) +
  geom_line(aes(y=Cumulative), color="red", size=1) +
  geom_point(aes(y=Cumulative), color="red") +
  labs(title="Explained Variance by Principal Components", 
       x="Principal Components", y="Variance Explained") +
  theme_minimal()

# 2. Regression Coefficients Comparison
coefficients_df <- data.frame(Variable = rownames(original_coefficients), 
                              PCA_Model = original_coefficients[,1], 
                              Standard_Model = summary_8_2$coefficients[-1,1])

coefficients_long <- melt(coefficients_df, id.vars = "Variable")

ggplot(coefficients_long, aes(x=Variable, y=value, fill=variable)) +
  geom_bar(stat="identity", position="dodge") +
  coord_flip() +
  labs(title="Comparison of Regression Coefficients", 
       x="Variables", y="Coefficient Value") +
  theme_minimal()

# 3. Residuals Analysis (Model Fit)
par(mfrow=c(2,2))
plot(model_8_2, which=1:2, main="Residuals & Normality (8.2)")
plot(model_pca, which=1:2, main="Residuals & Normality (9.1)")
par(mfrow=c(1,1))

# 4. Actual vs Predicted Values (Crime Rate)
pred_8_2 <- predict(model_8_2, newdata=data)
pred_pca <- predict(model_pca, newdata=pca_data)

comparison_df <- data.frame(
  Actual = crime_data,
  Predicted_8_2 = pred_8_2,
  Predicted_PCA = pred_pca
)

ggplot(comparison_df, aes(x=Actual)) +
  geom_point(aes(y=Predicted_8_2, color="Standard Regression")) +
  geom_point(aes(y=Predicted_PCA, color="PCA Regression")) +
  geom_abline(intercept=0, slope=1, linetype="dashed") +
  labs(title="Actual vs Predicted Crime Rate", 
       x="Actual Crime Rate", y="Predicted Crime Rate") +
  theme_minimal() +
  scale_color_manual(values=c("red", "blue"))
