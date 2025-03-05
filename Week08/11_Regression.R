library(glmnet)  # For Lasso and Elastic Net
library(MASS)    # For stepwise regression

# Load the dataset (assuming it's saved as uscrime.txt)
data <- read.table("C:/Users/kimyo/Desktop/LIMA/OMSCS/강의/2025Spring/ISYE6501-IntroductionToAnalyticsModeling/Lecture/Week08/hw8/uscrime.txt", header = TRUE)

# Define predictors (X) and response (y)
X <- as.matrix(data[, 1:15])  # Convert predictors to matrix format
y <- data$Crime               # Response variable

# Check the first few rows to confirm
head(data)

# Stepwise Regression ###################################
# Fit a full model with all predictors
full_model <- lm(Crime ~ ., data = data)

# Perform stepwise regression (both directions)
step_model <- stepAIC(full_model, direction = "both", trace = FALSE)

# Summary of the final model
summary(step_model)

# Visualize the fitted vs actual values
plot(data$Crime, predict(step_model), 
     xlab = "Actual Crime", ylab = "Predicted Crime", 
     main = "Stepwise Regression: Actual vs Predicted")
abline(0, 1, col = "red")  # Add a 45-degree line

# Lasso Regression ###################################
# Scale the predictors (excluding the response)
X_scaled <- scale(X)

# Fit Lasso model (alpha = 1)
lasso_model <- glmnet(X_scaled, y, alpha = 1)

# Cross-validation to find optimal lambda
cv_lasso <- cv.glmnet(X_scaled, y, alpha = 1)
best_lambda_lasso <- cv_lasso$lambda.min

# Plot cross-validation results
plot(cv_lasso, main = "Lasso: Cross-Validation for Lambda")

# Fit final Lasso model with best lambda
lasso_final <- glmnet(X_scaled, y, alpha = 1, lambda = best_lambda_lasso)

# Coefficients
print(coef(lasso_final))

# Elastic Net Regression ###################################
# Fit Elastic Net with cross-validation for a range of alpha values
alpha_values <- seq(0, 1, by = 0.1)  # Test alpha from 0 to 1
best_lambda_enet <- numeric(length(alpha_values))
mse_min <- numeric(length(alpha_values))

for (i in 1:length(alpha_values)) {
  cv_enet <- cv.glmnet(X_scaled, y, alpha = alpha_values[i])
  best_lambda_enet[i] <- cv_enet$lambda.min
  mse_min[i] <- min(cv_enet$cvm)  # Minimum cross-validated MSE
}

# Find the best alpha
best_alpha <- alpha_values[which.min(mse_min)]
best_lambda <- best_lambda_enet[which.min(mse_min)]

# Fit final Elastic Net model
enet_final <- glmnet(X_scaled, y, alpha = best_alpha, lambda = best_lambda)

# Coefficients
print(coef(enet_final))

# Plot MSE vs Alpha
plot(alpha_values, mse_min, type = "b", 
     xlab = "Alpha", ylab = "Minimum MSE", 
     main = "Elastic Net: MSE vs Alpha")
abline(v = best_alpha, col = "red", lty = 2)








