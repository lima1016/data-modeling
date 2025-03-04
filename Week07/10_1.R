# Load required libraries
library(rpart)          # For regression tree
library(randomForest)   # For random forest
library(ggplot2)        # For visualization (optional)

# Load dataset
data <- read.table("C:/Users/kimyo/Desktop/LIMA/OMSCS/강의/2025Spring/ISYE6501-IntroductionToAnalyticsModeling/Lecture/Week07/hw7/uscrime.txt", header=TRUE)

# Check the data
head(data)

# (a) Regression Tree Model
# Build the regression tree model
tree_model <- rpart(Crime ~ ., data = data, method = "anova")

# Print the summary of the tree model
print(tree_model)
summary(tree_model)

# Plot the tree (optional visualization)
plot(tree_model, uniform = TRUE, main = "Regression Tree for Crime Data")
text(tree_model, use.n = TRUE, all = TRUE, cex = 0.8)

# Predict using the tree model
tree_pred <- predict(tree_model, newdata = data)

# Calculate R-squared for the tree model
tree_rsq <- 1 - sum((data$Crime - tree_pred)^2) / sum((data$Crime - mean(data$Crime))^2)
cat("R-squared for Regression Tree:", tree_rsq, "\n")

# (b) Random Forest Model
# Build the random forest model
rf_model <- randomForest(Crime ~ ., data = data, ntree = 500, importance = TRUE)

# Print the random forest model
print(rf_model)

# Predict using the random forest model
rf_pred <- predict(rf_model, newdata = data)

# Calculate R-squared for the random forest model
rf_rsq <- 1 - sum((data$Crime - rf_pred)^2) / sum((data$Crime - mean(data$Crime))^2)
cat("R-squared for Random Forest:", rf_rsq, "\n")

# Variable importance for random forest
importance(rf_model)
varImpPlot(rf_model, main = "Variable Importance in Random Forest")

# Optional: Compare actual vs predicted (visualization)
comparison_df <- data.frame(Actual = data$Crime, Tree_Pred = tree_pred, RF_Pred = rf_pred)
ggplot(comparison_df, aes(x = Actual)) +
  geom_point(aes(y = Tree_Pred, color = "Regression Tree")) +
  geom_point(aes(y = RF_Pred, color = "Random Forest")) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(title = "Actual vs Predicted Crime Rate", x = "Actual", y = "Predicted") +
  scale_color_manual(values = c("blue", "red")) +
  theme_minimal()