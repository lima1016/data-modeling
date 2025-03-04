# Load necessary libraries
library(readr)
library(dplyr)
library(ggplot2)
library(pROC)

# Load the GermanCredit dataset (assuming it's saved as "germancredit.txt")
data <- read.table("C:/Users/kimyo/Desktop/LIMA/OMSCS/강의/2025Spring/ISYE6501-IntroductionToAnalyticsModeling/Lecture/Week07/hw7/germancredit.txt", header = FALSE)

# Assign column names based on the dataset description
colnames(data) <- c("checking_status", "duration", "credit_history", "purpose", 
                    "credit_amount", "savings_status", "employment", "installment_rate", 
                    "personal_status", "other_parties", "residence_since", "property_magnitude", 
                    "age", "other_payment_plans", "housing", "existing_credits", 
                    "job", "num_dependents", "telephone", "foreign_worker", "class")

# Convert class to binary (1 = good, 2 = bad -> 0 = bad, 1 = good)
data$class <- ifelse(data$class == 1, 1, 0)

# Fit logistic regression model with a subset of predictors for simplicity
model <- glm(class ~ duration + credit_amount + age + employment + checking_status, 
             data = data, family = binomial(link = "logit"))

# Display model summary (coefficients and output)
summary(model)

# Model coefficients
coef(model)

# Quality of fit: Predict probabilities
data$predicted_prob <- predict(model, type = "response")

# Confusion matrix at default threshold (0.5)
data$predicted_class <- ifelse(data$predicted_prob > 0.5, 1, 0)
confusion_matrix <- table(data$class, data$predicted_class)
print(confusion_matrix)

# Accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
cat("Accuracy:", accuracy, "\n")

# ROC Curve and AUC for quality of fit
roc_obj <- roc(data$class, data$predicted_prob)
plot(roc_obj, main = "ROC Curve for Credit Risk Model", col = "blue")
auc_value <- auc(roc_obj)
cat("AUC:", auc_value, "\n")

# Determine optimal threshold based on 5:1 cost ratio
# Cost function: 5 * False Positives + False Negatives
thresholds <- seq(0, 1, by = 0.01)
costs <- sapply(thresholds, function(t) {
  pred_class <- ifelse(data$predicted_prob > t, 1, 0)
  cm <- table(data$class, pred_class)
  fp <- ifelse(ncol(cm) > 1, cm[1, 2], 0)  # False positives
  fn <- ifelse(nrow(cm) > 1, cm[2, 1], 0)  # False negatives
  cost <- 5 * fp + fn
  return(cost)
})

optimal_threshold <- thresholds[which.min(costs)]
cat("Optimal Threshold (5:1 cost ratio):", optimal_threshold, "\n")

# Visualize cost vs threshold
ggplot(data.frame(thresholds, costs), aes(x = thresholds, y = costs)) +
  geom_line(color = "red") +
  geom_vline(xintercept = optimal_threshold, linetype = "dashed", color = "blue") +
  labs(title = "Cost vs Threshold", x = "Threshold", y = "Cost (5*FP + FN)") +
  theme_minimal()