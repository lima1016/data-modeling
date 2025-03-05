# 패키지 로드
library(glmnet)  # 라쏘와 엘라스틱 넷용
library(MASS)    # 단계적 회귀용

# 데이터셋 불러오기 (uscrime.txt로 저장된 것으로 가정)
data <- read.table("C:/Users/kimyo/Desktop/LIMA/OMSCS/강의/2025Spring/ISYE6501-IntroductionToAnalyticsModeling/Lecture/Week08/hw8/uscrime.txt", header = TRUE)

# 예측 변수(X)와 반응 변수(y) 정의
X <- as.matrix(data[, 1:15])  # 예측 변수를 매트릭스 형식으로 변환
y <- data$Crime               # 반응 변수

# 데이터 확인
head(data)

# 파트 1: 단계적 회귀 (Stepwise Regression)
full_model <- lm(Crime ~ ., data = data)
step_model <- stepAIC(full_model, direction = "both", trace = FALSE)
summary(step_model)

# 실제 값과 예측 값 비교 시각화
plot(data$Crime, predict(step_model), 
     xlab = "Actual Crime", ylab = "Predicted Crime", 
     main = "Stepwise Regression: Actual vs Predicted")
abline(0, 1, col = "red")

# 파트 2: 라쏘 회귀 (Lasso Regression)
X_scaled <- scale(X)
lasso_model <- glmnet(X_scaled, y, alpha = 1)
cv_lasso <- cv.glmnet(X_scaled, y, alpha = 1)
best_lambda_lasso <- cv_lasso$lambda.min
plot(cv_lasso, main = "Lasso: Cross-Validation for Lambda")
lasso_final <- glmnet(X_scaled, y, alpha = 1, lambda = best_lambda_lasso)
print(coef(lasso_final))

# 파트 3: 엘라스틱 넷 회귀 (Elastic Net Regression)
alpha_values <- seq(0, 1, by = 0.1)
best_lambda_enet <- numeric(length(alpha_values))
mse_min <- numeric(length(alpha_values))

for (i in 1:length(alpha_values)) {
  cv_enet <- cv.glmnet(X_scaled, y, alpha = alpha_values[i])
  best_lambda_enet[i] <- cv_enet$lambda.min
  mse_min[i] <- min(cv_enet$cvm)
}

best_alpha <- alpha_values[which.min(mse_min)]
best_lambda <- best_lambda_enet[which.min(mse_min)]
enet_final <- glmnet(X_scaled, y, alpha = best_alpha, lambda = best_lambda)
print(coef(enet_final))

# MSE vs Alpha 시각화
plot(alpha_values, mse_min, type = "b", 
     xlab = "Alpha", ylab = "Minimum MSE", 
     main = "Elastic Net: MSE vs Alpha")
abline(v = best_alpha, col = "red", lty = 2)