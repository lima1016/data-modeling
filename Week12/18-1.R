# 필요한 패키지 설치 및 로드
#install.packages(c("dplyr", "ggplot2", "pROC"))
library(dplyr)
library(ggplot2)
library(pROC)

# 가상 데이터 생성
set.seed(123)
customer_data <- data.frame(
  days_overdue = rnorm(100, mean = 30, sd = 10),
  past_payment_consistency = runif(100, 0, 1),
  income_level = rnorm(100, mean = 50000, sd = 15000),
  will_pay = rbinom(100, 1, 0.6) # 1 = 결제 가능, 0 = 결제 불가능
)

# 로지스틱 회귀 모델 학습
model <- glm(will_pay ~ days_overdue + past_payment_consistency + income_level, 
             data = customer_data, family = "binomial")

# 예측 확률 계산
customer_data$prob_pay <- predict(model, type = "response")

# 고객 분류 (0.5를 기준으로)
customer_data$shutoff_candidate <- ifelse(customer_data$prob_pay < 0.5, "Yes", "No")

# 결과 요약
summary(model)

# ROC 곡선 시각화
roc_obj <- roc(customer_data$will_pay, customer_data$prob_pay)
plot(roc_obj, main = "ROC Curve for Payment Prediction", col = "blue")
auc(roc_obj) # AUC 값 출력