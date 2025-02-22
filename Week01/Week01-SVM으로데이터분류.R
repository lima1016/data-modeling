#  SVM으로 데이터 분류
# 패키지 설치
if (!requireNamespace("kernlab")) install.packages("kernlab")
library(kernlab)

# 데이터 준비 (특징 열과 타겟 열 분리)
X <- as.matrix(data[, 1:10])
y <- as.factor(data[, 11])  # 이진 분류로 변환

# SVM 모델 학습
# C 값을 조정하며 최적의 값 찾기
C_values <- c(0.1, 1, 10, 100, 1000)
best_accuracy <- 0
best_model <- NULL
best_C <- NULL

for (C in C_values) {
  model <- ksvm(X, y, type = "C-svc", kernel = "vanilladot", C = C, scaled = TRUE)
  pred <- predict(model, X)
  accuracy <- sum(pred == y) / length(y)
  
  cat("C =", C, ", Accuracy =", accuracy, "\n")
  
  if (accuracy > best_accuracy) {
    best_accuracy <- accuracy
    best_model <- model
    best_C <- C
  }
}

# 최적의 모델 계수 추출
a <- colSums(best_model@xmatrix[[1]] * best_model@coef[[1]])
a0 <- -best_model@b

cat("Best C =", best_C, "\n")
cat("Coefficients (a):", a, "\n")
cat("Intercept (a0):", a0, "\n")

# 전체 데이터 정확도
final_pred <- predict(best_model, X)
final_accuracy <- sum(final_pred == y) / length(y)
cat("Final Accuracy =", final_accuracy, "\n")
