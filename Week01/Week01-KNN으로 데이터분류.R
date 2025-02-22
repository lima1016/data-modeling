library(kknn)

# 데이터 준비
data <- read.table("C:/Users/kimyo/Desktop/LIMA/OMSCS/강의/2025Spring/ISYE6501-IntroductionToAnalyticsModeling/HW/Week01/hw1/data 2.2/credit_card_data-headers.txt", header = TRUE)

# 최적의 k 값을 찾기 위한 반복문
k_values <- c(0.1, 1, 2, 3, 5, 7, 9, 100)
accuracy_results <- numeric(length(k_values))

for (i in 1:length(k_values)) {
  k <- k_values[i]
  
  # k-NN 모델 학습
  model <- kknn(as.factor(data[, 11]) ~ ., data, data, k = k, scale = TRUE)
  
  # 예측 및 정확도 계산
  pred <- fitted(model)
  accuracy <- sum(pred == as.factor(data[, 11])) / nrow(data)
  accuracy_results[i] <- accuracy
  
  print(paste("k =", k, "Accuracy:", accuracy))
}

# 최적의 k 값 및 정확도 출력
best_k <- k_values[which.max(accuracy_results)]
best_accuracy <- max(accuracy_results)

print(paste("Best k:", best_k, "with Accuracy:", best_accuracy))
