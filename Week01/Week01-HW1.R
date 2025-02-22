# 패키지 로드
library(kernlab)

# 데이터 로드
data <- read.table("C:/Users/kimyo/Desktop/LIMA/OMSCS/강의/2025Spring/ISYE6501-IntroductionToAnalyticsModeling/HW/Week01/hw1/data 2.2/credit_card_data-headers.txt", header = TRUE)

# SVM 모델 학습
model <- ksvm(as.matrix(data[, 1:10]), as.factor(data[, 11]), type = "C-svc", 
              kernel = "vanilladot", C = 1, scaled = TRUE)
# calculate a1…am
# colSums -> 위 결과를 열 방향으로 합산하여 최종 기울기를 계산
# xmatrix -> SVM이 학습한 Support Vectors를 의미. 모델에서 결정 경계에 가장 중요한 데이터 포인트를 나타냄냄
# coef -> 지원 벡터와 연결된 가중치(계수)를 의미.
a <- colSums(model@xmatrix[[1]] * model@coef[[1]])
a
# calculate a0
a0 <- -model@b
a0
# see what the model predicts
# 데이터의 각 샘플에 대해 예측한 클래스 (0또는 1)를 포함한다.
# 모델의 절편 (a0a_0a0) 계산.
pred <- predict(model,data[,1:10])
pred
# see what fraction of the model’s predictions match the actual classification
# 모델이 데이터를 얼마나 정확히 분류했는지 비율로 반환환 -> 정확도를 나타냄.
sum(pred == data[,11]) / nrow(data)

accuracy <- sum(pred == as.factor(data[, 11])) / nrow(data)
print(paste("Accuracy:", accuracy))
