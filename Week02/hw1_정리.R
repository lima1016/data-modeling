# 필요한 라이브러리 로드
library(caret)
library(kknn)
library(e1071)

# 데이터 불러오기
data <- read.table("C:/Users/kimyo/Desktop/LIMA/OMSCS/강의/2025Spring/ISYE6501-IntroductionToAnalyticsModeling/HW/Week02/hw2/data 3.1/credit_card_data-headers.txt", 
                   header = TRUE, sep = "\t")

# 데이터셋 확인 및 전처리
head(data)
data$R1 <- as.factor(data$R1) # 목표 변수 범주형으로 변환

# 데이터 분할
set.seed(123)
trainIndex <- createDataPartition(data$R1, p = 0.7, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# KNN 모델 구현 (교차 검증 포함)
cat("\nKNN 모델 결과\n")
knn_model <- train(
  R1 ~ ., data = trainData, method = "kknn",
  trControl = trainControl(method = "cv", number = 5)
)
print(knn_model)

# KNN 테스트 데이터 평가
cat("\nKNN 테스트 데이터 평가\n")
knn_pred <- predict(knn_model, newdata = testData)
knn_confusion <- confusionMatrix(knn_pred, testData$R1)
print(knn_confusion)

# SVM 모델 구현 및 평가
cat("\nSVM 모델 결과\n")
svm_model <- svm(R1 ~ ., data = trainData, kernel = "radial")
svm_pred <- predict(svm_model, testData)
svm_confusion <- confusionMatrix(svm_pred, testData$R1)
print(svm_confusion)

# 성능 요약 출력
cat("\n모델 성능 요약\n")
cat(sprintf("KNN 정확도: %.2f%%\n", knn_confusion$overall['Accuracy'] * 100))
cat(sprintf("SVM 정확도: %.2f%%\n", svm_confusion$overall['Accuracy'] * 100))
