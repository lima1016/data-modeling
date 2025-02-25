# 필요한 라이브러리 로드
# install.packages("e1071")
# install.packages("caret")
# install.packages("dplyr")

# 필요한 라이브러리 로드
library(caret)
library(kknn)
library(e1071)

# 데이터 불러오기
data <- read.table("C:/Users/kimyo/Desktop/LIMA/OMSCS/강의/2025Spring/ISYE6501-IntroductionToAnalyticsModeling/HW/Week02/hw2/data 3.1/credit_card_data-headers.txt", header = TRUE, sep = "\t")

# 데이터셋 확인 및 전처리
head(data)
data$R1 <- as.factor(data$R1) # 목표 변수 범주형으로 변환

# 데이터 분할
set.seed(123)
trainIndex <- createDataPartition(data$R1, p = 0.7, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# KNN 모델 구현 (교차 검증)
knn_model <- train(
  R1 ~ ., data = trainData, method = "kknn",
  trControl = trainControl(method = "cv", number = 5)
)

# 결과 확인
print(knn_model)

# SVM 모델
svm_model <- svm(R1 ~ ., data = trainData, kernel = "radial")
svm_pred <- predict(svm_model, testData)
confusionMatrix(svm_pred, testData$R1)

# 테스트 데이터 성능 평가
knn_pred <- predict(knn_model, newdata = testData)
confusionMatrix(knn_pred, testData$R1)
