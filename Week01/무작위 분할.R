# 데이터 나누기 위한 라이브러리
# install.packages("caret")
library(caret)

# 예제 데이터 생성
set.seed(123)
data <- iris

# 무작위로 데이터 분할 (훈련 70%, 테스트 30%)
trainIndex <- createDataPartition(data$Species, p = 0.7, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# 결과 확인
cat("훈련 데이터 크기:", nrow(trainData), "\n")
cat("테스트 데이터 크기:", nrow(testData), "\n")
