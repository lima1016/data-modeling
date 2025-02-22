# 데이터 나누기 위한 라이브러리 설치
# install.packages("caret")
library(caret)

# 예제 데이터 생성
set.seed(123)
data <- iris

# 데이터를 훈련(70%), 테스트(30%)로 나누기
trainIndex <- createDataPartition(data$Species, p = 0.7, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# 훈련 데이터 확인
head(trainData)

# 검증 데이터 추가적으로 나누기
set.seed(123)
validationIndex <- createDataPartition(trainData$Species, p = 0.8, list = FALSE)
validationData <- trainData[-validationIndex, ]
trainData <- trainData[validationIndex, ]

# 데이터 크기 확인
cat("훈련 데이터 크기:", nrow(trainData), "\n")
cat("검증 데이터 크기:", nrow(validationData), "\n")
cat("테스트 데이터 크기:", nrow(testData), "\n")
