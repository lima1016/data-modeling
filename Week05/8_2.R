# 필요한 패키지 로드
# install.packages("corrplot")
library(corrplot)

# 데이터 불러오기
crime_data <- read.table("C:/Users/kimyo/Desktop/LIMA/OMSCS/강의/2025Spring/ISYE6501-IntroductionToAnalyticsModeling/Lecture/Week05/hw5/uscrime.txt", header = TRUE)

# 선형 회귀 모델 생성
crime_model <- lm(Crime ~ ., data = crime_data)

# 모델 요약 출력
summary(crime_model)

# 예측을 위한 새로운 데이터 정의
new_city <- data.frame(
  M = 14.0, So = 0, Ed = 10.0, Po1 = 12.0, Po2 = 15.5, 
  LF = 0.640, M.F = 94.0, Pop = 150, NW = 1.1, 
  U1 = 0.120, U2 = 3.6, Wealth = 3200, Ineq = 20.1, 
  Prob = 0.04, Time = 39.0
)

# 범죄율 예측
predicted_crime <- predict(crime_model, newdata = new_city)
print(predicted_crime)

