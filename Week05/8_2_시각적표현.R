# 필요한 패키지 로드
# install.packages("corrplot")
# install.packages("ggplot2")
library(ggplot2)
library(corrplot)

# 데이터 불러오기
crime_data <- read.table("C:/Users/kimyo/Desktop/LIMA/OMSCS/강의/2025Spring/ISYE6501-IntroductionToAnalyticsModeling/HW/Week05/hw5/uscrime.txt", header = TRUE)

# 상관 행렬 계산
cor_matrix <- cor(crime_data)

# 상관 행렬 시각화 (히트맵)
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.7, title = "Crime Data Correlation Matrix")

# 범죄율과 소득 불평등(Ineq)의 관계
ggplot(crime_data, aes(x = Ineq, y = Crime)) +
  geom_point(color = "blue") +  # 데이터 포인트
  geom_smooth(method = "lm", color = "red") +  # 회귀선 추가
  ggtitle("Crime Rate vs Income Inequality") +
  xlab("Income Inequality (Ineq)") +
  ylab("Crime Rate (Crime)") +
  theme_minimal()

# 회귀 모델 생성
crime_model <- lm(Crime ~ ., data = crime_data)

# 잔차 플롯 (Residual Plot)
plot(crime_model$fitted.values, residuals(crime_model),
     xlab = "Fitted Crime Rates", ylab = "Residuals",
     main = "Residual Plot of the Regression Model",
     col = "blue", pch = 19)
abline(h = 0, col = "red", lwd = 2)

# 잔차 분포 히스토그램
hist(residuals(crime_model), breaks = 15, col = "lightblue",
     main = "Histogram of Residuals", xlab = "Residuals")
