# Load necessary library
library(outliers)

# Read the dataset
uscrime_data <- read.table("C:/Users/kimyo/Desktop/LIMA/OMSCS/강의/2025Spring/ISYE6501-IntroductionToAnalyticsModeling/HW/Week03/hw3/uscrime.txt", header = TRUE)

# Extract the last column (Crimes per 100,000 people)
crime_rate <- uscrime_data$Crime

# Perform Grubbs test for outliers
grubbs_test_result <- grubbs.test(crime_rate)

# Print the result
print(grubbs_test_result)

# 데이터의 정규성 확인
# Normality test
shapiro_test <- shapiro.test(crime_rate)
print(shapiro_test)

# Visualize data distribution
hist(crime_rate, main = "Crime Rate Distribution", xlab = "Crime Rate", col = "lightblue", border = "black")
qqnorm(crime_rate)
qqline(crime_rate, col = "red")


# 시각화
# Summary statistics
summary(crime_rate)
sd(crime_rate)

# Boxplot visualization
boxplot(crime_rate, main = "Crime Rate Boxplot", col = "purple", ylab = "Crime Rate")
