# 데이터 로드
data <- read.table("C:/Users/kimyo/Desktop/LIMA/OMSCS/강의/2025Spring/ISYE6501-IntroductionToAnalyticsModeling/HW/Week03/hw3/temps.txt", header = TRUE)

# 분석 범위 필터링 (7월~10월 데이터)
data_filtered <- data[grep("Jul|Aug|Sep|Oct", data$DAY), ]

# 연도별 평균 기온 계산
years <- colnames(data_filtered)[-1]
yearly_avg <- sapply(years, function(year) mean(data_filtered[[year]], na.rm = TRUE))

# 기준값(평균 기온)
overall_mean <- mean(unlist(data_filtered[-1]), na.rm = TRUE)

# CUSUM 계산
cusum <- cumsum(yearly_avg - overall_mean)

# CUSUM 변화 시각화
plot(years, cusum, type = "o", col = "blue", xlab = "Year", ylab = "CUSUM", main = "CUSUM Analysis for Atlanta Summer Climate")

# 여름 종료 시점 식별 (변화 기울기 감소 시점)
summer_end <- apply(data_filtered[-1], 2, function(x) {
  diff_temp <- diff(x)
  which(diff_temp < 0)[1]  # 첫 감소 시점
})

# 결과 출력
cat("연도별 비공식적인 여름 종료 시점 (줄번호):\n")
print(summer_end)

cat("\n여름 기후가 더워진 시점을 판단:\n")
warming_trend <- which.max(cusum)
cat(sprintf("기후 변화 시작 연도: %s\n", years[warming_trend]))
