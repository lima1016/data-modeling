# 데이터 읽기
temps_data <- read.table("C:/Users/kimyo/Desktop/LIMA/OMSCS/강의/2025Spring/ISYE6501-IntroductionToAnalyticsModeling/HW/Week03/hw3/temps.txt", header = TRUE, sep = "\t")

# 기본적인 데이터 확인
print("Data structure check")
str(temps_data)

# CUSUM 계산 함수
calculate_cusum <- function(x) {
  x <- x[!is.na(x)]  # NA 값 제외
  if(length(x) == 0) return(NA)
  mean_val <- mean(x)
  deviations <- x - mean_val
  cumsum(deviations)
}

# 결과를 저장할 데이터프레임 초기화
results <- data.frame(
  year = 1996:2015,
  summer_end = NA,
  avg_temp = NA
)

# 각 연도별 분석
for(i in 1:nrow(results)) {
  year <- results$year[i]
  year_col <- paste0("X", year)
  
  # 해당 연도의 기온 데이터 추출
  if(year_col %in% names(temps_data)) {
    temps <- temps_data[[year_col]]
    
    # CUSUM 계산
    cusum <- calculate_cusum(temps)
    
    # 여름 종료일 찾기 (CUSUM 최대값 위치)
    # cusum 벡터가 NA가 아니고 길이가 0보다 큰지 확인
    if(!all(is.na(cusum)) && length(cusum) > 0) {
      max_idx <- which.max(cusum)
      results$summer_end[i] <- as.character(temps_data$DAY[max_idx])
      results$avg_temp[i] <- mean(temps, na.rm = TRUE)
    }
  }
}

# 기온 변화 추세 계산
results$temp_deviation <- results$avg_temp - mean(results$avg_temp, na.rm = TRUE)
results$temp_cusum <- cumsum(results$temp_deviation)

# 그래프 그리기
par(mfrow = c(2,1), mar = c(4,4,3,2))

# NA 값을 제외한 데이터만 사용하여 그래프 그리기
valid_data <- results[!is.na(results$avg_temp) & !is.na(results$temp_cusum), ]

# 여름 종료일 그래프
if(nrow(valid_data) > 0) {
  plot(valid_data$year, 1:nrow(valid_data), 
       type = "b", 
       main = "Summer End Date Change (1996-2015)",
       xlab = "Year", 
       ylab = "End Date Order",
       pch = 16)
  grid()
}

# 기온 변화 추세 그래프
if(nrow(valid_data) > 0) {
  plot(valid_data$year, valid_data$temp_cusum, 
       type = "b",
       main = "Temperature Change Trend (CUSUM)",
       xlab = "Year", 
       ylab = "Cumulative Temperature Deviation",
       pch = 16)
  grid()
}

# 주요 통계
early_period <- mean(results$avg_temp[1:10], na.rm = TRUE)
late_period <- mean(results$avg_temp[11:20], na.rm = TRUE)

cat("\nFirst period (1996-2005) average:", round(early_period, 1), "°F\n")
cat("\nSecond period (2006-2015) average:", round(late_period, 1), "°F\n")
cat("\nTemperature change:", round(late_period - early_period, 1), "°F\n")
