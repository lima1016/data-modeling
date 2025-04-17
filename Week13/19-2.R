library(lpSolve)
library(tidyverse)
library(ggplot2)

# 데이터 설정
margins <- c(5, 3, 4)  # 제품별 수익 마진
sales_rate <- c(2, 1.5, 1.8)  # 공간 단위당 매출
min_space <- c(10, 15, 12)  # 최소 공간
max_space <- c(50, 40, 45)  # 최대 공간
total_space <- 100  # 총 공간

# 목표 함수
objective <- margins * sales_rate

# 제약 조건
constraints <- rbind(
  diag(3),  # 각 제품 최소 공간
  diag(3),  # 각 제품 최대 공간
  rep(1, 3)  # 총 공간
)
rhs <- c(min_space, max_space, total_space)
direction <- c(">=", ">=", ">=", "<=", "<=", "<=", "=")  # 수정: 7개 제약

# 최적화
result <- lp("max", objective, constraints, direction, rhs)
cat("최적 공간:", result$solution, "\n")
cat("최대 수익:", result$objval, "\n")

# 시각화 데이터
optimal <- tibble(
  product = c("A", "B", "C"),
  space = result$solution
)

# 막대 차트
ggplot(optimal, aes(x = product, y = space, fill = product)) +
  geom_bar(stat = "identity") +
  labs(title = "최적 선반 공간 배분", x = "제품", y = "공간 (평방 피트)") +
  theme_minimal()