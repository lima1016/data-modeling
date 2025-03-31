# 필요한 패키지 로드
library(lpSolve)
library(readxl)

# 파일 읽기
file_path <- "C:/Users/kimyo/Desktop/LIMA/OMSCS/강의/2025Spring/ISYE6501-IntroductionToAnalyticsModeling/Lecture/Week11/hw11/diet.xls"
data <- read_excel(file_path)

# 데이터 추출
foods <- data$Foods
costs <- as.numeric(gsub("\\$", "", data$Cost))
nutrients <- c("Calories", "Cholesterol mg", "Total_Fat g", "Sodium mg", "Carbohydrates g",
               "Dietary_Fiber g", "Protein g", "Vit_A IU", "Vit_C IU", "Calcium mg", "Iron mg")
nutrient_data <- as.matrix(data[, nutrients])
min_nutrients <- c(1500, 30, 20, 800, 130, 125, 60, 1000, 400, 700, 10)
max_nutrients <- c(2500, 240, 70, 2000, 450, 250, 100, 10000, 5000, 1500, 40)

# 육류/가금류/생선/계란 그룹 정의
meat_group <- c("Roasted Chicken", "Poached Eggs", "Scrambled Eggs", "Bologna,Turkey",
                "Frankfurter, Beef", "Ham,Sliced,Extralean", "Kielbasa,Prk", "Pork",
                "Sardines in Oil", "White Tuna in Water")
meat_indices <- which(foods %in% meat_group)

# 변수 개수: 연속 변수(x) + 이진 변수(y)
n_foods <- length(foods)
n_vars <- n_foods * 2  # x (인분) + y (선택 여부)

# 목적 함수: 비용 최소화 (y 변수는 비용에 영향 없으므로 0)
objective <- c(costs, rep(0, n_foods))

# 제약 조건 행렬 구성
constraints <- matrix(0, nrow = 0, ncol = n_vars)
direction <- c()
rhs <- c()

# 1. 영양소 최소/최대 제약
for (j in 1:length(nutrients)) {
  nutrient_row_min <- c(nutrient_data[, j], rep(0, n_foods))
  nutrient_row_max <- c(nutrient_data[, j], rep(0, n_foods))
  constraints <- rbind(constraints, nutrient_row_min, nutrient_row_max)
  direction <- c(direction, ">=", "<=")
  rhs <- c(rhs, min_nutrients[j], max_nutrients[j])
}

# 2. 최소 인분 제약: x_i >= 0.1 * y_i -> x_i - 0.1 * y_i >= 0
# 3. 최대 인분 제약: x_i <= M * y_i
M <- 1000
for (i in 1:n_foods) {
  min_serving <- rep(0, n_vars)
  max_serving <- rep(0, n_vars)
  min_serving[i] <- 1          # x_i
  min_serving[i + n_foods] <- -0.1  # -0.1 * y_i
  max_serving[i] <- 1          # x_i
  max_serving[i + n_foods] <- -M    # -M * y_i
  constraints <- rbind(constraints, min_serving, max_serving)
  direction <- c(direction, ">=", "<=")
  rhs <- c(rhs, 0, 0)
}

# 4. 셀러리 또는 브로콜리 제약: y_celery + y_broccoli <= 1
celery_idx <- which(foods == "Celery, Raw")
broccoli_idx <- which(foods == "Frozen Broccoli")
celery_broccoli <- rep(0, n_vars)
celery_broccoli[celery_idx + n_foods] <- 1
celery_broccoli[broccoli_idx + n_foods] <- 1
constraints <- rbind(constraints, celery_broccoli)
direction <- c(direction, "<=")
rhs <- c(rhs, 1)

# 5. 최소 3종류 육류 제약: sum(y_i for i in meat_group) >= 3
meat_constraint <- rep(0, n_vars)
meat_constraint[meat_indices + n_foods] <- 1
constraints <- rbind(constraints, meat_constraint)
direction <- c(direction, ">=")
rhs <- c(rhs, 3)

# 변수 타입 설정: x는 연속, y는 이진
types <- c(rep("C", n_foods), rep("B", n_foods))

# LP 문제 해결
solution <- lp("min", objective, constraints, direction, rhs, all.bin = FALSE, binary.vec = (n_foods + 1):n_vars)

# 결과 출력
if (solution$status == 0) {
  cat("Status: Optimal\n")
  cat("Total Cost = $", round(solution$objval, 2), "\n\n")
  cat("Diet Plan:\n")
  servings <- solution$solution[1:n_foods]
  for (i in 1:n_foods) {
    if (servings[i] > 0) {
      cat(sprintf("%s: %.2f servings\n", foods[i], servings[i]))
    }
  }
} else {
  cat("Status: No optimal solution found\n")
}