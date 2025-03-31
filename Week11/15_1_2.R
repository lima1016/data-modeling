library(lpSolve)
library(readxl)
library(ggplot2)
library(reshape2)

# 파일 읽기
file_path <- "C:/Users/kimyo/Desktop/LIMA/OMSCS/강의/2025Spring/ISYE6501-IntroductionToAnalyticsModeling/Lecture/Week11/hw11/diet.xls"
data <- read_excel(file_path)

# 데이터 추출
foods <- data$Foods
costs <- as.numeric(gsub("\\$", "", data$"Price/ Serving"))
nutrients <- c("Calories", "Cholesterol mg", "Total_Fat g", "Sodium mg", "Carbohydrates g",
               "Dietary_Fiber g", "Protein g", "Vit_A IU", "Vit_C IU", "Calcium mg", "Iron mg")
nutrient_data <- as.matrix(data[, nutrients])
min_nutrients <- c(1500, 30, 20, 800, 130, 125, 60, 1000, 400, 700, 10)
max_nutrients <- c(2500, 240, 70, 2000, 450, 250, 100, 10000, 5000, 1500, 40)

# NA 제거
valid_indices <- !is.na(foods) & !is.na(costs)
foods <- foods[valid_indices]
costs <- costs[valid_indices]
nutrient_data <- nutrient_data[valid_indices, ]
n_foods <- length(foods)
n_vars <- n_foods * 2

# 데이터 확인
cat("Foods sample:\n")
print(head(foods))
cat("Costs sample:\n")
print(head(costs))
cat("Nutrient data sample:\n")
print(head(nutrient_data))
cat("Any NA in costs?\n")
print(any(is.na(costs)))
cat("Any NA in nutrient_data?\n")
print(any(is.na(nutrient_data)))

# NA 처리
if (any(is.na(costs))) {
  cat("NA found in costs, replacing with 0\n")
  costs[is.na(costs)] <- 0
}
if (any(is.na(nutrient_data))) {
  cat("NA found in nutrient_data, replacing with 0\n")
  nutrient_data[is.na(nutrient_data)] <- 0
}

# 변수 설정
objective <- c(costs, rep(0, n_foods))

# 최소 모델 테스트
constraints <- matrix(0, nrow = length(nutrients), ncol = n_foods)
for (j in 1:length(nutrients)) {
  constraints[j, ] <- nutrient_data[, j]
}
direction <- rep(">=", length(nutrients))
rhs <- min_nutrients

cat("\nTesting minimal model (only nutrient constraints):\n")
solution_min <- lp("min", costs, constraints, direction, rhs)
cat("Minimal model status:", solution_min$status, "\n")
cat("Minimal model cost:", solution_min$objval, "\n")
cat("Minimal model solution sample:\n")
print(head(solution_min$solution))

# 전체 모델
constraints_full <- matrix(0, nrow = 0, ncol = n_vars)
direction_full <- c()
rhs_full <- c()

# 영양소 제약
for (j in 1:length(nutrients)) {
  nutrient_row_min <- c(nutrient_data[, j], rep(0, n_foods))
  nutrient_row_max <- c(nutrient_data[, j], rep(0, n_foods))
  constraints_full <- rbind(constraints_full, nutrient_row_min, nutrient_row_max)
  direction_full <- c(direction_full, ">=", "<=")
  rhs_full <- c(rhs_full, min_nutrients[j], max_nutrients[j])
}

# 최소/최대 인분 제약
M <- 1000
for (i in 1:n_foods) {
  min_serving <- rep(0, n_vars)
  max_serving <- rep(0, n_vars)
  min_serving[i] <- 1
  min_serving[i + n_foods] <- -0.1
  max_serving[i] <- 1
  max_serving[i + n_foods] <- -M
  constraints_full <- rbind(constraints_full, min_serving, max_serving)
  direction_full <- c(direction_full, ">=", "<=")
  rhs_full <- c(rhs_full, 0, 0)
}

# 셀러리/브로콜리 제약
celery_idx <- which(foods == "Celery, Raw")
broccoli_idx <- which(foods == "Frozen Broccoli")
celery_broccoli <- rep(0, n_vars)
celery_broccoli[celery_idx + n_foods] <- 1
celery_broccoli[broccoli_idx + n_foods] <- 1
constraints_full <- rbind(constraints_full, celery_broccoli)
direction_full <- c(direction_full, "<=")
rhs_full <- c(rhs_full, 1)

# 육류 최소 3종
meat_group <- c("Roasted Chicken", "Poached Eggs", "Scrambled Eggs", "Bologna,Turkey",
                "Frankfurter, Beef", "Ham,Sliced,Extralean", "Kielbasa,Prk", "Pork",
                "Sardines in Oil", "White Tuna in Water")
meat_indices <- which(foods %in% meat_group)
meat_constraint <- rep(0, n_vars)
meat_constraint[meat_indices + n_foods] <- 1
constraints_full <- rbind(constraints_full, meat_constraint)
direction_full <- c(direction_full, ">=")
rhs_full <- c(rhs_full, 3)

# 변수 타입
types <- c(rep("C", n_foods), rep("B", n_foods))

# LP 해결
cat("\nTesting full model:\n")
solution <- lp("min", objective, constraints_full, direction_full, rhs_full, 
               all.bin = FALSE, binary.vec = (n_foods + 1):n_vars)

# 결과 출력 및 검증
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
  cat("\nSelected foods (y_i):\n")
  selected <- solution$solution[(n_foods + 1):n_vars]
  print(foods[selected > 0])
  
  # 영양소 제약 검증
  nutrient_totals <- colSums(nutrient_data * servings)
  nutrient_df <- data.frame(
    Nutrient = nutrients,
    Actual = nutrient_totals,
    Min = min_nutrients,
    Max = max_nutrients
  )
  cat("\nNutrient Requirements Check:\n")
  print(nutrient_df)
  
  # 셀러리/브로콜리 제약 검증
  cat("\nCelery/Broccoli Constraint Check:\n")
  cat("Celery selected:", selected[celery_idx], "\n")
  cat("Broccoli selected:", selected[broccoli_idx], "\n")
  
  # 육류 제약 검증
  cat("\nMeat Constraint Check:\n")
  selected_meat <- foods[selected > 0 & foods %in% meat_group]
  print(selected_meat)
  cat("Number of meat types selected:", length(selected_meat), "\n")
} else {
  cat("Status: No optimal solution found (status:", solution$status, ")\n")
}

# 시각화 1: 식단 막대 그래프
diet_data <- data.frame(Food = foods, Servings = servings)
diet_data <- diet_data[diet_data$Servings > 0, ]
if (nrow(diet_data) > 0) {
  ggplot(diet_data, aes(x = reorder(Food, -Servings), y = Servings)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    coord_flip() +
    labs(title = "Optimal Diet Plan", x = "Food", y = "Servings") +
    theme_minimal()
} else {
  cat("No foods selected, skipping diet plan plot.\n")
}

# 시각화 2: 영양소 충족 여부
nutrient_df_long <- reshape2::melt(nutrient_df, id.vars = "Nutrient", 
                                   variable.name = "Type", value.name = "Value")
ggplot(na.omit(nutrient_df_long), aes(x = Nutrient, y = Value, color = Type)) +
  geom_point(size = 3) +
  geom_line(aes(group = Nutrient), color = "gray", linetype = "dashed") +
  labs(title = "Nutrient Requirements vs Actual", x = "Nutrient", y = "Value") +
  theme_minimal() +
  coord_flip() +
  scale_color_manual(values = c("Actual" = "blue", "Min" = "red", "Max" = "green"))