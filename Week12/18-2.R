# "결제 가능성 낮음" 고객 필터링
shutoff_candidates <- customer_data %>% filter(shutoff_candidate == "Yes")

# 우선순위 점수 계산 (가중치 예시: 연체 일수 0.4, 연체 금액 0.3, 결제 가능성 0.3)
shutoff_candidates$overdue_amount <- rnorm(nrow(shutoff_candidates), mean = 200, sd = 50)
shutoff_candidates$priority_score <- with(shutoff_candidates, 
                                          0.4 * days_overdue + 
                                            0.3 * overdue_amount + 
                                            0.3 * (1 - prob_pay))

# 우선순위 순위 매기기
shutoff_candidates <- shutoff_candidates %>% 
  arrange(desc(priority_score)) %>% 
  mutate(rank = row_number())

# 상위 10개 출력
head(shutoff_candidates, 10)

# 시각화: 우선순위 점수 분포
ggplot(shutoff_candidates, aes(x = reorder(rank, -priority_score), y = priority_score)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Priority Scores for Shutoff Candidates", x = "Customer Rank", y = "Priority Score") +
  theme_minimal()