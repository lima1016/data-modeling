# 필요한 패키지 로드
install.packages("TSP")
library(TSP)

# 가상 위치 데이터 생성
set.seed(456)
shutoff_candidates$latitude <- rnorm(nrow(shutoff_candidates), mean = 40, sd = 2)
shutoff_candidates$longitude <- rnorm(nrow(shutoff_candidates), mean = -80, sd = 2)

# K-means 클러스터링 (3개 지역 그룹 가정)
kmeans_result <- kmeans(shutoff_candidates[, c("latitude", "longitude")], centers = 3)
shutoff_candidates$cluster <- kmeans_result$cluster

# 클러스터링 결과 시각화
ggplot(shutoff_candidates, aes(x = longitude, y = latitude, color = as.factor(cluster))) +
  geom_point(size = 3) +
  labs(title = "Geographic Clustering of Shutoff Candidates", 
       x = "Longitude", y = "Latitude", color = "Cluster") +
  theme_minimal()

# 간단한 TSP 경로 최적화 (클러스터 1만 예시)
cluster1 <- shutoff_candidates %>% filter(cluster == 1)
dist_matrix <- dist(cluster1[, c("latitude", "longitude")])
tsp <- TSP(dist_matrix)
tour <- solve_TSP(tsp, method = "nearest_insertion")
tour_order <- as.integer(tour)
print(cluster1[tour_order, c("latitude", "longitude")])