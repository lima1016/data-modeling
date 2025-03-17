# Load necessary libraries
library(ggplot2)
library(dplyr)

# Sample data: Query execution times before and after optimization
query_data <- data.frame(
  query_id = 1:10,
  before_optimization = c(1.5, 2.3, 3.1, 2.8, 3.6, 4.2, 2.7, 3.3, 2.9, 3.8),  # in seconds
  after_optimization = c(0.9, 1.5, 1.8, 1.4, 2.0, 2.5, 1.3, 1.8, 1.5, 2.1)     # in seconds
)

# Calculate performance improvement
query_data <- query_data %>%
  mutate(improvement = (before_optimization - after_optimization) / before_optimization * 100)

# Plot execution time before and after optimization
ggplot(query_data, aes(x = query_id)) +
  geom_line(aes(y = before_optimization, color = "Before Optimization"), size = 1) +
  geom_line(aes(y = after_optimization, color = "After Optimization"), size = 1) +
  labs(title = "Query Performance Optimization",
       x = "Query ID",
       y = "Execution Time (seconds)",
       color = "Legend") +
  theme_minimal()

# Print query performance improvements
print(query_data)
