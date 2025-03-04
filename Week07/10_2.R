# Hypothetical data
set.seed(123)
data <- data.frame(
  purchase = sample(c(0, 1), 100, replace = TRUE),
  time_spent = rnorm(100, mean = 20, sd = 5),
  pages_visited = rpois(100, lambda = 5),
  cart_additions = sample(c(0, 1), 100, replace = TRUE),
  prev_purchases = rpois(100, lambda = 2),
  ad_clicks = rpois(100, lambda = 3)
)

# Plot relationship between time spent and purchase
ggplot(data, aes(x = time_spent, y = purchase)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), color = "blue") +
  labs(title = "Time Spent vs Purchase Probability", x = "Time Spent (minutes)", y = "Purchase (1 = Yes, 0 = No)") +
  theme_minimal()