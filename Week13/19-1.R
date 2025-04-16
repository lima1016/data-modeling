library(tidyverse)
library(ggplot2)

# Fake data
set.seed(123)
data <- tibble(
  product = rep(c("A", "B", "C"), each = 100),
  shelf_space = runif(300, 10, 50),
  sales = 100 + 2 * shelf_space + rnorm(300, 0, 10),
  price = runif(300, 5, 20),
  store_size = runif(300, 1000, 5000)
)

# Regression
model <- lm(sales ~ shelf_space + price + store_size, data = data)
summary(model)

# Plot
ggplot(data, aes(x = shelf_space, y = sales, color = product)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Sales vs. Shelf Space", x = "Shelf Space (sq ft)", y = "Sales (units)") +
  theme_minimal()