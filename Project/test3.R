library(lpSolve)
# Objective: maximize service value (10 for dining, 15 for shows)
obj <- c(10, 15)
# Constraints: staff hours (20 total), capacity needs
constr <- matrix(c(1, 1, 2, 1), ncol = 2, byrow = TRUE)
rhs <- c(20, 30)
dir <- c("<=", "<=")
result <- lp("max", obj, constr, dir, rhs)
# Visualization (bar plot of staff allocation)
allocation <- result$solution
barplot(allocation, names.arg = c("Dining", "Shows"), 
        ylim = c(0, 20), main = "Optimal Staff Allocation",
        ylab = "Staff Hours", col = "skyblue")
