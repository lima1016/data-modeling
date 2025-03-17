# Load data
data <- read.csv("C:/Users/kimyo/Desktop/LIMA/OMSCS/강의/2025Spring/ISYE6501-IntroductionToAnalyticsModeling/Lecture/Week10/hw10/breast-cancer-wisconsin.data.txt", header = FALSE, na.strings = "?")
colnames(data) <- c("ID", "ClumpThickness", "CellSize", "CellShape", "Adhesion", 
                    "EpithelialSize", "BareNuclei", "Chromatin", "Nucleoli", 
                    "Mitoses", "Class")

# Check missing values
cat("Missing values per column:\n")
print(colSums(is.na(data)))

# 1. Mean/Mode Imputation
mean_impute <- data
mean_impute$BareNuclei[is.na(mean_impute$BareNuclei)] <- mean(mean_impute$BareNuclei, na.rm = TRUE)

# 2. Regression Imputation
regression_impute <- data
model <- lm(BareNuclei ~ ClumpThickness + CellSize + CellShape + Adhesion + 
              EpithelialSize + Chromatin + Nucleoli + Mitoses, data = regression_impute, na.action = na.exclude)
regression_impute$BareNuclei[is.na(regression_impute$BareNuclei)] <- predict(model, regression_impute)[is.na(regression_impute$BareNuclei)]

# 3. Regression with Perturbation
perturbation_impute <- regression_impute
set.seed(123)
noise <- rnorm(sum(is.na(data$BareNuclei)), 0, sd(regression_impute$BareNuclei, na.rm = TRUE) * 0.1)
perturbation_impute$BareNuclei[is.na(data$BareNuclei)] <- regression_impute$BareNuclei[is.na(data$BareNuclei)] + noise

# Visualization
par(mfrow = c(1, 3))
boxplot(mean_impute$BareNuclei, main = "Mean Imputation", ylab = "Bare Nuclei")
boxplot(regression_impute$BareNuclei, main = "Regression Imputation", ylab = "Bare Nuclei")
boxplot(perturbation_impute$BareNuclei, main = "Regression + Perturbation", ylab = "Bare Nuclei")