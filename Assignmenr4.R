# Set working directory
setwd("D:/Data Science")

# Read the Toy_sales.csv file
toy_sales <- read.csv("Toy_sales.csv")

# Simple Linear Regression (SLR) ----
# Fit SLR model
slr_model <- lm(Unitsales ~ Price, data = toy_sales)

# Summarize SLR model
slr_summary <- summary(slr_model)
print(slr_summary)

# Plot SLR model
library(ggplot2)
slr_plot <- ggplot(toy_sales, aes(x = Price, y = Unitsales)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, color = "red", se = FALSE)
print(slr_plot)

# Predicted values and errors for SLR
slr_predicted <- predict(slr_model)
slr_error <- toy_sales$Unitsales - slr_predicted
cat("\nPredicted values for SLR:", slr_predicted)
cat("\nError for SLR:", slr_error)

# Multiple Linear Regression (MLR) ----
# Fit MLR model
mlr_model <- lm(Unitsales ~ Price + Adexp + Promexp, data = toy_sales)

# Summarize MLR model
mlr_summary <- summary(mlr_model)
cat("\n\nSummary for MLR:\n")
print(mlr_summary)

# Create data frame for prediction in MLR
df <- data.frame(
  Price = c(9.1, 8.1),
  Adexp = c(52, 50),
  Promexp = c(61, 60)
)

# Predicted values for MLR
mlr_predicted <- predict(mlr_model, newdata = df)
cat("\nPredicted values for MLR:", mlr_predicted)
