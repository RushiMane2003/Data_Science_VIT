# Question 1 ----
# Set working directory
setwd("D:/Data Science")

# Read the Hypothesis.csv file
hypothesis_data <- read.csv("Hypothesis.csv")

# Calculate the number of observations and mean life hours
n1 <- nrow(hypothesis_data)
m1 <- mean(hypothesis_data$Life_Hrs)
sd1 <- sd(hypothesis_data$Life_Hrs)
se1 <- sd1 / sqrt(n1)

# Calculate the p-value using the normal distribution
p1 <- pnorm(m1, 10000, se1)

# Print observed value, standard error, and p-value
cat("Observed value:", m1)
cat("\nStandard error:", se1)
cat("\nP-value:", p1)

# Check significance level 0.05
if (p1 > 0.05) {
  cat("\nClaim should not be rejected (p > 0.05)")
} else {
  cat("\nClaim should be rejected (p <= 0.05)")
}

# Check significance level 0.01
if (p1 > 0.01) {
  cat("\nClaim should not be rejected (p > 0.01)")
} else {
  cat("\nClaim should be rejected (p <= 0.01)")
}

# Question 2 ----
# Define variables for Question 2
m2 <- 134
sd2 <- 17
n2 <- 35
se2 <- sd2 / sqrt(n2)

# Calculate the two-tailed p-value using the normal distribution
p2 <- pnorm(m2, 130, se2, lower.tail = FALSE) * 2

# Print standard error and p-value
cat("\n\nStandard error:", se2)
cat("\nP-value:", p2)

# Check significance level 0.05
if (p2 < 0.05) {
  cat("\nLabel does not provide accurate measures (p < 0.05)")
} else {
  cat("\nLabel provides accurate measures (p >= 0.05)")
}

# Check significance level 0.01
if (p2 < 0.01) {
  cat("\nLabel does not provide accurate measures (p < 0.01)")
} else {
  cat("\nLabel provides accurate measures (p >= 0.01)")
}
