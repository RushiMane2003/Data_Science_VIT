# Set working directory
setwd("D:/Data Science")

# Read the travelled_abroad data from CSV file
travel_data <- read.csv("travelled_abroad.csv")

# Calculate the number of people who travelled abroad
num_travelled_abroad <- sum(travel_data$Travelledabroad == 'Y')

# Get the total number of observations in the dataset
total_observations <- nrow(travel_data)

# Calculate the probability and percentage of people who travelled abroad
prob_travelled_abroad <- num_travelled_abroad / total_observations
percent_travelled_abroad <- prob_travelled_abroad * 100

# Print the probability and percentage
cat("\nProbability:", prob_travelled_abroad)
cat("\nPercentage:", percent_travelled_abroad)

# Calculate the binomial distribution for n = 0 to 10
binom_dist_0_to_10 <- dbinom(0:10, 10, prob_travelled_abroad)

# Plot the binomial distribution for n = 0 to 10
plot(0:10, binom_dist_0_to_10, type = "l")
cat("\nProbability by binomial distribution for n=0 to n=10:", binom_dist_0_to_10)

# Calculate the cumulative probability of at least 59 out of 100 travelling abroad
cumulative_prob_59_to_100 <- sum(dbinom(59:100, 100, prob_travelled_abroad))

# Calculate the binomial distribution for n = 0 to 100
binom_dist_0_to_100 <- dbinom(0:100, 100, prob_travelled_abroad)

# Plot the binomial distribution for n = 0 to 100
plot(0:100, binom_dist_0_to_100, type = "l")
cat("\nProbability by Binomial distribution with n=100:", cumulative_prob_59_to_100)

# Calculate the mean and standard deviation for the binomial distribution
mean_binom <- 100 * prob_travelled_abroad
std_dev_binom <- sqrt(100 * prob_travelled_abroad * (1 - prob_travelled_abroad))

# Print the mean and standard deviation
cat("\nMean:", mean_binom)
cat("\nStandard deviation:", std_dev_binom)

# Calculate the probability using the normal distribution
prob_norm_59 <- pnorm(59, mean_binom, std_dev_binom, lower.tail = FALSE)

# Print the probability by normal distribution
cat("\nProbability by Normal distribution with n=100:", prob_norm_59)
