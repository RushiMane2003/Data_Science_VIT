# Pollutant data set ----
# Read the pollutant data from CSV file
pollutant_data <- read.csv("pollutant.csv")

# Calculate the mean temperature for the month of June
mean_temp_june <- mean(pollutant_data$Temp[pollutant_data$Month == 6])
cat("\nThe mean of Temp for month 6:", mean_temp_june)

# Get the number of observations in the dataset
num_observations <- nrow(pollutant_data)
cat("\nNumber of Observations in Given Table are:", num_observations)

# Get the last two rows of the dataset
last_two_rows <- tail(pollutant_data, 2)
cat("\nLast two rows of observation are:")
print(last_two_rows)

# Get the value of Ozone in the 47th row
ozone_value_47th_row <- pollutant_data$Ozone[47]
cat("\nThe value of Ozone in 47th row:", ozone_value_47th_row)

# Count the number of NA values in the Ozone column
num_na_ozone <- sum(is.na(pollutant_data$Ozone))
cat("\nNumber of NA values:", num_na_ozone)

# Calculate the mean of the Ozone column excluding missing values
mean_ozone <- mean(pollutant_data$Ozone, na.rm = TRUE)
cat("\nMean of Ozone column excluding missing values:", mean_ozone)

# Calculate the mean of Solar.R for observations with Ozone > 31 and Temp > 90
mean_solar_subset <- mean(pollutant_data$Solar.R[pollutant_data$Ozone > 31 & pollutant_data$Temp > 90], na.rm = TRUE)
cat("\nThe mean of Solar in the subset:", mean_solar_subset)

# Find the maximum Ozone value in the month of May
max_ozone_may <- max(pollutant_data$Ozone[pollutant_data$Month == 5], na.rm = TRUE)
cat("\nMaximum ozone value in the month of May:", max_ozone_may)

# Hair eye color data set ----
# Read the hair and eye color data from CSV file
hair_eye_data <- read.csv("hair_eye_color.csv")

# Count the number of people with Brown eyes
num_brown_eyes <- sum(hair_eye_data$Eye.Color == "Brown")
cat("\nNumber of Brown eyed people:", num_brown_eyes)

# Count the number of people with Blonde hair
num_blonde_hair <- sum(hair_eye_data$Hair.Color == "Blonde")
cat("\nNumber of Blonde hair people:", num_blonde_hair)

# Count the number of people with Brown hair and Black eyes
num_brown_hair_black_eyes <- sum(hair_eye_data$Hair.Color == "Brown" & hair_eye_data$Eye.Color == "Black")
cat("\nNumber of people with Brown hair and Black eye:", num_brown_hair_black_eyes)

# Get the total number of observations in the dataset
num_observations_hair_eye <- nrow(hair_eye_data)

# Calculate the percentage of people with Green eyes
num_green_eyes <- sum(hair_eye_data$Eye.Color == "Green")
percent_green_eyes <- (num_green_eyes / num_observations_hair_eye) * 100
cat("\nThe percentage of persons having green eyes are:", percent_green_eyes)

# Calculate the percentage of people with Red hair and Blue eyes
num_red_hair_blue_eyes <- sum(hair_eye_data$Hair.Color == "Red" & hair_eye_data$Eye.Color == "Blue")
percent_red_hair_blue_eyes <- (num_red_hair_blue_eyes / num_observations_hair_eye) * 100
cat("\nThe percentage of people have red hair and Blue eye is:", percent_red_hair_blue_eyes)

# Germination Data Set ----
# Read the germination data from CSV file
germination_data <- read.csv("germination.csv")

# Calculate the average number of seeds germinated for uncovered boxes with water_amt = 4
avg_germinated_uncovered <- mean(germination_data$germinated[germination_data$water_amt == 4 & germination_data$Box == "Uncovered"])
cat("\nThe average number of seeds germinated for the uncovered boxes with level of watering equal to 4 is:", avg_germinated_uncovered)

# Calculate the median value of germinated seeds for covered boxes
median_germinated_covered <- median(germination_data$germinated[germination_data$Box == "Covered"])
cat("\nThe median value of germinated seeds for the covered boxes are:", median_germinated_covered)

# Box plot ----
library(ggplot2)

# Create a boxplot for the iris dataset
boxplot_iris <- ggplot(iris, aes(Sepal.Length, Species, fill = Species)) +
  geom_boxplot(outlier.shape = 4, outlier.colour = "red", outlier.size = 3) +
  theme(legend.position = "none") +
  labs(title = "BOXPLOT", x = "Sepal Length", y = "Species") +
  coord_flip()
print(boxplot_iris)

# Scatter plot ----
library(dslabs)

# Create a scatter plot for the murders dataset
scatter_plot_murders <- ggplot(murders, aes(population / 10**6, total)) +
  geom_point(aes(col = region)) +
  scale_x_log10() +
  scale_y_log10() +
  geom_text(aes(label = abb), nudge_x = 0.075)
print(scatter_plot_murders)
