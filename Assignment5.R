# Set working directory
setwd("D:/Data Science")

# Read the knn1.csv file
knn_data <- read.csv("knn1.csv")

# Calculate the Euclidean distance from point (3, 2)
distance <- sqrt((3 - knn_data$x)^2 + (2 - knn_data$y)^2)
knn_data <- cbind(knn_data, distance)

# Sort the data by distance
knn_data <- knn_data[order(knn_data$distance),]

# Find the nearest neighbor
nearest_neighbor_class <- knn_data[1, "class"]
cat("Class of P(3,2) is", nearest_neighbor_class)

# KNN for K=5 ----
# Select the 5 nearest neighbors
k5_neighbors <- knn_data[1:5,]

# Count the classes
class_1_k5 <- sum(k5_neighbors$class == 1)
class_2_k5 <- sum(k5_neighbors$class == 2)
class_3_k5 <- sum(k5_neighbors$class == 3)

# Determine the majority class for K=5
if (class_1_k5 > class_2_k5 & class_1_k5 > class_3_k5) {
  cat("\nClass of P(3,2) for K=5 is 1")
} else if (class_2_k5 > class_1_k5 & class_2_k5 > class_3_k5) {
  cat("\nClass of P(3,2) for K=5 is 2")
} else if (class_3_k5 > class_2_k5 & class_3_k5 > class_1_k5) {
  cat("\nClass of P(3,2) for K=5 is 3")
}

# KNN for K=7 ----
# Select the 7 nearest neighbors
k7_neighbors <- knn_data[1:7,]

# Count the classes
class_1_k7 <- sum(k7_neighbors$class == 1)
class_2_k7 <- sum(k7_neighbors$class == 2)
class_3_k7 <- sum(k7_neighbors$class == 3)

# Determine the majority class for K=7
if (class_1_k7 > class_2_k7 & class_1_k7 > class_3_k7) {
  cat("\nClass of P(3,2) for K=7 is 1")
} else if (class_2_k7 > class_1_k7 & class_2_k7 > class_3_k7) {
  cat("\nClass of P(3,2) for K=7 is 2")
} else if (class_3_k7 > class_2_k7 & class_3_k7 > class_1_k7) {
  cat("\nClass of P(3,2) for K=7 is 3")
}

# Radius-based Nearest Neighbors (RNN) for R=1.45 ----
# Select neighbors within the radius of 1.45
rnn_neighbors <- knn_data[knn_data$distance < 1.45,]

# Count the classes
class_1_rnn <- sum(rnn_neighbors$class == 1)
class_2_rnn <- sum(rnn_neighbors$class == 2)
class_3_rnn <- sum(rnn_neighbors$class == 3)

# Determine the majority class for R=1.45
if (class_1_rnn > class_2_rnn & class_1_rnn > class_3_rnn) {
  cat("\nClass of P(3,2) for R=1.45 is 1")
} else if (class_2_rnn > class_1_rnn & class_2_rnn > class_3_rnn) {
  cat("\nClass of P(3,2) for R=1.45 is 2")
} else if (class_3_rnn > class_2_rnn & class_3_rnn > class_1_rnn) {
  cat("\nClass of P(3,2) for R=1.45 is 3")
}

# Weighted KNN (MKNN) for K=5 ----
# Calculate weights for the 5 nearest neighbors
denominator <- k5_neighbors$distance[5] - k5_neighbors$distance[1]
weights <- (k5_neighbors$distance[5] - k5_neighbors$distance) / denominator
k5_neighbors <- cbind(k5_neighbors, weights)

# Sum the weights for each class
weighted_class_1 <- sum(k5_neighbors$weights[k5_neighbors$class == 1])
weighted_class_2 <- sum(k5_neighbors$weights[k5_neighbors$class == 2])
weighted_class_3 <- sum(k5_neighbors$weights[k5_neighbors$class == 3])

# Determine the majority class for MKNN with K=5
if (weighted_class_1 > weighted_class_2 & weighted_class_1 > weighted_class_3) {
  cat("\nClass of P(3,2) for MKNN with K=5 is 1")
} else if (weighted_class_2 > weighted_class_1 & weighted_class_2 > weighted_class_3) {
  cat("\nClass of P(3,2) for MKNN with K=5 is 2")
} else if (weighted_class_3 > weighted_class_1 & weighted_class_3 > weighted_class_2) {
  cat("\nClass of P(3,2) for MKNN with K=5 is 3")
}
