#==================================================================
#
# MIDTERM EXAM
# BIG Data
# 2024-03-28
#
#==================================================================

## Problem 1 ## ----

# Defining the data
observations <- data.frame(
  x1 = c(2, 1, 1, 5, 3, 0, 5),
  x2 = c(2, 0, 2, 2, 2, 1, 1),
  x3 = c(2, 5, 3, 3, 8, 4, 0),
  y = c("Success", "Failure", "Failure", "Success", "Failure", "Failure", "Success")
)

new_product <- c(1, 2, 3)

# Function to calculate Manhattan distance
manhattan_distance <- function(a, b) {
  sum(abs(a - b))
}

# Function to calculate Euclidean distance
euclidean_distance <- function(a, b) {
  sqrt(sum((a - b)^2))
}

# Calculating distances
observations$manhattan <- apply(observations[1:3], 1, manhattan_distance, b = new_product)
observations$euclidean <- apply(observations[1:3], 1, euclidean_distance, b = new_product)

# Preview the updated data with distances
print(observations)

## Part a

# Predict function based on K-nearest neighbors
predict_knn <- function(data, k, distance_col = "manhattan") {
  sorted_data <- data[order(data[[distance_col]]), ]
  top_k <- head(sorted_data, k)
  return(names(sort(table(top_k$y), decreasing = TRUE))[1])
}

# Predictions for K = 3 and K = 5 using Manhattan distance
prediction_k3_manhattan <- predict_knn(observations, 3, "manhattan")
prediction_k5_manhattan <- predict_knn(observations, 5, "manhattan")

# Predictions for K = 3 and K = 5 using Euclidean distance
prediction_k3_euclidean <- predict_knn(observations, 3, "euclidean")
prediction_k5_euclidean <- predict_knn(observations, 5, "euclidean")

# Both Manhattan and Euclidean distances yield the same predictions for both 
# K=3 and K=5, with the new product being predicted as a "Failure" in both cases.
# This suggests that based on the nearest neighbors in the dataset, regardless of
# the distance metric or the number of neighbors considered, the predicted outcome
# for the new product remains consistent as "Failure."

## Part b
# Higher Variance: Expected with lower K values (e.g., K=3), because the model is more flexible
# and more closely follows the specific details (and noise) of the training data.

# Higher Bias: Expected with higher K values (e.g., K=5), because the model is less flexible,
# potentially oversimplifying the true underlying patterns in the data by averaging more neighbors' responses.

## Problem 3 ## ----

# Part a

# Define the parameters
beta_0 <- 1
beta_1 <- 1
beta_2 <- -1

# Create a sequence of X values from -4 to 4
X <- seq(-4, 4, by = 0.01)

# Compute the Y values based on the model
Y <- beta_0 + beta_1 * X + beta_2 * (X - 2)^2 * ifelse(X >= 2, 1, 0)

# Plot the curve
plot(X, Y, type = "l", main = "Estimated Curve A",
     xlab = "X", ylab = "Y",
     col = "blue", lwd = 2)

# Add a vertical line at X=2 to show the piecewise function change
abline(v = 2, col = "red", lty = 2)

# Add labels to indicate the piecewise sections and slopes
text(x = 0, y = beta_0, labels = paste("Intercept =", beta_0))
text(x = 2, y = beta_0 + beta_1 * 2, labels = "Kink at X=2")

# Add a legend to denote the slopes
legend("topright", legend=paste("Slope for X<2:", beta_1), bty="n")

# Part b

# Define the function for model B
model_B <- function(x) {
  if (0 <= x & x <= 1) {
    return(beta_0 + beta_1)
  } else if (1 < x & x <= 2) {
    return(beta_0 + beta_1 * (1 - (x - 1)))
  } else if (2 < x & x <= 3) {
    return(beta_0)
  } else if (3 <= x & x <= 4) {
    return(beta_0 + beta_2 * (x - 3))
  } else if (4 < x & x <= 5) {
    return(beta_0 + beta_2)
  } else {
    return(beta_0)
  }
}

# Generate X values and compute corresponding Y values
X <- seq(-2, 6, by = 0.01)
Y <- sapply(X, model_B)

# Plot the estimated curve B
plot(X, Y, type = "l", col = "blue", xlab = "X", ylab = "Y", main = "Estimated Curve B")
abline(v = c(1, 2, 3, 4, 5), col = "red", lty = 2)  # Adding vertical lines for transition points

# Add labels to indicate the intercepts and slopes
text(x = 0.5, y = beta_0 + beta_1, labels = paste("Intercept 0-1 =", beta_0 + beta_1))
text(x = 1.5, y = beta_0 + 0.5, labels = "Decreasing slope 1-2")
text(x = 3.5, y = beta_0 + beta_2, labels = paste("Slope 3-4:", beta_2))
text(x = 4.5, y = beta_0 + beta_2, labels = paste("Intercept 4-5:", beta_0 + beta_2))

