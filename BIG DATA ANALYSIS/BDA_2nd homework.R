#==================================================================
#
#
# BIG DATA
# Second Homework Assignment
# Matas Mažvila
#
#
#==================================================================
# Load necessary libraries
library(e1071)
library(ggplot2)
library(caret)
library(ISLR)
library(neuralnet)

## Exercise 9.6
## Part a ----

set.seed(248)

# Generate barely linearly separable data using uniform distribution
x1 <- runif(248, -5, 5)
x2 <- runif(248, -5, 5)
y1 <- runif(248, -5, 5)
y2 <- runif(248, -5, 5)

# Combine the data
x <- c(x1, x2)
y <- c(y1, y2)

# Adjust labels based on the condition involving x and y
labels <- numeric(496) # Initialize vector for labels
for (i in 1:496) {
  if (x[i] - y[i] < 0) {
    labels[i] <- 1
  } else {
    labels[i] <- -1
  }
}
labels <- factor(labels) # Convert to factor

# Create a data frame
data <- data.frame(x = x, y = y, class = labels)

# Plot the data
library(ggplot2)
ggplot(data, aes(x = x, y = y, color = class)) + 
  geom_point(size = 3) + 
  labs(title = "Barely Linearly Separable Data", x = "X1", y = "X2") + 
  theme_minimal()

## Part b ----

# Define a range of cost values
cost_values <- c(0.001, 0.01, 0.1, 1, 5, 10, 100, 1000)

# Perform cross-validation
tune_out <- tune(svm, class ~ ., data = data, kernel = "linear",
                 ranges = list(cost = cost_values))
summary(tune_out)

# Extract the best model and cross-validation error rates
cv_errors <- tune_out$performances$error

# Plot the cross-validation error rates
cv_data <- data.frame(cost = tune_out$performances$cost, error = cv_errors)
ggplot(cv_data, aes(x = cost, y = error)) + 
  geom_line() + 
  geom_point() + 
  scale_x_log10() + 
  labs(title = "Cross-Validation Error Rates", x = "Cost", y = "Error Rate") + 
  theme_minimal()

# Extract the number of training observations
num_obs <- nrow(data)

# Compute the number of misclassified observations for each cost value
misclassifications <- cv_errors * num_obs

# Combine cost, error rate, and misclassifications into a data frame
cv_data <- data.frame(cost = tune_out$performances$cost, 
                      error_rate = cv_errors, 
                      misclassifications = misclassifications)

# Print the data frame to see the results
print(cv_data)

# Plot the cross-validation error rates with misclassification counts
ggplot(cv_data, aes(x = cost, y = error_rate)) + 
  geom_line() + 
  geom_point() + 
  scale_x_log10() + 
  labs(title = "Cross-Validation Error Rates and Misclassifications", 
       x = "Cost", y = "Error Rate") + 
  theme_minimal() +
  geom_text(aes(label = round(misclassifications)), vjust = -1)

## Part c ----

# Set the seed for reproducibility
set.seed(2024)

# Generate test data using uniform distribution
x1_test <- runif(248, -5, 5)
x2_test <- runif(248, -5, 5)

# Adjust labels based on the condition involving x1_test and x2_test
labels_test <- numeric(248) # Initialize vector for test labels
for (i in 1:248) {
  if (x1_test[i] - x2_test[i] < 0) {
    labels_test[i] <- 1
  } else {
    labels_test[i] <- -1
  }
}
labels_test <- factor(labels_test) # Convert to factor

# Create a test data frame with adjusted labels
test_data <- data.frame(x = x1_test, y = x2_test, class = labels_test)

# Initialize a vector to store test errors
test_errors <- numeric(length(cost_values))

# Evaluate the model performance for each cost value
for (i in 1:length(cost_values)) {
  svm_model <- svm(class ~ ., data = data, kernel = "linear", cost = cost_values[i])
  predictions <- predict(svm_model, newdata = test_data)
  test_errors[i] <- mean(predictions != test_data$class)
}

# Combine cost and test error rates into a data frame
test_data_frame <- data.frame(cost = cost_values, test_error_rate = test_errors)

# Print the data frame to see the results
print(test_data_frame)

# Plot the test error rates
library(ggplot2)
ggplot(test_data_frame, aes(x = cost, y = test_error_rate)) + 
  geom_line() + 
  geom_point() + 
  scale_x_log10() + 
  labs(title = "Test Error Rates", x = "Cost", y = "Test Error Rate") + 
  theme_minimal()

# Compare test error rates with cross-validation error rates
comparison_data <- data.frame(cost = cost_values, 
                              cv_error_rate = cv_errors[match(cost_values, cost_values)], 
                              test_error_rate = test_errors)

print(comparison_data)

# Plot comparison of error rates
ggplot(comparison_data, aes(x = cost)) + 
  geom_line(aes(y = cv_error_rate, color = "Cross-Validation Error Rate")) + 
  geom_point(aes(y = cv_error_rate, color = "Cross-Validation Error Rate")) + 
  geom_line(aes(y = test_error_rate, color = "Test Error Rate")) + 
  geom_point(aes(y = test_error_rate, color = "Test Error Rate")) + 
  scale_x_log10() + 
  labs(title = "Comparison of Error Rates", x = "Cost", y = "Error Rate") + 
  theme_minimal() + 
  scale_color_manual(values = c("Cross-Validation Error Rate" = "blue", "Test Error Rate" = "red"))

## Part d ----
# In Word file

## Exercise 10.7
set.seed(19800222)

# Load the data
data(Default)

# Convert the default column to a binary factor
Default$default <- ifelse(Default$default == "Yes", 1, 0)

# Split the data into training and testing sets
trainIndex <- createDataPartition(Default$default, p = 0.7, list = FALSE)
trainData <- Default[trainIndex,]
testData <- Default[-trainIndex,]

# Fit a neural network with a single hidden layer of 10 units
nn_model <- neuralnet(default ~ balance + income, data = trainData, hidden = 10, linear.output = FALSE)

# Make predictions on the test set
nn_predictions <- compute(nn_model, testData[, c("balance", "income")])
nn_pred_class <- ifelse(nn_predictions$net.result > 0.5, 1, 0)

# Calculate the accuracy of the neural network
nn_confusion <- confusionMatrix(factor(nn_pred_class), factor(testData$default))
nn_accuracy <- nn_confusion$overall["Accuracy"]
print(paste("Neural Network Accuracy: ", nn_accuracy))

# Fit a logistic regression model
log_model <- glm(default ~ balance + income, data = trainData, family = binomial)

# Make predictions on the test set using logistic regression
log_predictions <- predict(log_model, newdata = testData, type = "response")
log_pred_class <- ifelse(log_predictions > 0.5, 1, 0)

# Calculate the accuracy of the logistic regression model
log_confusion <- confusionMatrix(factor(log_pred_class), factor(testData$default))
log_accuracy <- log_confusion$overall["Accuracy"]
print(paste("Logistic Regression Accuracy: ", log_accuracy))









