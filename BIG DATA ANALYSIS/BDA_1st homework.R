#==================================================================
#
#
# HOMEWORK ASSIGNMENT
# BIG Data
#
#
#==================================================================

install.packages("rmarkdown")

# Load the ISRL textbook data
library(ISLR)
fix(data)

## Exercise 3.9 ## ------------------

# Load the Auto dataset
data(Auto)
head(Auto)

## Part a

# Create a scatterplot matrix of all variables in the Auto dataset
pairs(Auto)

## Part b

# Compute the matrix of correlations between the variables
cor(Auto[,-which(names(Auto) == "name")])

## Part c

# Multiple linear regression with mpg as the response
lm_mpg <- lm(mpg ~ . -name, data=Auto)

# Print the summary of the regression
summary(lm_mpg)

## i. Is there a relationship between the predictors and the response?

# A significant F-statistic (with a very small p-value, in this case, less than 2.2e-16)
# indicates that there is a collective relationship between the predictors and the mpg.

## ii. Which predictors appear to have a statistically significant relationship to the response?

# The predictors that appear have a statistically significant relationship to the response
# appear to be displacement, weight, year, and origin. These predictors have p-values that
# are below thresholds for statistical significance (under 0.05).


## iii. What does the coefficient for the year variable suggest?

# The coefficient for the year variable is positive and statistically significant.
# This suggests that, on average, the mpg of a car increases by approximately 0.75
# units for each one-year increase, holding all other predictors constant.

## Part d

# Produce the diagnostic plots
par(mfrow=c(2, 2)) # Set up the graphics layout to show 4 plots
plot(lm_mpg)

# Residuals vs Fitted: The plot indicates that there might be non-linear relationships in the data.
# It might be a good idea to run a non-linear regression and see if it fits the model better.

# Normal Q-Q: The residuals mostly follow the straight line, suggesting that the normality
# assumption is reasonable. However, there are deviations at the upper tail, indicating
# potential outliers.

# Residuals vs Leverage Plot: Most data points are clustered towards the bottom left corner,
# which is ideal. However, there are a few points with higher leverage, notably point 14.

## Part e

# Fit linear regression with interaction effects
lm_mpg_interaction <- lm(mpg ~ . - name + weight:displacement + acceleration:horsepower, data = Auto)
summary(lm_mpg_interaction)

# Both interaction ('weight:displacement' and 'acceleration:horsepower') effects are highly statistically significant.
# With these interaction effects added to the model, there is an increase in adjusted R-squared value from 0.82 to 0.86, meaning
# that more of the variability in mpg is explained by updated model.

## Part f

# Log transformation of displacement
lm_mpg_log <- lm(mpg ~ . - name + log(displacement) + weight:displacement + acceleration:horsepower, data = Auto)
summary(lm_mpg_log)

# Square root transformation of weight
lm_mpg_sqrt <- lm(mpg ~ . - name + sqrt(weight) + weight:displacement + acceleration:horsepower, data = Auto)
summary(lm_mpg_sqrt)

# Squared transformation of acceleration and cylinders
lm_mpg_squared <- lm(mpg ~ . - name + I(acceleration^2) + I(cylinders^2) + weight:displacement + acceleration:horsepower, data = Auto)
summary(lm_mpg_squared)

# Mixed polynomial with transformations to weight, cylinders, displacement, and horsepower
lm_mpg_mixed <- lm(mpg ~ . - name + log(weight) + I(cylinders^2) + sqrt(displacement) + sqrt(horsepower) + weight:displacement + acceleration:horsepower, data = Auto)
summary(lm_mpg_mixed)

# In terms of the adjusted R-squared, there is not much change in neither of the fours models with log, square root, or square transformations
# as in all of the models the metric fluctuates around 0.86 just a little bit. However, I noticed that in most cases there has been at least some
# loss in terms of statistical significance in the variables. The transformations I made were mostly experimental and more rigorous qualitative analysis 
# is needed to find a more decent model.

## Exercise 5.8 ## ------------------

## Part a

# Generate a simulated data set
set.seed(1)
x=rnorm(100)
y=x-2*x^2+rnorm(100)

# n - number of observations. n = 100
# p - number of predictors. p = 2
# The model: y = x − 2x^2 + ϵ

## Part b
plot(x,y)

# From the plot it is clearly visible that the relationship between X and Y is non-linear,
# it seems to be quadratic.

## Part c

# Set the seed
set.seed(20240326)

# Simulate the data
x <- rnorm(100)
y <- x - 2 * x^2 + rnorm(100)

# Create a data frame
data_frame <- data.frame(x = x, y = y)

# Placeholder for storing cross-validation errors
cross_val_errors <- numeric(4)

# Load the boot package
library(boot)

# Looping through polynomial degrees 1 to 4
for (deg in 1:4) {
  # Fitting the model with current degree
  model_fit <- glm(y ~ poly(x, deg, raw = TRUE), data = data_frame)
  
  # Using cv.glm for LOOCV
  cv_result <- cv.glm(data_frame, model_fit, K = nrow(data_frame))
  
  # Storing the LOOCV error
  cross_val_errors[deg] <- cv_result$delta[1]
}

# Printing the cross-validation errors for each degree
print(cross_val_errors)

# The results show us the MSE's of a linear regression and polynomials from degree
# 2 to 4. We can see a sharp drop of the error in the polynomials (this confirms that
# our data quadratic), which is then increasing with polynomial degree, indicating
# that quadratic polynomial is the best fit for the data out of tested models.

## Part d

# Set another seed
set.seed(20020909)

# Simulate the data
x <- rnorm(100)
y <- x - 2 * x^2 + rnorm(100)

# Create a data frame
data_frame <- data.frame(x = x, y = y)

# Placeholder for storing cross-validation errors
cross_val_errors <- numeric(4)

# Looping through polynomial degrees 1 to 4
for (deg in 1:4) {
  # Fitting the model with current degree
  model_fit <- glm(y ~ poly(x, deg, raw = TRUE), data = data_frame)
  
  # Using cv.glm for LOOCV
  cv_result <- cv.glm(data_frame, model_fit, K = nrow(data_frame))
  
  # Storing the LOOCV error
  cross_val_errors[deg] <- cv_result$delta[1]
}

# Printing the cross-validation errors for each degree
print(cross_val_errors)

# While the exact MSE values vary due to the randomness in the data generation,
# the relative performance of the models remains consistent. The linear model is 
# still a large outlier with the biggest MSE. The quadratic model outperforms other models 
# suggesting that it most accurately captures the relationship between x and y.

## Part e

# The quadratic model has the smallest LOOCV error. In this case, the quadratic model's
# lowest error suggests that it generalizes best compared to the other models, fitting
# closely to the true relationship. This is consistent with the data generation process
# y = x − 2x^2 + ϵ (the relationship between x and y is quadratic).

## Part f

# Summaries of the models
summary(glm(y~poly(x,degree=1,raw=TRUE)))
summary(glm(y~poly(x,degree=2,raw=TRUE)))
summary(glm(y~poly(x,degree=3,raw=TRUE)))
summary(glm(y~poly(x,degree=4,raw=TRUE)))

# Polynomial of the 1st degree: In the linear regression, the coefficient is highly statistical significant

# Polynomial of the 2nd degree: In the quadratic model both coefficients are highly statistical significant

# Polynomial of the 3rd degree: In the cubic regression, the first two coefficients are highly
# statistical significant, while the 3rd one is insignificant.

# Polynomial of the 4th degree: In the quartic regression, the first two coefficients are
# highly statistical significant, while the other two are not.

# These results clearly agree with the conclusions drawn in the cross-validation analysis,
# the quadratic polynomial is the best fit.
