####----------- TASK 2 -----------####

## Installing necessary packages
install.packages("readr")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("xts")
install.packages("tseries")
install.packages("forecast")
library(ggplot2)
library(readr)
library(lubridate)
library(xts)
library(tseries)
library(forecast)

### Exercise 1 ###----------

irs2020 <- read.csv2("IRS2020.csv")
View(irs2020)

### Part a ###----

## Converting all the amount variables into dollar amount
irs2020$TotalIncome <- as.numeric(irs2020$TotalIncome)*1000
irs2020$Salaries <- as.numeric(irs2020$Salaries)*1000
irs2020$Divis <- as.numeric(irs2020$Divis)*1000
irs2020$Retax <- as.numeric(irs2020$Retax)*1000

## Descriptive analysis of all amount variables in dollars
descriptive_analysis <- matrix(c(min(irs2020$TotalIncome), max(irs2020$TotalIncome), mean(irs2020$TotalIncome), median(irs2020$TotalIncome), sd(irs2020$TotalIncome),
                    min(irs2020$Salaries), max(irs2020$Salaries), mean(irs2020$Salaries), median(irs2020$Salaries), sd(irs2020$Salaries),
                    min(irs2020$Divis), max(irs2020$Divis), mean(irs2020$Divis), median(irs2020$Divis), sd(irs2020$Divis),
                    min(irs2020$Retax), max(irs2020$Retax), mean(irs2020$Retax), median(irs2020$Retax), sd(irs2020$Retax)),
                  nrow = 5, ncol = 4)

## Row and column names
colnames(descriptive_analysis) <- c("TotalIncome", "Salaries", "Dividends", "Retax")
rownames(descriptive_analysis) <- c("Min", "Max", "Mean", "Median", "SD")

## The table
descriptive_analysis

### Part b ###----

## Household (HH) averages per ZIP code
irs2020$NumofReturns <- as.numeric(irs2020$NumofReturns)
hh_averages <- aggregate((irs2020$TotalIncome/1000)/irs2020$NumofReturns ~ ZIPCODE + STATE, data = irs2020, FUN = mean)
irs2020$hh_average <- with(irs2020, (TotalIncome/1000)/NumofReturns)
colnames(hh_averages)[3] <- "hh_average"

## Salary income averages by ZIP code
salary_averages <- aggregate((Salaries/1000)/NumofReturns ~ ZIPCODE + STATE, data = irs2020, FUN = mean)
irs2020$salary_average <- with(irs2020, (Salaries/1000)/NumofReturns)
colnames(salary_averages)[3] <- "salary_average"

## Dividend averages by ZIP code (for households that have dividends)
irs2020$dividend_average <- with(irs2020, as.numeric(Divis)/as.numeric(NumofRetwithDivis)/1000)
dividend_averages <- aggregate(as.numeric(Divis)/as.numeric(NumofRetwithDivis)/1000 ~ ZIPCODE + STATE, 
     data = subset(irs2020, NumofRetwithDivis > 0 & Divis > 0), FUN = mean)
colnames(dividend_averages)[3] <- "dividend_average"

## Real estate tax averages by ZIP code (for households that pay real estate taxes)
irs2020$retax_average <- with(irs2020, as.numeric(Retax)/as.numeric(NumofRetwithRE)/1000)
retax_averages <- aggregate(as.numeric(Retax)/as.numeric(NumofRetwithRE)/1000 ~ ZIPCODE + STATE, 
     data = subset(irs2020, NumofRetwithRE > 0 & Retax > 0), FUN = mean)
colnames(retax_averages)[3] <- "retax_average"


## Boxplot of income averages
ggplot(data = hh_averages, aes(y = hh_average)) +
  geom_boxplot() +
  labs(title = "Distribution of Household Income Averages", y="Average Income (in thousands of $)") +
  ylim(0, 300)
  
  
## Boxplot of salary averages
ggplot(data = salary_averages, aes(y = salary_average)) +
  geom_boxplot() +
  labs(title = "Distribution of Household Salary Averages", y="Average Salary (in thousands of $)") +
  ylim(0, 190)

## Boxplot of dividend averages
ggplot(data = dividend_averages, aes(y = dividend_average)) +
  geom_boxplot() +
  labs(title = "Distribution of Household Dividend Averages", y="Average Dividends (in thousands of $)") +
  ylim(0, 50)

## Boxplot of real estate tax averages
ggplot(data = retax_averages, aes(y = retax_average)) +
  geom_boxplot() +
  labs(title = "Distribution of Household Retax Averages", y="Average Retax (in thousands of $)") +
  ylim(0, 30)

## Density plot for income averages
ggplot(hh_averages, aes(x = hh_average)) +
  geom_density() +
  labs(title = "Density Plot of Income Averages",
       x = "Average Income (in thousands of $)",
       y = "Density",) + xlim(0, 300)

## Density plot for salary averages
ggplot(salary_averages, aes(x = salary_average)) +
  geom_density() +
  labs(title = "Density Plot of Salary Averages",
       x = "Average Salary (in thousands of $)",
       y = "Density",) + xlim(0, 300)

## Density plot for dividend averages
ggplot(dividend_averages, aes(x = dividend_average)) +
  geom_density() +
  labs(title = "Density Plot of Dividend Averages",
       x = "Average Dividends (in thousands of $)",
       y = "Density",) + xlim(0, 75)

## Density plot for retax averages
ggplot(retax_averages, aes(x = retax_average)) +
  geom_density() +
  labs(title = "Density Plot of Retax Averages",
       x = "Average Retax (in thousands of $)",
       y = "Density",) + xlim(0, 60)

## Mean and median values from each data frame
hh_mean <- mean(hh_averages$hh_average)
hh_median <- median(hh_averages$hh_average)

salary_mean <- mean(salary_averages$salary_average)
salary_median <- median(salary_averages$salary_average)

dividend_mean <- mean(dividend_averages$dividend_average)
dividend_median <- median(dividend_averages$dividend_average)

retax_mean <- mean(retax_averages$retax_average)
retax_median <- median(retax_averages$retax_average)

## Data frame
table_b <- c("AVG Income", "AVG Salaries", "AVG Dividends", "AVG Real Estate Tax")
mean_b <- c(hh_mean, salary_mean, dividend_mean, retax_mean)
median_b <- c(hh_median, salary_median, dividend_median, retax_median)

df <- data.frame(table_b, mean_b, median_b)

# The table that reports mean and median values in thousands of $
df

## Conclusions
# The boxplots show us the median, quartiles and outliers. Presence of the far-reaching 
# outliers suggests a sizable income and salary inequality in the society.
# We can see from the density plots that the income, salary, dividend and retax averages
# are positively distributed because all the density curves ar right-skewed. The long
# tail to the right indicates that there are large outliers (households that have big average
# values of income, salaries, dividens or retax).
# Lastly, the table with means and medians proves that data is positively distributed
# right-skewed since mean in all cases is higher than median.

### Part c ###----

## A data frame with ZIP codes and highest and lowest values of average HH income, HH salaries, HH dividends, and HH real estate taxes paid.
hh_values <- data.frame(table_c = c("AVG Income", "AVG Salaries", "AVG Dividends", "AVG Real Estate Tax Paid"),
                        highest_zip = c(hh_averages$ZIPCODE[which.max(hh_averages$hh_average)],
                                        salary_averages$ZIPCODE[which.max(salary_averages$salary_average)],
                                        dividend_averages$ZIPCODE[which.max(dividend_averages$dividend_average)],
                                        retax_averages$ZIPCODE[which.max(retax_averages$retax_average)]),
                        highest_value = c(max(hh_averages$hh_average),
                                          max(salary_averages$salary_average),
                                          max(dividend_averages$dividend_average),
                                          max(retax_averages$retax_average)),
                        lowest_zip = c(hh_averages$ZIPCODE[which.min(hh_averages$hh_average)],
                                       salary_averages$ZIPCODE[which.min(salary_averages$salary_average)],
                                       dividend_averages$ZIPCODE[which.min(dividend_averages$dividend_average)],
                                       retax_averages$ZIPCODE[which.min(retax_averages$retax_average)]),
                        lowest_value = c(min(hh_averages$hh_average),
                                         min(salary_averages$salary_average),
                                         min(dividend_averages$dividend_average),
                                         min(retax_averages$retax_average)),
                        highest_state = c(hh_averages$STATE[which.max(hh_averages$hh_average)],
                                          salary_averages$STATE[which.max(salary_averages$salary_average)],
                                          dividend_averages$STATE[which.max(dividend_averages$dividend_average)],
                                          retax_averages$STATE[which.max(retax_averages$retax_average)]),
                        lowest_state = c(hh_averages$STATE[which.min(hh_averages$hh_average)],
                                         salary_averages$STATE[which.min(salary_averages$salary_average)],
                                         dividend_averages$STATE[which.min(dividend_averages$dividend_average)],
                                         retax_averages$STATE[which.min(retax_averages$retax_average)]))

## The data frame
hh_values

# Given the US states these ZIP codes are in, most of the results don't surprise me.
# For example, the biggest income and salary averages lie in states such as Florida,
# California, New York, Washington which could be explained by high income inequality
# in these states and other variables like GDP and GDP per capita. There is a
# little more diversity in dividend averages. Minimum values of these three variables
# seem more random. Concerning real estate taxes we can see the same states
# with the most of the highest values which could be explained by high
# income and people's wealth there, also, by comparatively high real estate taxes.
# Meanwhile, Alabama has the lowest values because real estate taxes there
# are nearly the lowest in the US.


### Part d ###----

# The fraction of households receiving dividends
irs2020$NumofRetwithDivis <- as.numeric(irs2020$NumofRetwithDivis) 
irs2020$NumofReturns <- as.numeric(irs2020$NumofReturns)

irs2020$DivFraction <- irs2020$NumofRetwithDivis / irs2020$NumofReturns

## Summary of the statistical properties
summary(irs2020$DivFraction)


## Density plot of Divividend Fraction
ggplot(irs2020, aes(x = DivFraction)) +
  geom_density() +
  labs(title = "Density Plot",
       x = "Fraction of Dividends",
       y = "Density")

## The distribution of the fraction of households receiving dividends (DivFraction)
# is right-skewed with and ranges from 0 to 0.848. The median value is 0.155, and the
# mean value is 0.172. Moreover, from the 3rd quartile it is clear that the majority
# of the households receive up to 0.227 of their total income from dividends.

## Scatterplot of HH income and fraction of HH receiving dividends
ggplot(irs2020, aes(x = hh_average, y = DivFraction)) +
  geom_point() +
  labs(title = "Scatterplot of Income and received Dividend Fraction",
       x = "AVG Income $",
       y = "Fraction of Households Receiving Dividends") + xlim(0, 1000)

## Scatterplot with colors by US state
ggplot(irs2020, aes(x = hh_average, y = DivFraction, color = STATE)) +
  geom_point() +
  labs(title = "HH Income and received Dividend Fraction by State",
       x = "AVG Income $",
       y = "Fraction of Households Receiving Dividends") + xlim(0, 1000)

### Part e ###----

## Regression of HH income on fraction of HH receiving dividends
regression <- lm(hh_average ~ DivFraction, data = irs2020)


## Scatterplot with the regression line
ggplot(irs2020, aes(x = DivFraction, y = hh_average)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, color = "darkolivegreen4") +
  labs(title = "Regression of HH Average on Dividend Fraction",
       x = "Fraction of Households Receiving Dividends",
       y = "HH Average Income (in thousands $)") + ylim(0, 1000)

## Summary of the model
summary(regression)

## Plot of the residuals
plot(residuals(regression), ylim = c(-500, 3000))

## Conclusions
# The model has adjusted R-squared = 0.3565 which means that the model explains
# 35.65% of the variance in the response variable DivFraction which doesn't seem
# very high. On the other hand,t he residuals lie around 0 which indicates that
# model is unbiased and has a good fit.

### Part f ###----

irs2019 <- read.csv2("IRS2019.csv")
View(irs2019)

## Part f_a###----

irs2019$TotalIncome <- as.numeric(irs2019$TotalIncome)*1000
irs2019$Salaries <- as.numeric(irs2019$Salaries)*1000
irs2019$Divis <- as.numeric(irs2019$Divis)*1000
irs2019$Retax <- as.numeric(irs2019$Retax)*1000

descriptive_analysis_f <- matrix(c(min(irs2019$TotalIncome), max(irs2019$TotalIncome), mean(irs2019$TotalIncome), median(irs2019$TotalIncome), sd(irs2019$TotalIncome),
                                 min(irs2019$Salaries), max(irs2019$Salaries), mean(irs2019$Salaries), median(irs2019$Salaries), sd(irs2019$Salaries),
                                 min(irs2019$Divis), max(irs2019$Divis), mean(irs2019$Divis), median(irs2019$Divis), sd(irs2019$Divis),
                                 min(irs2019$Retax), max(irs2019$Retax), mean(irs2019$Retax), median(irs2019$Retax), sd(irs2019$Retax)),
                               nrow = 5, ncol = 4)

## Row and column names
colnames(descriptive_analysis_f) <- c("TotalIncome", "Salaries", "Dividends", "Retax")
rownames(descriptive_analysis_f) <- c("Min", "Max", "Mean", "Median", "SD")

## The table
descriptive_analysis_f

## Part f_b###----

## Household (HH) averages per ZIP code
irs2019$NumofReturns <- as.numeric(irs2019$NumofReturns)
hh_averages_f <- aggregate((irs2019$TotalIncome/1000)/irs2019$NumofReturns ~ ZIPCODE + STATE, data = irs2019, FUN = mean)
irs2019$hh_average <- with(irs2019, (TotalIncome/1000)/NumofReturns)
colnames(hh_averages_f)[3] <- "hh_average"

## Salary income averages by ZIP code
salary_averages_f <- aggregate((Salaries/1000)/NumofReturns ~ ZIPCODE + STATE, data = irs2019, FUN = mean)
irs2019$salary_average <- with(irs2019, (Salaries/1000)/NumofReturns)
colnames(salary_averages_f)[3] <- "salary_average"

## Dividend averages by ZIP code (for households that have dividends)
irs2019$dividend_average <- with(irs2019, as.numeric(Divis)/as.numeric(NumofRetwithDivis)/1000)
dividend_averages_f <- aggregate(as.numeric(Divis)/as.numeric(NumofRetwithDivis)/1000 ~ ZIPCODE + STATE,
                               data = subset(irs2019, NumofRetwithDivis > 0 & Divis > 0), FUN = mean)
colnames(dividend_averages_f)[3] <- "dividend_average"

## Real estate tax averages by ZIP code (for households that pay real estate taxes)
irs2019$retax_average <- with(irs2019, as.numeric(Retax)/as.numeric(NumofRetwithRE)/1000)
retax_averages_f <- aggregate(as.numeric(Retax)/as.numeric(NumofRetwithRE)/1000 ~ ZIPCODE + STATE,
                            data = subset(irs2019, NumofRetwithRE > 0 & Retax > 0), FUN = mean)
colnames(retax_averages_f)[3] <- "retax_average"


## Boxplot of income averages
ggplot(data = hh_averages_f, aes(y = hh_average)) +
  geom_boxplot() +
  labs(title = "Distribution of Household Income Averages", y="Average Income (in thousands of $)") +
  ylim(0, 300)

## Boxplot of salary averages
ggplot(data = salary_averages_f, aes(y = salary_average)) +
  geom_boxplot() +
  labs(title = "Distribution of Household Salary Averages", y="Average Salary (in thousands of $)") +
  ylim(0, 190)

## Boxplot of dividend averages
ggplot(data = dividend_averages_f, aes(y = dividend_average)) +
  geom_boxplot() +
  labs(title = "Distribution of Household Dividend Averages", y="Average Dividends (in thousands of $)") +
  ylim(0, 50)

## Boxplot of real estate tax averages
ggplot(data = retax_averages_f, aes(y = retax_average)) +
  geom_boxplot() +
  labs(title = "Distribution of Household Retax Averages", y="Average Retax (in thousands of $)") +
  ylim(0, 30)

## Density plot for income averages
ggplot(hh_averages_f, aes(x = hh_average)) +
  geom_density() +
  labs(title = "Density Plot of Income Averages",
       x = "Average Income (in thousands of $)",
       y = "Density",) + xlim(0, 300)

## Density plot for salary averages
ggplot(salary_averages_f, aes(x = salary_average)) +
  geom_density() +
  labs(title = "Density Plot of Salary Averages",
       x = "Average Salary (in thousands of $)",
       y = "Density",) + xlim(0, 300)

## Density plot for dividend averages
ggplot(dividend_averages_f, aes(x = dividend_average)) +
  geom_density() +
  labs(title = "Density Plot of Dividend Averages",
       x = "Average Dividends (in thousands of $)",
       y = "Density",) + xlim(0, 75)

## Density plot for retax averages
ggplot(retax_averages_f, aes(x = retax_average)) +
  geom_density() +
  labs(title = "Density Plot of Retax Averages",
       x = "Average Retax (in thousands of $)",
       y = "Density",) + xlim(0, 60)


## Mean and median values from each data frame
hh_mean_f <- mean(hh_averages_f$hh_average)
hh_median_f <- median(hh_averages_f$hh_average)

salary_mean_f <- mean(salary_averages_f$salary_average)
salary_median_f <- median(salary_averages_f$salary_average)

dividend_mean_f <- mean(dividend_averages_f$dividend_average)
dividend_median_f <- median(dividend_averages_f$dividend_average)

retax_mean_f <- mean(retax_averages_f$retax_average)
retax_median_f <- median(retax_averages_f$retax_average)


## Data frame
table_b_f <- c("AVG Income", "AVG Salaries", "AVG Dividends", "AVG Real Estate Tax")
mean_b_f <- c(hh_mean_f, salary_mean_f, dividend_mean_f, retax_mean_f)
median_b_f <- c(hh_median_f, salary_median_f, dividend_median_f, retax_median_f)

df_f <- data.frame(table_b_f, mean_b_f, median_b_f)

# The table that reports mean and median values in thousands of $
df_f


## Part f_c###----

## A data frame with ZIP codes and highest and lowest values of average HH income, HH salaries, HH dividends, and HH real estate taxes paid.
hh_values_f <- data.frame(table_f = c("AVG Income", "AVG Salaries", "AVG Dividends", "AVG Real Estate Tax Paid"),
                        highest_zip = c(hh_averages_f$ZIPCODE[which.max(hh_averages_f$hh_average)],
                                        salary_averages_f$ZIPCODE[which.max(salary_averages_f$salary_average)],
                                        dividend_averages_f$ZIPCODE[which.max(dividend_averages_f$dividend_average)],
                                        retax_averages_f$ZIPCODE[which.max(retax_averages_f$retax_average)]),
                        highest_value = c(max(hh_averages_f$hh_average),
                                          max(salary_averages_f$salary_average),
                                          max(dividend_averages_f$dividend_average),
                                          max(retax_averages_f$retax_average)),
                        lowest_zip = c(hh_averages_f$ZIPCODE[which.min(hh_averages_f$hh_average)],
                                       salary_averages_f$ZIPCODE[which.min(salary_averages_f$salary_average)],
                                       dividend_averages_f$ZIPCODE[which.min(dividend_averages_f$dividend_average)],
                                       retax_averages_f$ZIPCODE[which.min(retax_averages_f$retax_average)]),
                        lowest_value = c(min(hh_averages_f$hh_average),
                                         min(salary_averages_f$salary_average),
                                         min(dividend_averages_f$dividend_average),
                                         min(retax_averages_f$retax_average)),
                        highest_state = c(hh_averages_f$STATE[which.max(hh_averages_f$hh_average)],
                                          salary_averages_f$STATE[which.max(salary_averages_f$salary_average)],
                                          dividend_averages_f$STATE[which.max(dividend_averages_f$dividend_average)],
                                          retax_averages_f$STATE[which.max(retax_averages_f$retax_average)]),
                        lowest_state = c(hh_averages_f$STATE[which.min(hh_averages_f$hh_average)],
                                         salary_averages_f$STATE[which.min(salary_averages_f$salary_average)],
                                         dividend_averages_f$STATE[which.min(dividend_averages_f$dividend_average)],
                                         retax_averages_f$STATE[which.min(retax_averages_f$retax_average)]))
## The data frame
hh_values_f

## Part f_d###----

# The fraction of households receiving dividends
irs2019$NumofRetwithDivis <- as.numeric(irs2019$NumofRetwithDivis)
irs2019$NumofReturns <- as.numeric(irs2019$NumofReturns)

irs2019$DivFraction <- irs2019$NumofRetwithDivis / irs2019$NumofReturns

## Summary of the statistical properties
summary(irs2019$DivFraction)


## Density plot of Divividend Fraction
ggplot(irs2019, aes(x = DivFraction)) +
  geom_density() +
  labs(title = "Density Plot",
       x = "Fraction of Dividends",
       y = "Density")

## Scatterplot of HH income and fraction of HH receiving dividends
ggplot(irs2019, aes(x = hh_average, y = DivFraction)) +
  geom_point() +
  labs(title = "Scatterplot of Income and received Dividend Fraction",
       x = "AVG Income $",
       y = "Fraction of Households Receiving Dividends") + xlim(0, 1000)

## Scatterplot with colors by US state
ggplot(irs2019, aes(x = hh_average, y = DivFraction, color = STATE)) +
  geom_point() +
  labs(title = "HH Income and received Dividend Fraction by State",
       x = "AVG Income $",
       y = "Fraction of Households Receiving Dividends") + xlim(0, 1000)


## Part f_e###----

## Regression of HH income on fraction of HH receiving dividends
regression_f <- lm(hh_average ~ DivFraction, data = irs2019)

## Scatterplot with the regression line
ggplot(irs2019, aes(x = DivFraction, y = hh_average)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, color = "cornflowerblue") +
  labs(title = "Regression of HH Average on Dividend Fraction",
       x = "Fraction of Households Receiving Dividends",
       y = "HH Average Income (in thousands $)") + ylim(0, 1000)

## Summary of the model
summary(regression_f)

## Plot of the residuals
plot(residuals(regression_f), ylim = c(-500, 3000))


## Part f conclusions----
# From the data (`df` and `df_f`), we can see that the mean and median values for all variables
# have increased from 2019 to 2020, except for dividends and median salary value.
# The largest increase was in income. Salaries and real estate tax increased by
# a small margin. The increase in income and salaries may indicate economic growth
# or changes in tax policies.
# From the data (`hh_values` and `hh_values_f`), we can see that highest values
# for income and salaries increased quite a lot while dividends and real estate
# taxes decreased, none of the ZIP codes for highest values changed. On the other
# hand, lowest income and salaries decreased while dividends and real estate taxes,
# increased. States for lowest values seem random with an exception of Alabama
# (real estate tax there is almost the lowest in US). The difference between highest
# and lowest incomes and salaries increased which suggests increased income inequality.


### Exercise 2 ###----------

history <- read.csv2("VIX_History.csv", sep = ",")
View(history)

### Part g ###----

history$DATE <- mdy(history$DATE)
history$HIGH <- as.numeric(history$HIGH)
history$OPEN <- as.numeric(history$OPEN)
history$LOW <- as.numeric(history$LOW)
history$CLOSE <- as.numeric(history$CLOSE)

## Plot of the 4 series

ggplot(history, aes(x = DATE)) +
  geom_line(aes(y = OPEN), color = "navyblue") +
  geom_line(aes(y = HIGH), color = "mediumaquamarine") +
  geom_line(aes(y = LOW), color = "firebrick2") +
  geom_line(aes(y = CLOSE), color = "orchid1", alpha = 0.75) +
  labs(title="VIX History", x="Date", y="Volatility")

### Part h ###----

## Convert the OPEN series to a time series object
ts_open <- ts(history$OPEN, start = c(1990, 1, 2), frequency = 365)
View(ts_open)

## Decomposing the time series
components.ts <- decompose(ts_open)
plot(components.ts)

### Part i ###----

## Performing Augmented Dicky-Fuller test for unit root
adf.test(ts_open)

# p-value is 0.01 < 0.05, therefore we reject the null hypothesis and conclude
# that the time series is stationary.

### Part j ###----

## ACF plot
acf(ts_open)

## PACF plot
pacf(ts_open)

# My stationary process is AR(p) because ACF plot of my time series falls off
# gradually and the PACF plot drops sharply after the first lag.

### Part k ###----

# Simulate ARIMA process
auto.arima(ts_open, stationary = TRUE, ic = c("aicc", "aic", "bic"))
ts_sim <- arima(ts_open, order = c(3,0,2))

# Create plots side by side
par(mfrow=c(1,2))

# Plot original time series
plot(ts_open, main = "Original Time Series", ylab = "Volatility", col = "royalblue2")

# Plot simulated ARIMA process
plot(fitted(ts_sim), main = "Simulated ARIMA Process", ylab = "Volatility", col = "goldenrod1")


