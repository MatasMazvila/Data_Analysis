### Exercise 2 ###

library(haven)
library(dplyr)
library(tidyr)
library(fastDummies)

financial_literacy_data <- read_dta("C:/Users/DELL i5/OneDrive/Documents/Financial literacy data.dta")

# Drop observations with NA values in Q27
financial_literacy_data <- financial_literacy_data %>%
  filter(Q27 != "")

## Financial Behaviour ##------------------------

# Recode Q2_1 - Q2_5 and Q3_1 - Q3_5 from strongly disagree (1) to strongly agree (5)
for (q in c(paste0("Q2_", 1:5), paste0("Q3_", 1:5))) {
  financial_literacy_data[[paste0("n", q)]] <- case_when(
    financial_literacy_data[[q]] == "Strongly disagree" ~ 1,
    financial_literacy_data[[q]] == "Somewhat disagree" ~ 2,
    financial_literacy_data[[q]] == "Neither agree nor disagree" ~ 3,
    financial_literacy_data[[q]] == "Somewhat agree" ~ 4,
    financial_literacy_data[[q]] == "Strongly agree" ~ 5,
    TRUE ~ NA_real_ 
  )
}

# Recode Q5 from "Not confident" (1) to "Completely confident" (5)
financial_literacy_data$nQ5 <- case_when(
  financial_literacy_data$Q5 == "Not confident" ~ 1,
  financial_literacy_data$Q5 == "Slightly confident" ~ 2,
  financial_literacy_data$Q5 == "Somewhat confident" ~ 3,
  financial_literacy_data$Q5 == "Fairly confident" ~ 4,
  financial_literacy_data$Q5 == "Completely confident" ~ 5,
  TRUE ~ NA_real_  
)

# Adjusting the composite score for Financial Behaviour to include nQ5
financial_literacy_data$FinancialBehaviour <- rowMeans(financial_literacy_data[,grep("nQ2_", names(financial_literacy_data))], na.rm = TRUE) + 
  rowMeans(financial_literacy_data[,grep("nQ3_", names(financial_literacy_data))], na.rm = TRUE) + 
  financial_literacy_data$nQ5

# Adjusting encoding for Q3_3 and Q3_4 with reverse scoring
financial_literacy_data$nQ3_3 <- case_when(
  financial_literacy_data$Q3_3 == "Strongly disagree" ~ 5,  # Reverse scoring
  financial_literacy_data$Q3_3 == "Somewhat disagree" ~ 4,
  financial_literacy_data$Q3_3 == "Neither agree nor disagree" ~ 3,
  financial_literacy_data$Q3_3 == "Somewhat agree" ~ 2,
  financial_literacy_data$Q3_3 == "Strongly agree" ~ 1,
  TRUE ~ NA_real_  
)

financial_literacy_data$nQ3_4 <- case_when(
  financial_literacy_data$Q3_4 == "Strongly disagree" ~ 5,  # Reverse scoring
  financial_literacy_data$Q3_4 == "Somewhat disagree" ~ 4,
  financial_literacy_data$Q3_4 == "Neither agree nor disagree" ~ 3,
  financial_literacy_data$Q3_4 == "Somewhat agree" ~ 2,
  financial_literacy_data$Q3_4 == "Strongly agree" ~ 1,
  TRUE ~ NA_real_  
)

# Combine all relevant nQ variables into one average score for FinancialBehaviour
financial_literacy_data$FinancialBehaviour <- rowMeans(financial_literacy_data[,c(grep("nQ2_", names(financial_literacy_data), value = TRUE), 
                                                                                  grep("nQ3_", names(financial_literacy_data), value = TRUE), 
                                                                                  "nQ5")], na.rm = TRUE)

## Financial Literacy ##------------------------

# Encoding for Q6 - "Buying a single company stock usually provides a safer return than a stock mutual fund."
financial_literacy_data$nQ6 <- ifelse(financial_literacy_data$Q6 == "False", 5/7, 0)

# Encoding for Q7 - "Imagine that the interest rate on your savings account was 1% per year and inflation was 2% per year. After 1 year, how much would you be able to buy with the money in this account?"
financial_literacy_data$nQ7 <- ifelse(financial_literacy_data$Q7 == "Less than today", 5/7, 0)

# Encoding for Q8 - "Suppose you had £100 in a savings account and the interest rate was 2% per year. After 5 years, how much do you think you would have in the account if you left the money to grow?"
financial_literacy_data$nQ8 <- ifelse(financial_literacy_data$Q8 == "More than £102", 5/7, 0)

# Encoding for Q9_1 - Q9_4
financial_literacy_data$nQ9_1 <- ifelse(financial_literacy_data$Q9_1 == "True", 5/7, 0)
financial_literacy_data$nQ9_2 <- ifelse(financial_literacy_data$Q9_2 == "False", 5/7, 0)
financial_literacy_data$nQ9_3 <- ifelse(financial_literacy_data$Q9_3 == "True", 5/7, 0) 
financial_literacy_data$nQ9_4 <- ifelse(financial_literacy_data$Q9_4 == "True", 5/7, 0)

# Summing the encoded variables to create a Financial Literacy score
financial_literacy_data$FinancialLiteracy <- financial_literacy_data$nQ6 +
  financial_literacy_data$nQ7 +
  financial_literacy_data$nQ8 +
  financial_literacy_data$nQ9_1 +
  financial_literacy_data$nQ9_2 +
  financial_literacy_data$nQ9_3 +
  financial_literacy_data$nQ9_4

## Financial Fragility ##------------------------

# Encoding for Q10 - Confidence in coming up with £2000
financial_literacy_data$nQ10 <- case_when(
  financial_literacy_data$Q10 == "I am certain I could come up with the full £2,000" ~ 1,
  financial_literacy_data$Q10 == "I could probably come up with £2,000" ~ 2,
  financial_literacy_data$Q10 == "I could probably not come up with £2,000" ~ 3,
  financial_literacy_data$Q10 == "I am certain I could not come up with £2,000" ~ 4,
  financial_literacy_data$Q10 == "I don't know" ~ 5,
  TRUE ~ NA_real_  
)

# Add financial fragility variable to the dataset
financial_literacy_data$FinancialFragility <- financial_literacy_data$nQ10

## Financial Stress ##------------------------
  
for (i in 1:5) {
  question_name <- paste0("Q12_", i)
  financial_literacy_data[[question_name]] <- case_when(
    financial_literacy_data[[question_name]] == "Never" ~ 1,
    financial_literacy_data[[question_name]] == "Rarely" ~ 2,
    financial_literacy_data[[question_name]] == "Sometimes" ~ 3,
    financial_literacy_data[[question_name]] == "Often" ~ 4,
    financial_literacy_data[[question_name]] == "Always" ~ 5,
    TRUE ~ NA_real_ 
  )
}
  
# Create a composite score for financial stress
financial_literacy_data$FinancialStress <- rowMeans(financial_literacy_data[, grep("Q12_", names(financial_literacy_data))], na.rm = TRUE)
  
## Demographic Variables ##------------------------

# Filter the levels for gender to only include "Male" and "Female"
financial_literacy_data <- financial_literacy_data %>%
  filter(Q18 %in% c("Male", "Female"))

# Create gender dummy variable (Male as reference group)
financial_literacy_data$gender_female <- ifelse(financial_literacy_data$Q18 == "Female", 1, 0)

# Filter the levels for marital status to only include "Married" and "Single"
financial_literacy_data <- financial_literacy_data %>%
  filter(Q26 %in% c("Married", "Single"))

# Create marital status dummy variable (Single as reference group)
financial_literacy_data$marital_status_married <- ifelse(financial_literacy_data$Q26 == "Married", 1, 0)

# Create education dummy variable for bachelor's degree
financial_literacy_data$education_bachelors <- ifelse(financial_literacy_data$Q21 == "University Bachelors Degree", 1, 0)

## OLS ##------------------------

# OLS model
ols_model <- lm(FinancialFragility ~ FinancialLiteracy + FinancialBehaviour + FinancialStress,
                data = financial_literacy_data)

# Summary of the OLS model
summary(ols_model)

# Model 1: Gender and Marital Status
model1 <- lm(FinancialFragility ~ FinancialLiteracy + FinancialBehaviour + FinancialStress +
               gender_female + marital_status_married, data = financial_literacy_data)
summary(model1)

# Model 2: Education (Bachelor's Degree), Marital Status, and Gender
model2 <- lm(FinancialFragility ~ FinancialLiteracy + FinancialBehaviour + FinancialStress +
             gender_female + education_bachelors + marital_status_married,
             data = financial_literacy_data)
summary(model2)

## Regression Visualization ##------------------------

stargazer(ols_model, model1, model2, type = "text", 
          title = "Regression Results",
          align = TRUE,
          out = "regression_results.doc",
          report = "vc*t",  # v: variable labels, c: coefficients, t: t-statistics
          single.row = TRUE)

stargazer(ols_model, model1, model2, type = "html", 
          title = "Regression Results",
          out = "regression_results.html")

## Data Summaries ##

# Main Summary
summary(financial_literacy_data[c("FinancialLiteracy", "FinancialBehaviour", "FinancialStress", "FinancialFragility")])

# Correlation Matrix
cor(financial_literacy_data[c("FinancialLiteracy", "FinancialBehaviour", "FinancialStress", "FinancialFragility")], use = "complete.obs")



