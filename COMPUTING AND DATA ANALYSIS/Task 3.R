#### TASK 3 ####
## Installing necessary packages
install.packages("tidyverse")

### Part 1 ###----

## Loading the dataset into R
data <- read.csv2("C:/Users/DELL i5/OneDrive/Documents/variablesforreplication.csv")

## Removing the missing values in columns with our fixed variables
clean_data <- data[complete.cases(data[, c("gamma", "GDPSH60", "LIFEE060", "P60")]),]

## Summary statistics of the main variables of interest
summary(clean_data[, c("gamma", "GDPSH60", "LIFEE060", "P60")])

## Note: the results are interpreted in the report


### Part 2 ###----

## Run the fixed regression
fixed_regression <- lm(gamma ~ GDPSH60 + LIFEE060 + P60, data = clean_data)

## Sumarry of the regression
summary(fixed_regression)

## Note: the findings are reported and interpreted in the report


### Part 3 ###----

## Regressions with fixed variables and variables from the table
reg1 <- lm(gamma ~ GDPSH60 + LIFEE060 + P60 + YrsOpen + EQINV + SPAIN + RULELAW, data = clean_data)
reg2 <- lm(gamma ~ GDPSH60 + LIFEE060 + P60 + MUSLIM + prightsb + laam + safrica, data = clean_data)
reg3 <- lm(gamma ~ GDPSH60 + LIFEE060 + P60 + civlibb + revcoup + Mining + bmp1, data = clean_data)
reg4 <- lm(gamma ~ GDPSH60 + LIFEE060 + P60 + PRIEXP70 + EcOrg + wardum + EQINV, data = clean_data)
reg5 <- lm(gamma ~ GDPSH60 + LIFEE060 + P60 + BUDDHA + ABSLATIT + RERD + prightsb, data = clean_data)
reg6 <- lm(gamma ~ GDPSH60 + LIFEE060 + P60 + PROT + NONEQINV + CATH + RERD, data = clean_data)
reg7 <- lm(gamma ~ GDPSH60 + LIFEE060 + P60 + SPAIN + YrsOpen + ABSLATIT + CONFUC, data = clean_data)
reg8 <- lm(gamma ~ GDPSH60 + LIFEE060 + P60 + CONFUC + safrica + MUSLIM + wardum, data = clean_data)
reg9 <- lm(gamma ~ GDPSH60 + LIFEE060 + P60 + laam + RULELAW + PROT + PRIEXP70, data = clean_data)
reg10 <- lm(gamma ~ GDPSH60 + LIFEE060 + P60 + revcoup + CATH + bmp1 + EcOrg, data = clean_data)
reg11 <- lm(gamma ~ GDPSH60 + LIFEE060 + P60 + BUDDHA + Mining + civlibb + NONEQINV, data = clean_data)

library(tidyverse)

## Function that extracts coefficients, p-values, and standard deviations
extract_info <- function(model) {
  coef_table <- broom::tidy(model, conf.int = TRUE)
  coef_table %>%
    filter(term != "(Intercept)") %>%
    select(term, estimate, std.error, p.value) %>%
    mutate_if(is.numeric, round, digits = 4)
}

## Applying the function to all 11 models and adding the results into a table
results_table <- map_dfr(list(reg1, reg2, reg3, reg4, reg5, reg6, reg7, reg8, reg9, reg10, reg11),
             extract_info, .id = "Regression") %>% arrange(Regression, term)

## Reporting the findings in a table
View(results_table)

## Note: the table is also provided in the report


### Part 4 ###----

## Note: part 4 is answered in the report.











































