#### TASK 4 ####

## Installing necessary packages
install.packages("tidyverse")
install.packages("PerformanceAnalytics")
install.packages("tidyquant")
install.packages("quantmod")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("reshape2")
library(tidyverse)
library(PerformanceAnalytics)
library(tidyquant)
library(quantmod)
library(lubridate)
library(ggplot2)
library(reshape2)

### Part 1 ###----

## Loading the dataset into R
data <- read.csv2("C:/Users/DELL i5/OneDrive/Documents/EVstocks2122.csv", sep = ",")

## Creating a vector of symbols
symbols <- data[, c("X", "SPY","TSLA","SOLO","WKHS","NKLA", "PTRA")]

## Converting price columns to numeric values
symbols[, 2:7] <- lapply(symbols[, 2:7], as.numeric)

## Calculating the monthly prices
monthly_prices <- symbols %>%
  mutate(X = as.Date(paste0(year(X), "-", month(X), "-01"))) %>%
  group_by(X) %>%
  summarize_all(mean)

## Creating a data frame
monthly_prices_df <- as.data.frame(monthly_prices)

## Setting dates as row names
rownames(monthly_prices_df) <- monthly_prices_df$X

## Removing the column X which contains dates
monthly_prices_df$X <- NULL

## Converting prices into numeric variables
monthly_prices <- monthly_prices %>%
  mutate_at(vars(SPY, TSLA, SOLO, WKHS, NKLA, PTRA), as.numeric)

## Calculating log returns for the assets
asset_returns <- monthly_prices_df %>%
  Return.calculate(method = "log")

## Converting the row names to exclude day since it is monthly data
rownames(asset_returns) <- format(as.Date(rownames(asset_returns)), "%Y-%m")

## Creating a date column in the dataset
asset_returns$date <- rownames(asset_returns)

## Removing the row names
rownames(asset_returns) <- NULL

## Setting the `date` column as the first column in the dataset
asset_returns <- asset_returns[,c(ncol(asset_returns),1:(ncol(asset_returns)-1))]


