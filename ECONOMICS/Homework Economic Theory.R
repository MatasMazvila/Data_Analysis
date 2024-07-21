###############################################################################
#
#
#
#                    #### HOMEWORK ASSIGNMENT #####
#
#
#
###############################################################################

## Part 1.1.1.---------------------------------------------

library(readxl)

# Load the excel data from sheet 2.2 and specify the range of rows to import
data_2_2 <- read_excel(paste0("Data/", "Indicators_2023Q3.xlsx"), sheet = "2.2.", range = "A5:G119")

# Rename the columns for convenience
colnames(data_2_2)[1] <- "Date"
colnames(data_2_2)[2] <- "Credit-to-GDP"
colnames(data_2_2)[3] <- "Long-term trend (standardised method)"
colnames(data_2_2)[4] <- "Standardised credit-to-GDP gap"

library(dplyr)

# Remove the first row and the fifth column
data_2_2 <- data_2_2 %>%
  slice(-1) %>%
  select(-5)

# Define the start and end dates
start_date <- as.Date("1995-03-01")
end_date <- as.Date("2023-03-01")

# Generate a sequence of quarterly dates
quarterly_dates <- seq.Date(start_date, end_date, by = "3 months")

# Extract year and month without the day
year_month <- format(quarterly_dates, format = "%Y-%m")

# Replace Date column with year-month information
data_2_2$Date <- year_month

# Convert the Date column to the Date format
data_2_2$Date <- as.Date(paste(data_2_2$Date, "01", sep = "-"), format = "%Y-%m-%d")

# Define the cutoff date (first quarter of 2006)
cutoff_date <- "2006-03-01"

# Create a new dataset with values up to the cutoff date
data_up_to_q1_2006 <- data_2_2 %>%
  filter(Date <= cutoff_date)

# Define the lambda parameter for the Hodrick-Prescott filter
lambda <- 400000

library(mFilter)

# Convert "Credit-to-GDP" to numeric
data_up_to_q1_2006$`Credit-to-GDP` <- as.numeric(data_up_to_q1_2006$`Credit-to-GDP`)

# Remove rows with NA values in the "Credit-to-GDP" variable
data_up_to_q1_2006 <- data_up_to_q1_2006[complete.cases(data_up_to_q1_2006$`Credit-to-GDP`), ]

# Apply the Hodrick-Prescott filter to the existing "Credit-to-GDP" variable
hp_filter <- hpfilter(data_up_to_q1_2006$`Credit-to-GDP`, freq = lambda)

# Extract the trend component
credit_to_gdp_trend <- hp_filter$trend

# Calculate the credit-to-GDP gap
credit_to_gdp_gap <- data_up_to_q1_2006$`Credit-to-GDP` - credit_to_gdp_trend

library(ggplot2)

# Create a data frame with the three series
plot_data <- data.frame(
  Date = data_up_to_q1_2006$Date,
  Main_Series = data_up_to_q1_2006$`Credit-to-GDP`,
  Long_Term_Trend = credit_to_gdp_trend,
  Gap = credit_to_gdp_gap
)

# Create the plot
ggplot(plot_data, aes(x = Date, group = 1)) +
  geom_line(aes(y = Main_Series, color = "Main Series"), linewidth = 1) +
  geom_line(aes(y = Long_Term_Trend, color = "Long-Term Trend"), linewidth = 1) +
  geom_line(aes(y = Gap, color = "Gap"), linewidth = 1) +
  scale_color_manual(values = c("Main Series" = "blue", "Long-Term Trend" = "green", "Gap" = "red")) +
  labs(title = "Credit-to-GDP Gap Analysis",
       x = "Date",
       y = "Percentage Points") +
  theme_minimal()

## Part 1.1.2.---------------------------------------------

# Define the lambda parameter for a 16-year financial cycle
lambda_16_years <- 26000

# Apply the Hodrick-Prescott filter with the new lambda
hp_filter_16_years <- hpfilter(data_up_to_q1_2006$`Credit-to-GDP`, freq = lambda_16_years)

# Extract the trend component
credit_to_gdp_trend_16_years <- hp_filter_16_years$trend

# Calculate the credit-to-GDP gap for the 16-year cycle
credit_to_gdp_gap_16_years <- data_up_to_q1_2006$`Credit-to-GDP` - credit_to_gdp_trend_16_years

# Create a data frame with the three series, including the 16-year financial cycle
plot_data_16_years <- data.frame(
  Date = data_up_to_q1_2006$Date,
  Main_Series = data_up_to_q1_2006$`Credit-to-GDP`,
  Long_Term_Trend = credit_to_gdp_trend_16_years,
  Gap_16_Years = credit_to_gdp_gap_16_years
)

# Create the plot with the 16-year cycle gap
ggplot(plot_data_16_years, aes(x = Date, group = 1)) +
  geom_line(aes(y = Main_Series, color = "Main Series"), linewidth = 1) +
  geom_line(aes(y = Long_Term_Trend, color = "Long-Term Trend"), linewidth = 1) +
  geom_line(aes(y = Gap_16_Years, color = "Gap (16 Years)"), linewidth = 1) +
  scale_color_manual(values = c("Main Series" = "blue", "Long-Term Trend" = "green", "Gap (16 Years)" = "red")) +
  labs(
    title = "Credit-to-GDP Gap Analysis (16-Year Cycle)",
    x = "Date",
    y = "Percentage Points"
  ) +
  theme_minimal()

## Part 1.1.3.---------------------------------------------

#### λ = 400 000 ####

# Define the Lower and Higher Thresholds
lower_threshold <- 2  # 2 p.p.
higher_threshold <- 10  # 10 p.p.

# Initialize an empty vector to store buffer rates
buffer_rates <- numeric()

# Calculate buffer rates for each quarter
for (i in 1:length(credit_to_gdp_gap)) {
  gap_value <- credit_to_gdp_gap[i]
  
  # Apply the buffer rules
  if (gap_value <= lower_threshold) {
    buffer_rate <- 0
  } else if (gap_value >= higher_threshold) {
    buffer_rate <- 2.5
  } else {
    buffer_rate <- 0.3125 * gap_value - 0.625
  }
  
  # Append the buffer rate to the vector
  buffer_rates <- c(buffer_rates, buffer_rate)
}

# Calculate the reference buffer size for each quarter
reference_buffer_sizes <- buffer_rates

# Print the reference buffer sizes for each quarter
print(reference_buffer_sizes)

# Create a dataframe with Date, Credit-to-GDP Gap, and Reference Buffer Sizes
plot_data_buffer <- data.frame(Date = data_up_to_q1_2006$Date, 
                        Credit_to_GDP_Gap = credit_to_gdp_gap,
                        reference_buffer_sizes = reference_buffer_sizes)

# Create the plot using ggplot2
ggplot(plot_data, aes(x = Date)) +
  geom_line(aes(y = credit_to_gdp_gap, color = "Credit-to-GDP Gap"), size = 1) +
  geom_line(aes(y = reference_buffer_sizes, color = "Reference Buffer (λ = 400,000)"), size = 1) +
  geom_hline(yintercept = c(0, 2, 10), linetype = "dashed", color = "red") +
  labs(x = "Quarterly Dates", y = "Value") +
  ggtitle("Credit-to-GDP Gap and Reference Buffer Sizes (λ = 400,000)") +
  scale_color_manual(values = c("Credit-to-GDP Gap" = "yellowgreen", "Reference Buffer (λ = 400,000)" = "navyblue")) +
  theme_minimal()

#### λ = 26 000 ####

# Define the Lower and Higher Thresholds
lower_threshold <- 2  # 2 p.p.
higher_threshold <- 10  # 10 p.p.

# Initialize an empty vector to store buffer rates
buffer_rates <- numeric()

# Calculate buffer rates for each quarter
for (i in 1:length(credit_to_gdp_gap_16_years)) {
  gap_value <- credit_to_gdp_gap_16_years[i]
  
  # Apply the buffer rules
  if (gap_value <= lower_threshold) {
    buffer_rate <- 0
  } else if (gap_value >= higher_threshold) {
    buffer_rate <- 2.5
  } else {
    buffer_rate <- 0.3125 * gap_value - 0.625
  }
  
  # Append the buffer rate to the vector
  buffer_rates <- c(buffer_rates, buffer_rate)
}

# Calculate the reference buffer size for each quarter
reference_buffer_sizes_16_years <- buffer_rates

# Print the reference buffer sizes for each quarter
print(reference_buffer_sizes_16_years)

# Create a dataframe with Date, Credit-to-GDP Gap, and Reference Buffer Sizes for the 16-year financial cycle
plot_data_buffer_16_years <- data.frame(
  Date = data_up_to_q1_2006$Date,
  Credit_to_GDP_Gap = credit_to_gdp_gap_16_years,
  reference_buffer_sizes = reference_buffer_sizes_16_years
)

# Create a plot of Credit-to-GDP Gap and Reference Buffer Sizes with lambda = 26,000 and 16-year cycle
ggplot(plot_data_buffer_16_years, aes(x = Date)) +
  geom_line(aes(y = credit_to_gdp_gap_16_years, color = "Credit-to-GDP Gap"), size = 1) +
  geom_line(aes(y = reference_buffer_sizes, color = "Reference Buffer (λ = 26,000)"), size = 1) +
  geom_hline(yintercept = c(0, 2, 10), linetype = "dashed", color = "red") +
  labs(x = "Dates", y = "Percentage Points") +
  ggtitle("Credit-to-GDP Gap and Reference Buffer Sizes (λ = 26,000)") +
  scale_color_manual(values = c("Credit-to-GDP Gap" = "orchid", "Reference Buffer (λ = 26,000)" = "darkblue")) +
  theme_minimal()

## Part 1.2.---------------------------------------------

# Load the excel data for the loan-to-deposit ratio
data_LDR <- read_excel(paste0("Data/", "Indicators_2023Q3.xlsx"), sheet = "2.3. Other indicators", range = "N6:N53")

# Remove the first row
data_LDR <- data_LDR %>%
  slice(-1) 

# Convert to numeric values
data_LDR$`Loan-to-deposit ratio` <- as.numeric(data_LDR$`Loan-to-deposit ratio`)

# Create a sequence of quarterly dates
dates <- seq(from = as.Date("1994-12-31"), to = as.Date("2006-03-31"), by = "3 months")

# Create a new dataframe with Dates and Loan-to-deposit ratio
data_LDR <- data.frame(
  Date = dates,
  `Loan-to-deposit ratio` = data_LDR$`Loan-to-deposit ratio`
)

# Plot the LDR over time
ggplot(data_LDR, aes(x = Date, y = `Loan.to.deposit.ratio`)) +
  geom_line() +
  labs(x = "Date", y = "Loan-to-deposit ratio (%)") +
  ggtitle("Loan-to-Deposit Ratio Over Time") +
  scale_x_date(date_labels = "%Y", date_breaks = "12 months") + # Adjust date labels
  theme_minimal()


