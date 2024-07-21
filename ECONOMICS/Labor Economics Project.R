###############################################################################
#
#
#      #### Labor Supply Optimal Decision or Options Available? #####
#
#                           ### EXAM ID: 302 ###
#           
###############################################################################

# Upload the dataset
data <- read.csv(paste0("Data/", "PUF_LFS_2019Q1.csv"), header = TRUE, sep=",")

# Create a new data frame with only the necessary variables
subset_data <- data[, c("HWACTUAL", "HWWISH", "WISHMORE", "HWUSUAL")]

library(dplyr)

# Create separate data frames for each variable and clean them
data_HWACTUAL <- subset_data %>%
  select(HWACTUAL) %>%
  filter(!(HWACTUAL %in% c("0", "99")))

data_HWUSUAL <- subset_data %>%
  select(HWUSUAL) %>%
  filter(!(HWUSUAL %in% c("0", "99")))

data_HWWISH <- subset_data %>%
  select(HWWISH) %>%
  filter(!(HWWISH %in% "99"))

data_WISHMORE <- subset_data %>%
  select(WISHMORE) %>%
  filter(!(WISHMORE %in% "9"))

# Define breaks and labels for different categories
breaks <- c(0, 1, 2, 3, 4, 5)
labels <- c("up to 20 hours", "21-29 hours", "30-39 hours", "40-44 hours", "45 hours or more")

# Create a new columns with categories
data_HWACTUAL$HWACTUAL_category <- cut(data_HWACTUAL$HWACTUAL, breaks = breaks, labels = labels, include.lowest = TRUE)
data_HWUSUAL$HWUSUAL_category <- cut(data_HWUSUAL$HWUSUAL, breaks = breaks, labels = labels, include.lowest = TRUE)
data_HWWISH$HWWISH_category <- cut(data_HWWISH$HWWISH, breaks = breaks, labels = labels, include.lowest = TRUE)

# All Distributions Separately--------------------------------------
library(ggplot2)

# Plot the distribution of whether people wish to work more
ggplot(data_WISHMORE, aes(x = factor(WISHMORE))) +
  geom_bar(aes(fill = factor(WISHMORE)), position = "dodge") +
  labs(title = "Do People Wish to Work More?",
       x = "Wish to work more",
       y = "Count of Responses") +
  scale_fill_manual(values = c("0" = "darkred", "1" = "chartreuse4"),
                    guide = "none") +  # Remove legend
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) +
  theme_minimal()

# Plot the distribution of actual hours worked
ggplot(data_HWACTUAL, aes(x = HWACTUAL_category)) +
  geom_bar(fill = "deepskyblue2", color = "white") +
  labs(title = "Distribution of Actual Hours Worked",
       x = "Actual Hours Worked",
       y = "Count of Responses") +
  theme_minimal() +
  ylim(0, 7000)

# Plot the distribution of usual working hours
ggplot(data_HWUSUAL, aes(x = HWUSUAL_category)) +
  geom_bar(fill = "deeppink2", color = "white") +
  labs(title = "Distribution of Usual Working Hours",
       x = "Usual Working Hours",
       y = "Count of Responses") +
  theme_minimal() +
  ylim(0, 7000)

# Plot the distribution of hours wished to work
ggplot(data_HWWISH, aes(x = HWWISH_category)) +
  geom_bar(fill = "chocolate3", color = "white") +
  labs(title = "Distribution of Hours Wished to Work",
       x = "Wished Hours to Work",
       y = "Count of Responses") +
  theme_minimal() +
  ylim(0, 7000)

# Actual vs Wished Working Hours --------------------------------------
# Create a subset of data for a plot
plot_data <- data[, c("HWACTUAL", "HWWISH")]

# Remove rows with unnecessary values from the dataset
clean_plot_data <- plot_data[!(plot_data$HWACTUAL %in% c("0", "99") | plot_data$HWWISH %in% "99"),]

# Create a new columns with categories
clean_plot_data$HWACTUAL_category <- cut(clean_plot_data$HWACTUAL, breaks = breaks, labels = labels, include.lowest = TRUE)
clean_plot_data$HWWISH_category <- cut(clean_plot_data$HWWISH, breaks = breaks, labels = labels, include.lowest = TRUE)

# Create a point plot with count of responses for both actual and wished working hours
ggplot(clean_plot_data, aes(x = as.factor(HWACTUAL_category), color = "HWACTUAL")) +
  geom_point(stat = "count", size = 2) +
  geom_point(aes(x = as.factor(HWWISH_category), color = "HWWISH"), stat = "count", size = 2) +
  labs(title = "Actual vs Wished Working Hours",
       x = "Hours Worked",
       y = "Count of Responses",
       color = "Variable") +
  scale_color_manual(values = c("HWACTUAL" = "deepskyblue2", "HWWISH" = "chocolate3")) +
  theme_minimal()

# There are more cases in which wished hours are higher than actual hours
sum(clean_plot_data$HWACTUAL > clean_plot_data$HWWISH)
# 567
sum(clean_plot_data$HWWISH > clean_plot_data$HWACTUAL)
# 1012

# Usual vs Wished Working Hours --------------------------
# Create a subset of data for a plot
plot_data2 <- data[, c("HWWISH", "HWUSUAL")]

# Remove unnecessary values from the dataset
clean_plot_data2 <- plot_data2[!(plot_data2$HWWISH %in% "99" |
                                   plot_data2$HWUSUAL %in% c("99", "0")),]

# Create a new columns with categories
clean_plot_data2$HWWISH_category <- cut(clean_plot_data2$HWWISH, breaks = breaks, labels = labels, include.lowest = TRUE)
clean_plot_data2$HWUSUAL_category <- cut(clean_plot_data2$HWUSUAL, breaks = breaks, labels = labels, include.lowest = TRUE)

# Create a point plot with count of responses for usual and wished working hours
ggplot(clean_plot_data2, aes(x = as.factor(HWUSUAL_category), color = "HWUSUAL")) +
  geom_point(stat = "count", size = 2, alpha = 0.75) +
  geom_point(aes(x = as.factor(HWWISH_category), color = "HWWISH"), stat = "count", size = 2, alpha = 0.75) +
  labs(title = "Usual vs Wished Working Hours",
       x = "Hours Worked",
       y = "Count of Responses",
       color = "Variable") +
  scale_color_manual(values = c("HWUSUAL" = "deeppink2", "HWWISH" = "chocolate3")) +
  theme_minimal()

# Actual vs Wished vs Usual Working Hours--------------------------------------
# Create a subset of data for a plot
plot_data1 <- data[, c("HWACTUAL", "HWWISH", "HWUSUAL")]

# Remove unnecessary values from the dataset
clean_plot_data1 <- plot_data1[!(plot_data1$HWACTUAL %in% c("0", "99") | plot_data1$HWWISH %in% "99" |
                                 plot_data1$HWUSUAL %in% c("99", "0")),]

# Create a new columns with categories
clean_plot_data1$HWACTUAL_category <- cut(clean_plot_data1$HWACTUAL, breaks = breaks, labels = labels, include.lowest = TRUE)
clean_plot_data1$HWWISH_category <- cut(clean_plot_data1$HWWISH, breaks = breaks, labels = labels, include.lowest = TRUE)
clean_plot_data1$HWUSUAL_category <- cut(clean_plot_data1$HWUSUAL, breaks = breaks, labels = labels, include.lowest = TRUE)

# Create a point plot with count of responses for actual, wished, and usual working hours
ggplot(clean_plot_data1, aes(x = as.factor(HWACTUAL_category), color = "HWACTUAL")) +
  geom_point(stat = "count", size = 1.5) +
  geom_point(aes(x = as.factor(HWWISH_category), color = "HWWISH"), stat = "count", size = 1.5) +
  geom_point(aes(x = as.factor(HWUSUAL_category), color = "HWUSUAL"), stat = "count", size = 1.5) +
  labs(title = "Actual vs Wished vs Usual Working Hours",
       x = "Hours Worked",
       y = "Count of Responses",
       color = "Variable") +
  scale_color_manual(values = c("HWACTUAL" = "deepskyblue2", "HWWISH" = "chocolate3", "HWUSUAL" = "deeppink2")) +
  theme_minimal()

# Females and Males-------------------------------
# Create a new data frame with only the necessary variables
subset_data_sex <- data[, c("SEX", "HWACTUAL", "HWWISH", "WISHMORE", "HWUSUAL")]

# FEMALES ---------------------------
# Create a new data frame with only female respondents
subset_data_female <- subset_data_sex %>%
  filter(SEX != 1)

# Remove unnecessary values from the dataset
Female_plot_data <- subset_data_female[!(subset_data_female$HWACTUAL %in% c("0", "99") | subset_data_female$HWWISH %in% "99" |
                                           subset_data_female$HWUSUAL %in% c("99", "0")),]

# Create new columns with categories
Female_plot_data$HWACTUAL_category <- cut(Female_plot_data$HWACTUAL, breaks = breaks, labels = labels, include.lowest = TRUE)
Female_plot_data$HWUSUAL_category <- cut(Female_plot_data$HWUSUAL, breaks = breaks, labels = labels, include.lowest = TRUE)
Female_plot_data$HWWISH_category <- cut(Female_plot_data$HWWISH, breaks = breaks, labels = labels, include.lowest = TRUE)

# Create a point plot with count of responses for actual and wished working hours
ggplot(Female_plot_data, aes(x = as.factor(HWACTUAL_category), color = "HWACTUAL")) +
  geom_point(stat = "count", size = 2) +
  geom_point(aes(x = as.factor(HWWISH_category), color = "HWWISH"), stat = "count", size = 2) +
  labs(title = "Actual vs Wished Working Hours (Females)",
       x = "Hours Worked",
       y = "Count of Responses",
       color = "Variable") +
  scale_color_manual(values = c("HWACTUAL" = "deepskyblue2", "HWWISH" = "chocolate3")) +
  theme_minimal()

# Create a point plot with count of responses for usual and wished working hours
ggplot(Female_plot_data, aes(x = as.factor(HWUSUAL_category), color = "HWUSUAL")) +
  geom_point(stat = "count", size = 2, alpha = 0.75) +
  geom_point(aes(x = as.factor(HWWISH_category), color = "HWWISH"), stat = "count", size = 2, alpha = 0.75) +
  labs(title = "Usual vs Wished Working Hours (Females)",
       x = "Hours Worked",
       y = "Count of Responses",
       color = "Variable") +
  scale_color_manual(values = c("HWUSUAL" = "deeppink2", "HWWISH" = "chocolate3")) +
  theme_minimal()

# Create a bar plot for the distribution of whether people wish to work more
ggplot(Female_plot_data, aes(x = factor(WISHMORE))) +
  geom_bar(aes(fill = factor(WISHMORE)), position = "dodge") +
  labs(title = "Do People Wish to Work More? (Females)",
       x = "Wish to work more",
       y = "Count of Responses") +
  scale_fill_manual(values = c("0" = "darkred", "1" = "chartreuse4"),
                    guide = "none") +  # Remove legend
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) +
  theme_minimal()

# MALES --------------------------------------
# Create a new data frame with only male respondents
subset_data_male <- subset_data_sex %>%
  filter(SEX != 2)

# Remove unnecessary values from the dataset
male_plot_data <- subset_data_male[!(subset_data_male$HWACTUAL %in% c("0", "99") | subset_data_male$HWWISH %in% "99" |
                                       subset_data_male$HWUSUAL %in% c("99", "0")),]

# Create a new columns with categories
male_plot_data$HWACTUAL_category <- cut(male_plot_data$HWACTUAL, breaks = breaks, labels = labels, include.lowest = TRUE)
male_plot_data$HWUSUAL_category <- cut(male_plot_data$HWUSUAL, breaks = breaks, labels = labels, include.lowest = TRUE)
male_plot_data$HWWISH_category <- cut(male_plot_data$HWWISH, breaks = breaks, labels = labels, include.lowest = TRUE)

# Create a point plot with count of responses for actual and wished working hours
ggplot(male_plot_data, aes(x = as.factor(HWACTUAL_category), color = "HWACTUAL")) +
  geom_point(stat = "count", size = 2) +
  geom_point(aes(x = as.factor(HWWISH_category), color = "HWWISH"), stat = "count", size = 2) +
  labs(title = "Actual vs Wished Working Hours (Males)",
       x = "Hours Worked",
       y = "Count of Responses",
       color = "Variable") +
  scale_color_manual(values = c("HWACTUAL" = "deepskyblue2", "HWWISH" = "chocolate3")) +
  theme_minimal()

# Create a point plot with count of responses for usual and wished working hours
ggplot(male_plot_data, aes(x = as.factor(HWUSUAL_category), color = "HWUSUAL")) +
  geom_point(stat = "count", size = 2, alpha = 0.75) +
  geom_point(aes(x = as.factor(HWWISH_category), color = "HWWISH"), stat = "count", size = 2, alpha = 0.75) +
  labs(title = "Usual vs Wished Working Hours (Males)",
       x = "Hours Worked",
       y = "Count of Responses",
       color = "Variable") +
  scale_color_manual(values = c("HWUSUAL" = "deeppink2", "HWWISH" = "chocolate3")) +
  theme_minimal()

# Create a bar plot for the distribution of whether people wish to work more
ggplot(male_plot_data, aes(x = factor(WISHMORE))) +
  geom_bar(aes(fill = factor(WISHMORE)), position = "dodge") +
  labs(title = "Do People Wish to Work More? (Males)",
       x = "Wish to work more",
       y = "Count of Responses") +
  scale_fill_manual(values = c("0" = "darkred", "1" = "chartreuse4"),
                    guide = "none") +  # Remove legend
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) +
  theme_minimal()