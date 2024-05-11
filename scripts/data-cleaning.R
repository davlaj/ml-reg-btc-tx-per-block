# Install packages if they are not already installed
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("tidyr")) install.packages("tidyr")
if (!require("lubridate")) install.packages("lubridate", dependencies=TRUE)
if (!require("purrr")) install.packages("purrr")

# Load packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(purrr)

# Load data 
data <- read.csv("/Users/github/bitcoin-transactions-per-block/data/raw/block_data.csv")

###################
# Data Formatting #
###################

# Convert the 'Date' column to POSIXct date-time objects
data$Date <- as.POSIXct(data$Date, origin="1970-01-01", tz="UTC")
# View the updated dataframe to confirm changes
head(data)

# Adding new columns
## Hour of the day
data$HourOfDay <- hour(data$Date)
## Day of the week
# Returns numbers where 1 = Sunday, 2 = Monday, ..., 7 = Saturday
data$DayOfWeek <- wday(data$Date) 

################
# Missing Data #
################

# Identify missing data
summary(data)  # Gives a summary, including NA counts for all columns

# Only 28 blocks with missing AverageFee.  
# These are blocks with only 1 tx, the coinbase tx which doesn't have any fee
missing_averageFee_rows <- data[is.na(data$AverageFee), ]
print(missing_averageFee_rows) 

# Decide on a strategy
## Imputation -> Replace by 0
data <- data %>%
  mutate(across(everything(), ~replace(., is.na(.), 0)))

## Deletion
# data <- na.omit(data)  # Removes all rows with any NA values

############
# Outliers #
############

colnames(data)

# Detection
# Reshape the data and create separate boxplots for all numeric columns
data %>%
  select(where(is.numeric)) %>%  # Select only numeric columns
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%  # Reshape data
  ggplot(aes(x = "", y = Value)) +  # Setup axes
  geom_boxplot() +  # Create boxplots
  facet_wrap(~ Variable, scales = "free_y") +  # Separate plot for each variable with free y scale
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +  # Remove x-axis elements
  labs(y = "Values", title = "Separate Boxplot for Each Numeric Variable")  # Label the y-axis and provide a title

# # Treatment: Trimming (removing)
# quantiles <- quantile(data$numeric_column, probs=c(.25, .75), na.rm = TRUE)
# iqr <- IQR(data$numeric_column, na.rm = TRUE)
# data <- data %>%
#   filter(numeric_column >= (quantiles[1] - 1.5 * iqr) & numeric_column <= (quantiles[2] + 1.5 * iqr))

##########################################
# Correcting Inconsistencies and Scaling #
##########################################

# Standardization (uniformity/consitency in values)
# none

# Standardization (Z-score scaling) -> Apply to continuous numeric non-categorical data
numeric_cols_to_scale <- names(data)[sapply(data, is.numeric) & !names(data) %in% c("TotalTransactions", "BlockNumber", "HourOfDay", "DayOfWeek")]
data_scaled <- data %>%
  mutate(across(all_of(numeric_cols_to_scale), scale))

# Boxplot after scaling
data_scaled %>%
  select(where(function(x) is.numeric(x)), -c(TotalTransactions, BlockNumber, HourOfDay, DayOfWeek)) %>%  # Select only numeric columns
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%  # Reshape data
  ggplot(aes(x = "", y = Value)) +  # Setup axes
  geom_boxplot() +  # Create boxplots
  facet_wrap(~ Variable, scales = "free_y") +  # Separate plot for each variable with free y scale
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +  # Remove x-axis elements
  labs(y = "Values", title = "Separate Boxplot for Each Non-Categorical Numeric Variables after Scaling")  # Label the y-axis and provide a title

summary(data_scaled)

# Encoding categorical variable
## One-hot encoding for nominal data
# Convert HourOfDay and DayOfWeek to factors to ensure model.matrix recognizes them as categorical variables
data_scaled$HourOfDay <- factor(data_scaled$HourOfDay)
data_scaled$DayOfWeek <- factor(data_scaled$DayOfWeek)
# model.matrix will by default drop the first dummy variable of each category if the index start at 1
# DayOfWeek starts at 1, but HourOfDay start at 0, so we need to drop it to avoid the Dummy Variable Trap
onehot_encoded <- model.matrix(~ HourOfDay + DayOfWeek - 1, data_scaled) 
data_encoded_onehot <- cbind(data_scaled, onehot_encoded)
data_encoded_onehot <- data_encoded_onehot %>% select(-HourOfDay0)  # Remove the HourOfDay0 column
# Check the structure to confirm changes
str(data_encoded_onehot)
# Convert back DayOfWeek and HourOfDay to numeric format 
data_encoded_onehot$DayOfWeek <- as.numeric(as.character(data_encoded_onehot$DayOfWeek))
data_encoded_onehot$HourOfDay <- as.numeric(as.character(data_encoded_onehot$HourOfDay))


## Label encoding
# none

## Cyclical encoding
data_encoded_onehot$hour_sin <- sin(2 * pi * data_encoded_onehot$HourOfDay / 24)
data_encoded_onehot$hour_cos <- cos(2 * pi * data_encoded_onehot$HourOfDay / 24)
data_encoded_onehot$day_sin <- sin(2 * pi * data_encoded_onehot$DayOfWeek / 7)
data_encoded_onehot$day_cos <- cos(2 * pi * data_encoded_onehot$DayOfWeek / 7)
data_encoded <- data_encoded_onehot
head(data_encoded)

#######################
# Feature Engineering #
#######################

# # Interaction terms
# data$interaction_term <- with(data, variable1 * variable2)
# 
# # Polynomial features
# data$polynomial_term <- with(data, variable1^2)

#########################
# Data Integrity Checks #
#########################

# # Remove duplicates
# data <- data %>% distinct()
# 
# # Validity checks
# data <- data %>%
#   filter(age >= 0)  # Example: Ensuring age is non-negative

#########################
# Splitting the Dataset #
#########################

# set.seed(123)  # for reproducibility
# train_indices <- sample(1:nrow(data), 0.7 * nrow(data))
# train_data <- data[train_indices, ]
# test_data <- data[-train_indices, ]





