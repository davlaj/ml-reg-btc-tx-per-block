# Install packages if they are not already installed
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("tidyr")) install.packages("tidyr")
if (!require("lubridate")) install.packages("lubridate", dependencies=TRUE)
if (!require("purrr")) install.packages("purrr")
if (!require("zoo")) install.packages("zoo")

# Load packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(purrr)
library(zoo)

# Load data 
data <- read.csv("/Users/github/bitcoin-transactions-per-block/data/raw/block_data.csv")

#####################
# 1.Data Formatting #
#####################

# FORMATTING

# Convert the 'Date' column to POSIXct date-time objects
data$Date <- as.POSIXct(data$Date, origin="1970-01-01", tz="UTC")

# DETECTING INCONSISTENCIES

summary(data)
# none

# ADDING NEW COLUMNS

## Ensure data is sorted by the 'Date' column
data <- data %>%
  arrange(Date)
## Calculate the time difference in minutes between consecutive blocks
data <- data %>%
  mutate(TimeDiffMinutes = c(NA, diff(Date) / 60))  # 'diff' calculates the difference between consecutive elements
## Hour of the day
data$HourOfDay <- hour(data$Date)
## Day of the week
# Returns numbers where 1 = Sunday, 2 = Monday, ..., 7 = Saturday
data$DayOfWeek <- wday(data$Date) 
## Whether the day is a weekday or weekend
data$IsWeekend <- ifelse(data$DayOfWeek %in% c(1, 7), 1, 0)

# View the updated dataframe to confirm changes
summary(data)

##################
# 2.Missing Data #
##################

# IDENTIFYING MISSING DATA

summary(data)

# DECIDE ON STRATEGY (Deletion, Imputation)

# TimeDiffMinutes: First row has missing value. 
# Strategy: Deletion
data <- data[-1,]

# 28 blocks with missing AverageFee.  
# These are blocks with only 1 tx, the coinbase tx which doesn't have any fee
missing_averageFee_rows <- data[is.na(data$AverageFee), ]
print(missing_averageFee_rows) 
# Strategy: Imputation -> Replace by 0
data$AverageFee <- ifelse(is.na(data$AverageFee), 0, data$AverageFee)

##############
# 3.Outliers #
##############

# DETECTION

# Reshape the data and create separate boxplots for all numeric columns
data %>%
  select(where(is.numeric)) %>%  # Select only numeric columns
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%  # Reshape data
  ggplot(aes(x = "", y = Value)) +  # Setup axes
  geom_boxplot() +  # Create boxplots
  facet_wrap(~ Variable, scales = "free_y") +  # Separate plot for each variable with free y scale
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +  # Remove x-axis elements
  labs(y = "Values", title = "Separate Boxplot for Each Numeric Variable")  # Label the y-axis and provide a title

# TREATMENT (Trimming, Capping)

# # Trimming (removing)
# quantiles <- quantile(data$numeric_column, probs=c(.25, .75), na.rm = TRUE)
# iqr <- IQR(data$numeric_column, na.rm = TRUE)
# data <- data %>%
#   filter(numeric_column >= (quantiles[1] - 1.5 * iqr) & numeric_column <= (quantiles[2] + 1.5 * iqr))

#########################
# 4.Feature Engineering #
#########################

# TEMPORAL FEATURES

# Ensure data is sorted by the 'Date' column
data <- data %>%
  arrange(Date)
# Create lagged features to retrieve the value of the previous block
data$TotalTransactions_Lag1 <- dplyr::lag(data$TotalTransactions, 1)
data$BlockSize_Lag1 <- dplyr::lag(data$BlockSize, 1)
data$AverageFee_Lag1 <- dplyr::lag(data$AverageFee, 1)
data$TotalFees_Lag1 <- dplyr::lag(data$TotalFees, 1)
# Create running average features
# Calculate running average of the previous 3 blocks including the current block
data$TotalTransactions_RunningAvg3 <- rollmean(data$TotalTransactions, k = 3, fill = NA, align = "right")
# Shift forward by one block so the running average is for the previous 3 blocks excluding the current one
data$TotalTransactions_RunningAvg3 <- dplyr::lag(data$TotalTransactions_RunningAvg3, 1, default = NA)
data$BlockSize_RunningAvg3 <- rollmean(data$BlockSize, k = 3, fill = NA, align = "right")
data$BlockSize_RunningAvg3 <- dplyr::lag(data$BlockSize_RunningAvg3, 1, default = NA)
data$AverageFee_RunningAvg3 <- rollmean(data$AverageFee, k = 3, fill = NA, align = "right")
data$AverageFee_RunningAvg3 <- dplyr::lag(data$AverageFee_RunningAvg3, 1, default = NA)
data$TotalFees_RunningAvg3 <- rollmean(data$TotalFees, k = 3, fill = NA, align = "right")
data$TotalFees_RunningAvg3 <- dplyr::lag(data$TotalFees_RunningAvg3, 1, default = NA)

# Check for NA values and decide on deleting these rows
summary(data)
head(data)
data <- data[-(1:3),]

# INTERACTION FEATURES

# Transaction volumes might vary by time of day differently on weekends versus weekdays
data$Interaction_HourWeekend <- with(data, HourOfDay * IsWeekend)
# To examine if larger blocks, which can include more transactions and potentially higher fees, 
# also correlate with higher average fees in a way that isn't linearly predictable from either 
# block size or average fees alone.
# This could reflect situations where larger blocks are processed with priority due to higher fees 
# being included, influencing the transaction count in those blocks differently than smaller blocks.
data$Interaction_BlockSizeFee <- with(data, BlockSize_Lag1 * AverageFee_Lag1)

# POLYNOMIAL FEATURES

# Block size might have a non-linear effect on the number of transactions
data$BlockSize_Squared <- with(data, BlockSize_Lag1^2)
data$BlockSize_Cubed <- with(data, BlockSize_Lag1^3)
# Quadratic or cubic terms for HourOfDay could capture morning rush, midday lull, and evening peaks 
data$HourOfDay_Squared <- with(data, HourOfDay^2)
data$HourOfDay_Cubed <- with(data, HourOfDay^3)


# VIZUALIZATION OF ENGINEERED FEATURES TO SEE CORRELATION WITH DEPENDENT VARIABLE

# Scatter plot for Lagged Transactions
ggplot(data, aes(x = TotalTransactions_Lag1, y = TotalTransactions)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Total Transactions vs. Lag 1 Transactions",
       x = "Lagged Transactions (lag 1)",
       y = "Total Transactions")

# Scatter plot for Running Average of Transactions
ggplot(data, aes(x = TotalTransactions_RunningAvg3, y = TotalTransactions)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Total Transactions vs. Running Average of Transactions (3 blocks)",
       x = "Running Average Transactions (3 blocks)",
       y = "Total Transactions")

# Scatter plot for BlockSize and Fee Interaction
ggplot(data, aes(x = Interaction_BlockSizeFee, y = TotalTransactions)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Total Transactions vs. Interaction of BlockSize and AverageFee",
       x = "BlockSize * AverageFee",
       y = "Total Transactions")

# Scatter plot for Polynomial Feature of BlockSize
ggplot(data, aes(x = BlockSize_Cubed, y = TotalTransactions)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Total Transactions vs. Cubed BlockSize",
       x = "BlockSize Cubed",
       y = "Total Transactions")

##########################
# 5.Scaling and Encoding #
##########################

# STANDARDIZATION (Z-SCORE SCALING) (Apply to continuous numeric non-categorical variables)

numeric_cols_to_scale <- names(data)[sapply(data, is.numeric) & !names(data) %in% c("TotalTransactions", "BlockNumber", "HourOfDay", "DayOfWeek", "IsWeekend")]
data_scaled <- data %>%
  mutate(across(all_of(numeric_cols_to_scale), scale))

# Boxplot after scaling
data_scaled %>%
  select(where(function(x) is.numeric(x)), -c(TotalTransactions, BlockNumber, HourOfDay, DayOfWeek, IsWeekend)) %>%  # Select only numeric columns
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%  # Reshape data
  ggplot(aes(x = "", y = Value)) +  # Setup axes
  geom_boxplot() +  # Create boxplots
  facet_wrap(~ Variable, scales = "free_y") +  # Separate plot for each variable with free y scale
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +  # Remove x-axis elements
  labs(y = "Values", title = "Separate Boxplot for Each Non-Categorical Numeric Variables after Scaling")  # Label the y-axis and provide a title

summary(data_scaled)

# ENCODING (Apply to categorical variables)

## One-hot encoding (Apply to nominal data, e.g. blue, green, etc)

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


## Label encoding (Apply to ordinal data, e.g. High School, Masters, PhD, etc)

# none

## Cyclical encoding (Apply to data with a cyclical nature, e.g. days of the week)

data_encoded_onehot$hour_sin <- sin(2 * pi * data_encoded_onehot$HourOfDay / 24)
data_encoded_onehot$hour_cos <- cos(2 * pi * data_encoded_onehot$HourOfDay / 24)
data_encoded_onehot$day_sin <- sin(2 * pi * data_encoded_onehot$DayOfWeek / 7)
data_encoded_onehot$day_cos <- cos(2 * pi * data_encoded_onehot$DayOfWeek / 7)
data_encoded <- data_encoded_onehot
head(data_encoded)

############################
# DIMENSIONALITY REDUCTION #
############################

# none

###########################
# 6.Data Integrity Checks #
###########################

# DUPLICATES

# Remove duplicate rows
data_processed <- data_encoded %>% distinct()

# VALIDITY CHECKS

summary(data_processed)

####################
# 7.Data Splitting #
####################

set.seed(123)  # for reproducibility
train_indices <- sample(1:nrow(data_processed), 0.7 * nrow(data_processed))
train_data <- data_processed[train_indices, ]
test_data <- data_processed[-train_indices, ]





