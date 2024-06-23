# Data cleaning, transforming and feature engineering

# Load packages
source("setup-packages.R")
source("scripts/1-exploratory-data-analysis.R")

# Load data 
raw_data <- read.csv("data/raw/block_data.csv")

# 0.Formatting
formatting <- function (input_file){
  
  data <- input_file
  
  # Remove duplicate rows
  data <- data %>% distinct()
  
  # Convert the 'Date' column to POSIXct date-time objects
  data$Date <- as.POSIXct(data$Date, origin="1970-01-01", tz="UTC")
  
  return(data)
}
#data_formatted <- formatting(raw_data)
#analyze_post_formatting(data_formatted)

# 1.Missing Values 
missing_values <- function (input_data){
  
  # Initial feature selection
  data <- input_data[, !names(input_data) %in% c("")]
  
  # DECIDE ON STRATEGY (Deletion, Imputation)
  
  # AverageFee: 28 blocks with missing values.  
  missing_AverageFee_rows <- data[is.na(data$AverageFee), ]
  
  output_dir <- "output/exploratory-data-analysis"
  # Open a text file for writing summary statistics
  sink(paste0(output_dir, "/1.0-missing_rows_averageFee.txt"))
  cat("ROWS WITH MISSING VALUES\n")
  cat("-----------------\n")
  print(missing_AverageFee_rows) 
  sink()  # Stop diverting the output to the file
  
  # Strategy: Imputation -> Replace by 0
  # These are blocks with only 1 tx, the coinbase tx which doesn't have any fee
  data$AverageFee <- ifelse(is.na(data$AverageFee), 0, data$AverageFee)
  
  return(data)
}
#data_without_missing_values <- missing_values(data_formatted)
#analyze_post_missing_values(data_without_missing_values, data_before = data_formatted)

# 2.Temporal Feature Engineering 
temporal_feature <- function(input_data) {
  # To avoid introducing inaccuracies or distortions in temporal calculations, which can occur due to gaps
  # in the data after the removal of outliers and would create new outliers

  data <- input_data
    
  # TEMPORAL FEATURES
  
  # Ascending sort by Date
  data <- data %>%
    arrange(Date)
  
  ## Time between consecutive blocks in minutes
  data <- data %>%
    mutate(TimeDiffMinutes = c(NA, diff(Date) / 60))
  ## Hour of the day
  data$HourOfDay <- hour(data$Date)
  ## Day of the week
  # 1 = Sunday, 2 = Monday, ..., 7 = Saturday
  data$DayOfWeek <- wday(data$Date)
  ## Weekday = 0, Weekend = 1
  data$IsWeekend <- ifelse(data$DayOfWeek %in% c(1, 7), 1, 0)
  
  ## Log-Transformation features
  # Reduces the effect of outliers, reduces skewness in data
  # Handling right-skewed data by compressing large values more than smaller ones, bringing the data closer to a normal distribution
  data$TotalFeesLog <- log1p(data$TotalFees)
  
  ## Lagged features to retrieve the value of the previous block
  data$TotalTransactions_Lag1 <- dplyr::lag(data$TotalTransactions, 1)
  data$BlockSize_Lag1 <- dplyr::lag(data$BlockSize, 1)
  data$TotalFees_Lag1 <- dplyr::lag(data$TotalFees, 1)
  data$TotalFeesLog_Lag1 <- dplyr::lag(data$TotalFeesLog, 1)
  
  ## Running average features
  # Loop over each column and window size
  for (col in c("TotalTransactions","BlockSize", "TotalFees", "TotalFeesLog")) {
    for (k in 2:3) {
      # Running average including the current block
      running_avg <- rollmean(data[[col]], k = k, fill = NA, align = "right")
      # Shift the running average forward by one block so it exclude the current block
      running_avg <- lag(running_avg, 1, default = NA)
      
      # Create a new column name based on the original column name and k
      new_col_name <- paste(col, "_RunningAvg", k, sep = "")
      # Assign the running average to a new column in the data
      data[[new_col_name]] <- running_avg
    }
  }
  
  # Check for NA values and decide on deleting these rows
  data <- data[-(1:3),]

}
#data_with_temporal_feature <- temporal_feature(data_without_missing_values)
#analyze_temporal_features(data_with_temporal_feature, c("TotalTransactions", "BlockSize", "AverageFee", "TotalFees"), log_transform = FALSE)

# 3.Outliers 
outliers <- function(input_data, outliers_to_remove = TRUE) {
  
  data <- input_data

  # TREATMENT (Clipping/Truncation, Transformation, Imputation, Removal)
  
  # Check if outliers_to_remove is set to FALSE, then exit early
  if (is.logical(outliers_to_remove) && !outliers_to_remove) {
    return(data)  # Return original data without any changes
  }
  
  # If outliers_to_remove is set to TRUE
  if (is.logical(outliers_to_remove) && outliers_to_remove) {
    outliers_to_remove <- names(data)[sapply(data, is.numeric)]
  # If outliers_to_remove is a list of variables
  } else if (is.character(outliers_to_remove)) {
    valid_columns <- outliers_to_remove %in% names(data)
    if (any(!valid_columns)) {
      warning("The following columns do not exist:", paste(outliers_to_remove[!valid_columns], collapse = ", "))
      outliers_to_remove <- outliers_to_remove[valid_columns]
    }
    if (length(outliers_to_remove) == 0) {
      stop("None of the specified columns exist in the data.")
    }
  } else {
    stop("Invalid value for outliers_to_remove. It should be TRUE, FALSE, or a character vector of column names.")
  }
  
  # Apply outlier removal for the specified columns
  for (col in outliers_to_remove) {
    quantiles <- quantile(data[[col]], probs = c(0.25, 0.75), na.rm = TRUE)
    iqr <- IQR(data[[col]], na.rm = TRUE)
    lower_bound <- quantiles[1] - 1.5 * iqr
    upper_bound <- quantiles[2] + 1.5 * iqr
    data <- data %>% filter(data[[col]] >= lower_bound & data[[col]] <= upper_bound)
  }
  
  return(data)
}
#data_outliers_treated <- outliers(data_with_temporal_feature, outliers_to_remove = FALSE) # c("AverageFee")
#analyze_outliers(data_pre = data_with_temporal_feature, data_post = data_outliers_treated)

# 4.Interaction and Polynomial Feature Engineering 
interaction_polynomial_feature <- function(input_data) {

  data <- input_data

  # INTERACTION FEATURES

  # Transaction volumes might vary by time of day differently on weekends versus weekdays
  data$Interaction_HourIsWeekend <- with(data, HourOfDay * IsWeekend)

  # POLYNOMIAL FEATURES

  # Block size might have a non-linear effect on the number of transactions
  data$BlockSize_Squared <- with(data, BlockSize_Lag1^2)
  data$BlockSize_Cubed <- with(data, BlockSize_Lag1^3)
  # Quadratic or cubic terms for HourOfDay could capture morning rush, midday lull, and evening peaks
  data$HourOfDay_Squared <- with(data, HourOfDay^2)
  data$HourOfDay_Cubed <- with(data, HourOfDay^3)

  return(data)
}
#data_full_feature_engineered <- interaction_polynomial_feature(data_outliers_treated)
#analyze_interaction_polynomial_features(data_full_feature_engineered, "TotalTransactions", c("Interaction_HourIsWeekend", "BlockSize_Squared", "BlockSize_Cubed", "HourOfDay_Squared", "HourOfDay_Cubed"))

# 5.Encoding # Apply to categorical variables
encoding <- function(input_data) {
  # WHICH ALGORITHMS NEED ENCODING?
  
  data <- input_data

  ## ONE-HOT ENCODING (Apply to nominal data, e.g. blue, green, etc)
  # none
  
  # # Convert HourOfDay and DayOfWeek to factors to ensure model.matrix recognizes them as categorical variables
  # data$HourOfDay <- factor(data$HourOfDay)
  # data$DayOfWeek <- factor(data$DayOfWeek)
  # # model.matrix will by default drop the first dummy variable of each category if the index start at 1
  # # DayOfWeek starts at 1, but HourOfDay start at 0, so we need to drop it to avoid the Dummy Variable Trap
  # onehot_encoded <- model.matrix(~ HourOfDay + DayOfWeek - 1, data)
  # data <- cbind(data, onehot_encoded)
  # data <- data %>% select(-HourOfDay0)  # Remove the HourOfDay0 column
  # # Convert back DayOfWeek and HourOfDay to numeric format
  # data$DayOfWeek <- as.numeric(as.character(data$DayOfWeek))
  # data$HourOfDay <- as.numeric(as.character(data$HourOfDay))
  
  ## TARGET ENCODING (See StatQuest -> For when one-hot encoding would create to many dummy variables)
  # none
  
  ## LABEL ENCODING (Apply to ordinal data, e.g. High School, Masters, PhD, etc)
  # none
  
  ## CYCLICAL ENCODING (Apply to data with a cyclical nature, e.g. days of the week)
  
  data$hour_sin <- sin(2 * pi * data$HourOfDay / 24)
  data$hour_cos <- cos(2 * pi * data$HourOfDay / 24)
  data$day_sin <- sin(2 * pi * data$DayOfWeek / 7)
  data$day_cos <- cos(2 * pi * data$DayOfWeek / 7)
  
  return(data) 
}
#data_encoded <- encoding(data_full_feature_engineered) 
#analyze_encoding_effects(data_encoded)

###########################
# 6.Data Integrity Checks #
###########################

# DUPLICATES
# Remove duplicate rows
#data_preprocessed <- data_encoded %>% distinct()

# VALIDITY CHECKS
#summary(data_preprocessed)


