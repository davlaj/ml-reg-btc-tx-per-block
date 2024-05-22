# Load packages
source("setup-packages.R")

# Load data 
raw_data <- read.csv("data/raw/block_data.csv")

#################
# 0. Formatting #
#################

# Includes:
# Handling missing values (e.g., filling in, imputing, or removing missing data)
# Removing duplicates
# Correcting errors and inconsistencies (e.g., fixing typos, standardizing formats)
# Filtering out outliers or irrelevant data
# Ensuring data types are correct (e.g., converting strings to integers)

formatting <- function (input_file){
  
  data <- input_file
  # Convert the 'Date' column to POSIXct date-time objects
  data$Date <- as.POSIXct(data$Date, origin="1970-01-01", tz="UTC")
  return(data)
}
#data_formatted <- formatting(raw_data)

####################
# 1.Missing Values #
####################

missing_values <- function (input_data){
  
  data <- input_data
  
  # IDENTIFYING MISSING DATA & INCONSISTENCIES
  print(summary(data))
  
  # DECIDE ON STRATEGY (Deletion, Imputation)
  
  # TimeDiffMinutes: First row has missing value. 
  # Strategy: Deletion
  data <- data[-1,]
  
  # AverageFee: 28 blocks with missing values.  
  # These are blocks with only 1 tx, the coinbase tx which doesn't have any fee
  missing_averageFee_rows <- data[is.na(data$AverageFee), ]
  
  print(missing_averageFee_rows) 
  
  # Strategy: Imputation -> Replace by 0
  data$AverageFee <- ifelse(is.na(data$AverageFee), 0, data$AverageFee)
  
  return(data)
}
#data_without_missing_values <- missing_values(data_formatted)

##################################
# 2.Temporal Feature Engineering #
##################################
# To avoid introducing inaccuracies or distortions in temporal calculations, which can occur due to gaps
# in the data after the removal of outliers and would create new outliers

temporal_feature <- function(input_data) {

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
  
  ## Lagged features to retrieve the value of the previous block
  data$TotalTransactions_Lag1 <- dplyr::lag(data$TotalTransactions, 1)
  data$BlockSize_Lag1 <- dplyr::lag(data$BlockSize, 1)
  data$AverageFee_Lag1 <- dplyr::lag(data$AverageFee, 1)
  data$TotalFees_Lag1 <- dplyr::lag(data$TotalFees, 1)
  
  ## Running average features
  # Loop over each column and window size
  for (col in c("TotalTransactions","BlockSize", "AverageFee", "TotalFees")) {
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
  summary(data)
  head(data)
  data <- data[-(1:3),]

}
#data_with_temporal_feature <- temporal_feature(data_without_missing_values)

##############
# 3.Outliers #
##############

# Seems to work, but all y-axis adjust in function of others. Should when have separate plot? CAN SEE LATER, MAYBE AS A SECOND LAYER (i.e. sub folder with individual plots)

outliers <- function(input_data, outliers_to_remove = TRUE) {
  
  data <- input_data
  
  # DETECTION: Create and save boxplots of numeric columns
  plot_with_outliers <- data %>%
    select(where(is.numeric)) %>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
    ggplot(aes(x = "", y = Value)) +
    geom_boxplot() +
    facet_wrap(~ Variable, scales = "free_y") +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
    labs(y = "Values", title = "Boxplots with outliers")
  
  # Save the initial boxplot
  ggsave("output/data-cleaning/3.1-with-outliers-boxplot.pdf", plot_with_outliers, width = 10, height = 8)
  
  # TREATMENT (Trimming or capping)
  
  # Check if outliers removal is set to FALSE, then exit early
  if (is.logical(outliers_to_remove) && !outliers_to_remove) {
    # Attempt to delete the graph 3.1-without-outliers-boxplot if it exists
    file_path <- "output/data-cleaning/3.2-without-outliers-boxplot.pdf"
    if (file.exists(file_path)) {
      file_removed <- file.remove(file_path)
    }
    return(data)  # Return original data without any changes
  }
  
  # Process based on type of outliers_to_remove
  if (is.logical(outliers_to_remove) && outliers_to_remove) {
    outliers_to_remove <- names(data)[sapply(data, is.numeric)]
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
  
  # Create and save boxplots without outliers
  plot_without_outliers <- data %>%
    select(where(is.numeric)) %>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
    ggplot(aes(x = "", y = Value)) +
    geom_boxplot() +
    facet_wrap(~ Variable, scales = "free_y") +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
    labs(y = "Values", title = "Boxplots without outliers")
  
  ggsave("output/data-cleaning/3.2-without-outliers-boxplot.pdf", plot_without_outliers, width = 10, height = 8)
  
  # #SCATTER PLOTS
  
  return(data)
}
#data_outliers_treated <- outliers(data_with_temporal_feature, outliers_to_remove = FALSE) # c("AverageFee")

####################################################
# 4.Interaction and Polynomial Feature Engineering #
####################################################

interaction_polynomial_feature <- function(input_data) {

  data <- input_data

  # INTERACTION FEATURES

  # Transaction volumes might vary by time of day differently on weekends versus weekdays
  data$Interaction_HourIsWeekend <- with(data, HourOfDay * IsWeekend)
  # To examine if larger blocks, which can include more transactions and potentially higher fees,
  # also correlate with higher average fees in a way that isn't linearly predictable from either
  # block size or average fees alone.
  # This could reflect situations where larger blocks are processed with priority due to higher fees
  # being included, influencing the transaction count in those blocks differently than smaller blocks.
  data$Interaction_BlockSizeAvgFee <- with(data, BlockSize_Lag1 * AverageFee_Lag1)

  # POLYNOMIAL FEATURES

  # Block size might have a non-linear effect on the number of transactions
  data$BlockSize_Squared <- with(data, BlockSize_Lag1^2)
  data$BlockSize_Cubed <- with(data, BlockSize_Lag1^3)
  # Quadratic or cubic terms for HourOfDay could capture morning rush, midday lull, and evening peaks
  data$HourOfDay_Squared <- with(data, HourOfDay^2)
  data$HourOfDay_Cubed <- with(data, HourOfDay^3)

  # VISUALIZATION

  # Define the function to create multiple scatter plots
  plot_scatter_plots <- function(data, var_list) {
    plots <- list()  # Initialize to store ggplot objects

    for (var_name in var_list) {
      p <- ggplot(data, aes(x = .data[[var_name]], y = .data[["TotalTransactions"]])) +
        geom_point(alpha = 0.5) +
        geom_smooth(method = "lm", color = "blue") +
        labs(title = paste("Total Transactions vs.", var_name),
             x = var_name,
             y = "Total Transactions")

      plots[[length(plots) + 1]] <- p  # Append plot to list
    }

    # Determine number of plots per page
    plots_per_page <- 6  # 2x2 layout
    page_number <- ceiling(length(plots) / plots_per_page)

    # Open a PDF device
    pdf("output/data-cleaning/4.1-full_feature_engineered_scatter_plots.pdf", width = 20, height = 15)

    for (i in seq_len(page_number)) {
      # Subset plots for the current page
      slice_start <- (i - 1) * plots_per_page + 1
      slice_end <- min(i * plots_per_page, length(plots))
      page_plots <- plots[slice_start:slice_end]
      page_plots_layout <- do.call(patchwork::wrap_plots, c(page_plots, ncol = 3))

      print(page_plots_layout)
    }

    # Close the PDF device
    dev.off()
  }

  # Filter data to include only numeric columns
  numeric_data <- select(data, where(is.numeric))

  # Get the names of all numeric columns
  numeric_columns <- names(numeric_data)

  # Call the function with the list of variables you want to plot
  plot_scatter_plots(data, numeric_columns)

  return(data)
}
#data_full_feature_engineered <- interaction_polynomial_feature(data_outliers_treated)

# ##############
# # 4.Encoding # Apply to categorical variables
# ##############
# 
# ## ONE-HOT ENCODING (Apply to nominal data, e.g. blue, green, etc)
# 
# # Convert HourOfDay and DayOfWeek to factors to ensure model.matrix recognizes them as categorical variables
# data$HourOfDay <- factor(data$HourOfDay)
# data$DayOfWeek <- factor(data$DayOfWeek)
# # model.matrix will by default drop the first dummy variable of each category if the index start at 1
# # DayOfWeek starts at 1, but HourOfDay start at 0, so we need to drop it to avoid the Dummy Variable Trap
# onehot_encoded <- model.matrix(~ HourOfDay + DayOfWeek - 1, data) 
# data_encoded_onehot <- cbind(data, onehot_encoded)
# data_encoded_onehot <- data_encoded_onehot %>% select(-HourOfDay0)  # Remove the HourOfDay0 column
# # Check the structure to confirm changes
# str(data_encoded_onehot)
# # Convert back DayOfWeek and HourOfDay to numeric format 
# data_encoded_onehot$DayOfWeek <- as.numeric(as.character(data_encoded_onehot$DayOfWeek))
# data_encoded_onehot$HourOfDay <- as.numeric(as.character(data_encoded_onehot$HourOfDay))
# 
# ## LABEL ENCODING (Apply to ordinal data, e.g. High School, Masters, PhD, etc)
# # none
# 
# ## CYCLICAL ENCODING (Apply to data with a cyclical nature, e.g. days of the week)
# 
# data_encoded_onehot$hour_sin <- sin(2 * pi * data_encoded_onehot$HourOfDay / 24)
# data_encoded_onehot$hour_cos <- cos(2 * pi * data_encoded_onehot$HourOfDay / 24)
# data_encoded_onehot$day_sin <- sin(2 * pi * data_encoded_onehot$DayOfWeek / 7)
# data_encoded_onehot$day_cos <- cos(2 * pi * data_encoded_onehot$DayOfWeek / 7)
# data_encoded <- data_encoded_onehot
# head(data_encoded)
# 
# ###########################
# # 5.Data Integrity Checks #
# ###########################
# 
# # DUPLICATES
# # Remove duplicate rows
# data_preprocessed <- data_encoded %>% distinct()
# 
# # VALIDITY CHECKS
# summary(data_preprocessed)
# 

