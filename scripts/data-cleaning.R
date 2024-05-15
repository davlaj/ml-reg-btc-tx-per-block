# Load packages
source("setup-packages.R")

# Load data 
raw_data <- read.csv("/data/raw/block_data.csv")

#####################
# 0. PRE-FORMATTING #
#####################

data_formatting <- function (input_file){
  
  data <- input_file
  # Convert the 'Date' column to POSIXct date-time objects
  data$Date <- as.POSIXct(data$Date, origin="1970-01-01", tz="UTC")
  return(data)
}
#formatted_data <- data_formatting(raw_data)

##################
# 1.Missing Data #
##################

data_missing_values <- function (input_data){
  
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
#data_with_no_missing_values <- data_missing_values(formatted_data)

##############
# 2.Outliers #
##############

data_outliers <- function (input_data){
  
  data <- input_data
  
  # DETECTION
  
  # Reshape the data and create separate boxplots for all numeric columns
  plot <- data %>%
    select(where(is.numeric)) %>%  # Select only numeric columns
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%  # Reshape data
    ggplot(aes(x = "", y = Value)) +  # Setup axes
    geom_boxplot() +  # Create boxplots
    facet_wrap(~ Variable, scales = "free_y") +  # Separate plot for each variable with free y scale
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +  # Remove x-axis elements
    labs(y = "Values", title = "Separate Boxplot for Each Numeric Variable")  # Label the y-axis and provide a title
  
  # Save the plot to a file
  ggsave("/data-cleaning/outliers-boxplot.pdf", plot, width = 10, height = 8)
  
  # TREATMENT (Trimming, Capping)
  
  # # Trimming (removing)
  # quantiles <- quantile(data$numeric_column, probs=c(.25, .75), na.rm = TRUE)
  # iqr <- IQR(data$numeric_column, na.rm = TRUE)
  # data <- data %>%
  #   filter(numeric_column >= (quantiles[1] - 1.5 * iqr) & numeric_column <= (quantiles[2] + 1.5 * iqr))
  
  return(data)
}
#data_outliers_treated <- data_outliers(data_with_no_missing_values)

# #########################
# # 3.Feature Engineering #
# #########################
# 
# # TEMPORAL FEATURES
# 
# # Ensure data is sorted by the 'Date' column
# data <- data %>%
#   arrange(Date)
# 
# ## Calculate the time difference in minutes between consecutive blocks
# data <- data %>%
#   mutate(TimeDiffMinutes = c(NA, diff(Date) / 60))  # 'diff' calculates the difference between consecutive elements
# ## Hour of the day
# data$HourOfDay <- hour(data$Date)
# ## Day of the week
# # Returns numbers where 1 = Sunday, 2 = Monday, ..., 7 = Saturday
# data$DayOfWeek <- wday(data$Date) 
# ## Whether the day is a weekday or weekend
# data$IsWeekend <- ifelse(data$DayOfWeek %in% c(1, 7), 1, 0)
# 
# # Create lagged features to retrieve the value of the previous block
# data$TotalTransactions_Lag1 <- dplyr::lag(data$TotalTransactions, 1)
# data$BlockSize_Lag1 <- dplyr::lag(data$BlockSize, 1)
# data$AverageFee_Lag1 <- dplyr::lag(data$AverageFee, 1)
# data$TotalFees_Lag1 <- dplyr::lag(data$TotalFees, 1)
# # Create running average features
# # Calculate running average of the previous 3 blocks including the current block
# data$TotalTransactions_RunningAvg3 <- rollmean(data$TotalTransactions, k = 3, fill = NA, align = "right")
# # Shift forward by one block so the running average is for the previous 3 blocks excluding the current one
# data$TotalTransactions_RunningAvg3 <- dplyr::lag(data$TotalTransactions_RunningAvg3, 1, default = NA)
# data$BlockSize_RunningAvg3 <- rollmean(data$BlockSize, k = 3, fill = NA, align = "right")
# data$BlockSize_RunningAvg3 <- dplyr::lag(data$BlockSize_RunningAvg3, 1, default = NA)
# data$AverageFee_RunningAvg3 <- rollmean(data$AverageFee, k = 3, fill = NA, align = "right")
# data$AverageFee_RunningAvg3 <- dplyr::lag(data$AverageFee_RunningAvg3, 1, default = NA)
# data$TotalFees_RunningAvg3 <- rollmean(data$TotalFees, k = 3, fill = NA, align = "right")
# data$TotalFees_RunningAvg3 <- dplyr::lag(data$TotalFees_RunningAvg3, 1, default = NA)
# 
# # Check for NA values and decide on deleting these rows
# summary(data)
# head(data)
# data <- data[-(1:3),]
# 
# # INTERACTION FEATURES
# 
# # Transaction volumes might vary by time of day differently on weekends versus weekdays
# data$Interaction_HourWeekend <- with(data, HourOfDay * IsWeekend)
# # To examine if larger blocks, which can include more transactions and potentially higher fees, 
# # also correlate with higher average fees in a way that isn't linearly predictable from either 
# # block size or average fees alone.
# # This could reflect situations where larger blocks are processed with priority due to higher fees 
# # being included, influencing the transaction count in those blocks differently than smaller blocks.
# data$Interaction_BlockSizeFee <- with(data, BlockSize_Lag1 * AverageFee_Lag1)
# 
# # POLYNOMIAL FEATURES
# 
# # Block size might have a non-linear effect on the number of transactions
# data$BlockSize_Squared <- with(data, BlockSize_Lag1^2)
# data$BlockSize_Cubed <- with(data, BlockSize_Lag1^3)
# # Quadratic or cubic terms for HourOfDay could capture morning rush, midday lull, and evening peaks 
# data$HourOfDay_Squared <- with(data, HourOfDay^2)
# data$HourOfDay_Cubed <- with(data, HourOfDay^3)
# 
# # VIZUALIZATION OF ENGINEERED FEATURES TO SEE CORRELATION WITH DEPENDENT VARIABLE
# 
# # Define the function to create multiple scatter plots
# plot_scatter_plots <- function(data, var_list) {
#   plots <- list()  # Initialize list to store ggplot objects
#   
#   for (var_name in var_list) {
#     p <- ggplot(data, aes_string(x = var_name, y = "TotalTransactions")) +
#       geom_point(alpha = 0.5) +
#       geom_smooth(method = "lm", color = "blue") +
#       labs(title = paste("Total Transactions vs.", var_name),
#            x = var_name,
#            y = "Total Transactions")
#     plots[[length(plots) + 1]] <- p  # Append plot to list
#   }
#   
#   # Arrange and display all plots
#   do.call(grid.arrange, c(plots, ncol = 2))
# }
# 
# # Call the function with the list of variables you want to plot
# plot_scatter_plots(data, c("TotalTransactions_Lag1", "TotalTransactions_RunningAvg3",
#                            "Interaction_BlockSizeFee", "BlockSize_Cubed"))
# 
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
# data_processed <- data_encoded %>% distinct()
# 
# # VALIDITY CHECKS
# summary(data_processed)
# 

