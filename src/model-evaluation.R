# # Install packages if they are not already installed
# if (!require("dplyr")) install.packages("dplyr")
# if (!require("ggplot2")) install.packages("ggplot2")
# if (!require("tidyr")) install.packages("tidyr")
# if (!require("lubridate")) install.packages("lubridate", dependencies=TRUE)
# if (!require("purrr")) install.packages("purrr")
# if (!require("zoo")) install.packages("zoo")
# if (!require("caret")) install.packages("caret")
# if (!require("timetk")) install.packages("timetk") # for time series manipulation and visualization
# if (!require("modeltime")) install.packages("modeltime") # for time series modeling and cross-validation
# 
# # Load packages
# library(dplyr)
# library(ggplot2)
# library(tidyr)
# library(lubridate)
# library(purrr)
# library(zoo)
# library(caret)
# library(timetk)  
# library(modeltime)  

####################
# 1.Data Splitting #
####################

# Setting up the time series cross-validation
set.seed(123)  # for reproducibility
training_rows <- createTimeSlices(
  1:nrow(data),  # The total number of rows in the data 
  initialWindow = round(nrow(data) * 0.8),  # Initial training set size
  horizon = 1,  # Forecast horizon of 1 period ahead
  fixedWindow = TRUE,  # Whether to keep the training size constant
  skips = 0  # No skips between slices
)$train

# The output of createTimeSlices is a list of indices for training sets ($train)
# [[1]] corresponds to the indices of the first fold's training data
train_data <- data[training_rows[[1]], ]
test_data <- data[-training_rows[[1]], ]

# # Data splitting
# train_indices <- sample(1:nrow(data_processed), 0.7 * nrow(data_processed))
# train_data <- data_processed[train_indices, ]
# test_data <- data_processed[-train_indices, ]

#########################
# 2.DIMENSION REDUCTION #
#########################

# PCA, Heatmaps, t-SNE plots, Multidimensional Scaling (MDS) plots

#############
# 3.Scaling # Apply to continuous numeric non-categorical variables
#############

# STANDARDIZATION (Z-SCORE SCALING)

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

#######################
# 4. MODEL EVALUATION #
#######################












