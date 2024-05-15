# Code that generated _targets.R

# Define the pipeline
tar_script({
  
  # Set it to your project root if it's not already set
  setwd("/Users/github/bitcoin-transactions-per-block") # GET RID OF THIS FOR REPRODUCIBILITY
  
  # Install packages if they are not already installed
  if (!require("dplyr")) install.packages("dplyr")
  if (!require("ggplot2")) install.packages("ggplot2")
  if (!require("tidyr")) install.packages("tidyr")
  if (!require("lubridate")) install.packages("lubridate", dependencies=TRUE)
  if (!require("purrr")) install.packages("purrr")
  if (!require("zoo")) install.packages("zoo")
  if (!require("caret")) install.packages("caret")
  if (!require("timetk")) install.packages("timetk") # for time series manipulation and visualization
  if (!require("modeltime")) install.packages("modeltime") # for time series modeling and cross-validation
  if (!require("targets")) install.packages("targets")
  
  # Load packages
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(lubridate)
  library(purrr)
  library(zoo)
  library(caret)
  library(timetk)  
  library(modeltime)  
  library(targets)
  
  # Source the scripts
  source("scripts/data-cleaning.R")
  
  list(
    tar_target(
      dataFormatted,
      data_formatting(raw_data)
    ),
    tar_target(
      dataMissingValues,
      data_missing_values(dataFormatted)
    ),
    tar_target(
      dataOutliers,
      data_outliers(dataMissingValues)
    )
  )
})
