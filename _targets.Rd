# Code that generated _targets.R

# Define the pipeline
tar_script({
  
  # Load packages
  source("setup-packages.R")
  
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
