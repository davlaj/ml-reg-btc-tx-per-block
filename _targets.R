source("setup-packages.R")
source("scripts/data-preprocessing.R")
list(tar_target(dataFormatted, data_formatting(raw_data)), 
    tar_target(dataMissingValues, data_missing_values(dataFormatted)), 
    tar_target(dataOutliers, data_outliers(dataMissingValues)))