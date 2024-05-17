source("setup-packages.R")
source("scripts/data-preprocessing.R")
list(tar_target(dataFormatted, formatting(raw_data)), 
    tar_target(dataMissingValues, missing_values(dataFormatted)), 
    tar_target(dataOutliers, outliers(dataMissingValues, outliers_to_remove = "AverageFee"))
    #tar_target(dataSplitted, splitting(dataPreprocessed), seed = 123)
    )