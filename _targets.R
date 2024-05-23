source("setup-packages.R")
source("scripts/1-exploratory-data-analysis.R")
source("scripts/1-data-preprocessing.R")
list(tar_target(dataFormatted, formatting(raw_data)), 
     tar_target(analyzedDataFormatted, analyze_post_formatting(dataFormatted)),
     tar_target(dataMissingValues, missing_values(dataFormatted)),
     tar_target(analyzedDataMissingValues, analyze_post_missing_values(dataMissingValues)), 
     tar_target(dataTemporalFeature, temporal_feature(dataMissingValues)),
     tar_target(analyzedTemporalFeature, analyze_temporal_features(dataTemporalFeature, c("TotalTransactions", "BlockSize", "AverageFee", "TotalFees")))
    # tar_target(dataOutliers, outliers(dataTemporalFeature, outliers_to_remove = "AverageFee")),
    # tar_target(dataFullFeatureEngerineered, interaction_polynomial_feature(dataOutliers))
     #tar_target(dataSplitted, splitting(dataPreprocessed), seed = 123)
     )


