source("setup-packages.R")
source("scripts/1-exploratory-data-analysis.R")
source("scripts/1-data-preprocessing.R")
list(
     # 0.Formatting
     tar_target(dataFormatted, formatting(raw_data)), 
     tar_target(analyzedDataFormatted, analyze_post_formatting(dataFormatted)),
     # 1.Missing Values 
     tar_target(dataMissingValues, missing_values(dataFormatted)),
     tar_target(analyzedDataMissingValues, analyze_post_missing_values(dataMissingValues, data_before = dataFormatted)), 
     # 2.Temporal Feature Engineering 
     tar_target(dataTemporalFeature, temporal_feature(dataMissingValues)),
     tar_target(analyzedTemporalFeature, analyze_temporal_features(dataTemporalFeature, c("TotalTransactions", "BlockSize", "AverageFee", "TotalFees"), target_var = "TotalTransactions")),
     tar_target(analyzedTemporalFeatureLogTransform, analyze_temporal_features(dataTemporalFeature, c("TotalTransactions", "BlockSize", "AverageFee", "TotalFees"), target_var = "TotalTransactions", log_transform = TRUE)),
     # 3.Outliers 
     # comment: outliers_to_remove in outliers function has to be a list c()
     tar_target(dataOutliers, outliers(dataTemporalFeature, outliers_to_remove = TRUE)),
     tar_target(analyzeOutliers, analyze_outliers(dataTemporalFeature, dataOutliers)),
     # 4.Interaction and Polynomial Feature Engineering 
     tar_target(dataFullFeatureEngerineered, interaction_polynomial_feature(dataOutliers)),
     tar_target(analyzeInteractionPolynomialFeatures, analyze_interaction_polynomial_features(dataFullFeatureEngerineered, "TotalTransactions", c("Interaction_HourIsWeekend", "BlockSize_Squared", "BlockSize_Cubed", "HourOfDay_Squared", "HourOfDay_Cubed"))),
     # 5.Encoding # Apply to categorical variables
     tar_target(dataEncoded, encoding(dataFullFeatureEngerineered)),
     tar_target(analyzeEncodingEffects, analyze_encoding_effects(dataEncoded)),
     tar_target(reportEncodingEffects, generate_analysis_report(analyzeEncodingEffects))
  
     #tar_target(dataSplitted, splitting(dataPreprocessed), seed = 123)
     )


