# Visualizations and statistical analysis

# Load packages
source("setup-packages.R")

# Setup output directory
output_dir <- "output/exploratory-data-analysis"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Function to perform EDA after Data Formatting
analyze_post_formatting <- function(data) {
  
  # Open a text file for writing summary statistics
  sink(paste0(output_dir, "/0.1-data_summary_post_formating.txt"))
  print(summary(data))
  sink()  # Stop diverting the output to the file
  
  pdf(paste0(output_dir, "/0.2-post_formatting_histograms.pdf"))
  
  # Histograms for all numeric columns
  p <- data %>%
    select(where(is.numeric)) %>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
    ggplot(aes(x = Value)) +
    geom_histogram(bins = 30, alpha = 0.3, fill = "blue") +
    facet_wrap(~ Variable, scales = "free_x") +
    ggtitle("Histograms of Numeric Variables") +
    theme_minimal()
  
  print(p)  # Explicitly print the ggplot object
  dev.off()
}

# Function to analyze data after Missing Values Treatment
analyze_post_missing_values <- function(data, data_before = NULL) {
  
  # Open a text file for writing summary statistics
  sink(paste0(output_dir, "/1.1-data_summary_post_missing_values.txt"))
  print(summary(data))
  sink()  # Stop diverting the output to the file
  
  # Open PDF output
  pdf(paste0(output_dir, "/1.2-post_missing_values_histograms.pdf"))
  
  # Plotting changes in distribution if 'data_before' is available
  if (!is.null(data_before)) {
    data_comparison <- bind_rows(
      mutate(data_before, Stage = "Before"),
      mutate(data, Stage = "After")
    ) %>%
      pivot_longer(cols = where(is.numeric), names_to = "Variable", values_to = "Value")
    
    p <- ggplot(data_comparison, aes(x = Value, fill = Stage)) +
      geom_histogram(data = ~ subset(., Stage == "After"), bins = 30, alpha = 0.7, position = "identity") +
      geom_histogram(data = ~ subset(., Stage == "Before"), bins = 30, alpha = 0.3, position = "identity") +
      facet_wrap(~ Variable, scales = "free_x") +
      scale_fill_manual(values = c("Before" = "blue", "After" = "orange")) +
      labs(title = "Comparative Histograms Before and After Missing Values Treatment", x = NULL, y = "Count") +
      theme_minimal() +
      guides(fill = guide_legend(title = "Data Stage", labels = c("Before", "After")))
    
    print(p)  # Print the plot to the PDF
  } else {
    print(ggplot(data, aes(x = 1)) + geom_blank() +
            ggtitle("No comparison plot generated: 'data_before' not provided"))
  }
  
  # Close the PDF device
  dev.off()
}

# # Function to analyze Temporal Features
# analyze_temporal_features <- function(data) {
#   
#   # Open PDF output
#   pdf(paste0(output_dir, "/2.1-temporal_feature_analysis.pdf"))
#   
#   # Time series plot of Total Transactions
#   p1 <- ggplot(data, aes(x = Date, y = TotalTransactions)) +
#     geom_line(color = "blue") +
#     labs(title = "Time Series of Total Transactions", x = "Date", y = "Total Transactions") +
#     theme_minimal()
#   
#   # Time series decomposition of Total Transactions
#   if (all(!is.na(data$TotalTransactions))) {
#     ts_data <- ts(data$TotalTransactions, frequency = 24*6)  # adjust frequency based on your data's seasonality
#     decomposed_ts <- stl(ts_data, s.window = "periodic")
#     p2 <- autoplot(decomposed_ts) + 
#       ggtitle("Decomposed Time Series of Total Transactions")
#   } else {
#     p2 <- ggplot() + 
#       geom_blank() +
#       ggtitle("Decomposed Time Series Not Available - Missing Values")
#   }
#   
#   # Autocorrelation Function (ACF) plot
#   p3 <- ggAcf(data$TotalTransactions, na.action = na.pass) +
#     ggtitle("ACF of Total Transactions")
#   
#   # Scatter plot of Transactions vs. Lagged Transactions
#   p4 <- ggplot(data, aes(x = TotalTransactions_Lag1, y = TotalTransactions)) +
#     geom_point(alpha = 0.5) +
#     geom_smooth(method = "lm", color = "red") +
#     labs(title = "Total Transactions vs. Lagged Transactions",
#          x = "Lagged Transactions", y = "Total Transactions") +
#     theme_minimal()
#   
#   # Combining plots
#   grid.arrange(p1, p2, p3, p4, ncol = 2)
#   
#   dev.off()
# }

analyze_temporal_features <- function(data, variables) {
  # Open PDF output
  pdf(paste0(output_dir, "/2.1-temporal_feature_analysis.pdf"))
  
  # Iterate over each variable
  for (var in variables) {
    # Convert character variable name to symbol
    var_sym <- sym(var)
    lag_var_sym <- sym(paste(var, "Lag1", sep = "_"))
    
    # Ensure lagged column exists
    data[[paste(var, "Lag1", sep = "_")]] <- dplyr::lag(data[[var]], 1)
    
    # Time series plot
    ts_plot <- ggplot(data, aes(x = Date, y = !!var_sym)) +
      geom_line(color = "blue") +
      labs(title = paste("Time Series of", var), x = "Date", y = var) +
      theme_minimal()
    
    # Decomposition plot, only if no NAs
    if (all(!is.na(data[[var]]))) {
      ts_data <- ts(data[[var]], frequency = 24*6)  # Adjust frequency based on data's seasonality
      decomposed_ts <- stl(ts_data, s.window = "periodic")
      decomposed_plot <- autoplot(decomposed_ts) + 
        ggtitle(paste("Decomposed Time Series of", var))
    } else {
      decomposed_plot <- ggplot() + 
        geom_blank() +
        ggtitle(paste("Decomposed Time Series of", var, "Not Available - Missing Values"))
    }
    
    # ACF plot
    acf_plot <- ggAcf(data[[var]], na.action = na.pass) +
      ggtitle(paste("ACF of", var))
    
    # Scatter plot of Variable vs. its Lagged version
    scatter_plot <- ggplot(data, aes(x = !!lag_var_sym, y = !!var_sym)) +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "lm", color = "red") +
      labs(title = paste(var, "vs. Lagged", var),
           x = paste("Lagged", var), y = var) +
      theme_minimal()
    
    # Print each set of plots on a new page
    print(gridExtra::grid.arrange(ts_plot, decomposed_plot, acf_plot, scatter_plot, ncol = 2))
  }
  
  # Close the PDF
  dev.off()
}
#analyze_temporal_features(data, c("TotalTransactions", "BlockSize", "AverageFee", "TotalFees"))


#Statistical Tests: Consider statistical tests for seasonality, such as the Ljung-Box test on 
#the seasonal component, to quantitatively assess its significance.

#ADF (Augmented Dickey-Fuller) Test: Tests the null hypothesis that a unit root is present in a time series sample 
#(i.e., the series is non-stationary).
#KPSS (Kwiatkowski-Phillips-Schmidt-Shin) Test: Tests for the stationarity of a time series around a deterministic 
#trend.
#Ljung-Box Test: Checks if there are significant autocorrelations at lag k in the residuals of a time series model.



#################################################


# Function to evaluate Outliers Treatment
analyze_post_outliers <- function(data) {
  pdf(paste0(output_dir, "/post_outliers_treatment.pdf"))

  # Boxplots for numeric variables before and after treatment
  # Similar structure as 'analyze_post_missing_values' if before and after data are available

  dev.off()
}

# Function to evaluate Interaction and Polynomial Features
analyze_interaction_polynomial_features <- function(data) {
  pdf(paste0(output_dir, "/interaction_polynomial_feature_analysis.pdf"))

  # Scatter plots of interaction features against the target
  ggplot(data, aes(x = Interaction_BlockSizeAvgFee, y = TotalTransactions)) +
    geom_point() +
    geom_smooth(method = "lm") +
    labs(title = "Effect of Block Size and Average Fee Interaction on Transactions")

  dev.off()
}

# Function to analyze Encoding Effects
analyze_encoding_effects <- function(data) {
  pdf(paste0(output_dir, "/encoding_effects_analysis.pdf"))

  # Correlation matrix including new dummy variables
  data_numeric <- data %>% select(where(is.numeric))
  corr_matrix <- cor(data_numeric)
  corrplot(corr_matrix, method = "circle")

  dev.off()
}

# Function to perform Final Data Integrity Check
final_data_integrity_check <- function(data) {
  pdf(paste0(output_dir, "/final_data_integrity_check.pdf"))

  # Summary statistics and any final checks
  print(summary(data))

  dev.off()
}


