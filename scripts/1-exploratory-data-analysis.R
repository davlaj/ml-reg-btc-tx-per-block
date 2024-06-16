# Visualizations and statistical analysis

# Load packages
source("setup-packages.R")

# Output directory
output_dir <- "output/exploratory-data-analysis"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Function to perform EDA after Data Formatting
analyze_post_formatting <- function(data) {
  
  # Open a text file for writing summary statistics
  sink(paste0(output_dir, "/0.1-data_summary_post_formating.txt"))
  cat("SUMMARY\n")
  cat("-----------------\n")
  print(summary(data))
  sink()  # Stop diverting the output to the file
  
  pdf(paste0(output_dir, "/0.2-histograms_post_formatting.pdf"))
  
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
  cat("SUMMARY\n")
  cat("-----------------\n")
  print(summary(data))
  sink()  # Stop diverting the output to the file
  
  # Open PDF output
  pdf(paste0(output_dir, "/1.2-histograms_post_missing_values.pdf"))
  
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

# Function to analyze Temporal Features
analyze_temporal_features <- function(data, variables, target_var, log_transform = FALSE) {

  # Open a text file for writing summary statistics
  sink(paste0(output_dir, "/2.1-data_summary_post_temporal_features.txt"))
  # Print the section title for column names in a clear format
  cat("COLUMN NAMES\n")
  cat("-----------------\n")
  print(names(data))
  cat("\n")  # Adding a newline for better separation
  cat("SUMMARY\n")
  cat("-----------------\n")
  print(summary(data))
  sink()  # Stop diverting the output to the file

  # Define the output file name based on whether log transformation is applied
  file_name <- if (log_transform) {
    "2.2b-log_transformed_temporal_feature_analysis.pdf"
  } else {
    "2.2a-temporal_feature_analysis.pdf"
  }

  # Open PDF output
  pdf(paste0(output_dir, "/", file_name))

  # Iterate over each variable
  for (var in variables) {
    # Convert character variable name to symbol
    var_sym <- sym(var)
    lag_var_sym <- sym(paste(var, "Lag1", sep = "_"))
    target_var_sym <- sym(target_var)

    # Ensure lagged column exists
    data[[paste(var, "Lag1", sep = "_")]] <- lag(data[[var]], 1)

    # Apply log transformation if selected
    transform <- function(x) if (log_transform) log1p(x) else x

    # Time series plot
    ts_plot <- ggplot(data, aes(x = Date, y = transform(!!var_sym))) +
      geom_line(color = "blue") +
      labs(title = paste(if (log_transform) "Log-Transformed" else "", "Time Series of", var),
           x = "Date", y = if (log_transform) paste("Log of", var) else var) +
      theme_minimal()

    # Decomposition plot, only if no NAs
    if (all(!is.na(data[[var]]))) {
      ts_data <- ts(transform(data[[var]]), frequency = 24*6)  # Adjust frequency based on data's seasonality
      decomposed_ts <- stl(ts_data, s.window = "periodic")
      decomposed_plot <- autoplot(decomposed_ts) +
        ggtitle(paste(if (log_transform) "Log-Transformed" else "", "Decomposed Time Series of", var))
    } else {
      decomposed_plot <- ggplot() +
        geom_blank() +
        ggtitle(paste(if (log_transform) "Log-Transformed" else "", "Decomposed Time Series of", var, "Not Available - Missing Values"))
    }

    # ACF plot
    acf_plot <- ggAcf(transform(data[[var]]), na.action = na.pass) +
      ggtitle(paste("ACF of", if (log_transform) "Log-Transformed" else "", var))

    # Scatter plot of Variable vs. its Lagged version
    scatter_plot <- ggplot(data[-1,], aes(x = transform(!!lag_var_sym), y = transform(!!var_sym))) +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "lm", color = "red") +
      labs(title = paste(if (log_transform) "Log of" else "", var, "vs. Lagged", if (log_transform) "Log of" else "", var),
           x = if (log_transform) paste("Lagged Log of", var) else paste("Lagged", var),
           y = if (log_transform) paste("Log of", var) else var) +
      theme_minimal()

    # Scatter plot of Variable vs. Target Variable
    correlation_plot <- ggplot(data, aes(x = data[[target_var_sym]], y = transform(!!var_sym))) +
      geom_point(alpha = 0.5, color = "green") +
      geom_smooth(method = "lm", color = "darkgreen") +
      labs(title = paste(if (log_transform) "Log of" else "", var, "vs.", target_var),
           x = target_var,
           y = if (log_transform) paste("Log of", var) else var) +
      theme_minimal()

    # Cross-Correlation Function (CCF) and plot
    # Interpretation: e.g. positive correlation at a lag of 5 implies that a change first variable
    # significantly predict the movement in the second variable five time units later
    ccf_data <- ccf(transform(data[[var]]), data[[target_var_sym]], na.action = na.exclude, lag.max = 20, plot = FALSE)
    ccf_plot <- autoplot(ccf_data) +
      ggtitle(paste("Cross-Correlation Function of", var, "and", target_var))

    # Print each set of plots on a new page including the CCF plot
    print(gridExtra::grid.arrange(ts_plot, decomposed_plot, acf_plot, scatter_plot, correlation_plot, ccf_plot, ncol = 2))
  }

  # Close the PDF
  dev.off()
}

# Function to analyze Outliers
analyze_outliers <- function(data_pre, data_post) {
  # Open PDF output with specified width and height
  pdf(paste0(output_dir, "/3.1a-boxplot_outliers_treatment_visual.pdf"), width = 12, height = 10)
  # Open a text file for writing summary statistics
  stats_file <- file(paste0(output_dir, "/3.1b-boxplot_outliers_summary_stats.txt"), open = "wt")
  
  # Identify numeric variables
  numeric_vars <- sapply(data_pre, is.numeric)
  
  # Prepare data for plotting
  data_pre_long <- reshape2::melt(data_pre[, numeric_vars], id.vars = NULL)
  data_post_long <- reshape2::melt(data_post[, numeric_vars], id.vars = NULL)
  
  # Add a new 'Condition' column to differentiate the datasets
  data_pre_long$Condition <- 'Before'
  data_post_long$Condition <- 'After'
  
  # Combine the datasets
  combined_data <- rbind(data_pre_long, data_post_long)
  
  # Reorder the Condition factor
  combined_data$Condition <- factor(combined_data$Condition, levels = c("Before", "After"))
  
  # Calculate IQR and identify outliers for coloring
  combined_data <- combined_data %>%
    group_by(variable, Condition) %>%
    mutate(
      Q1 = quantile(value, probs = 0.25, na.rm = TRUE),
      Q3 = quantile(value, probs = 0.75, na.rm = TRUE),
      IQR = Q3 - Q1,
      Lower = Q1 - 1.5 * IQR,
      Upper = Q3 + 1.5 * IQR,
      Outlier = ifelse(value < Lower | value > Upper, "Outlier", "Inlier")
    ) %>%
    ungroup()
  
  # Write summary statistics
  writeLines("Summary Statistics for Numeric Variables:", stats_file)
  for(variable in unique(combined_data$variable)) {
    writeLines(paste("\nVariable:", variable), stats_file)
    writeLines("Metric\tBefore\tAfter\t% Change", stats_file)
    
    before_data <- combined_data %>% filter(variable == variable, Condition == "Before")
    after_data <- combined_data %>% filter(variable == variable, Condition == "After")
    
    before_stats <- summary(before_data$value)
    after_stats <- summary(after_data$value)
    
    before_outliers_count <- sum(before_data$Outlier == "Outlier")
    after_outliers_count <- sum(after_data$Outlier == "Outlier")
    
    metrics <- c("Min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max", "IQR", "Outliers", "Outliers (%)")
    before_values <- c(before_stats[1], before_stats[2], before_stats[3], before_stats[4], before_stats[5], before_stats[6], before_data$IQR[1], before_outliers_count, (before_outliers_count / nrow(before_data)) * 100)
    after_values <- c(after_stats[1], after_stats[2], after_stats[3], after_stats[4], after_stats[5], after_stats[6], after_data$IQR[1], after_outliers_count, (after_outliers_count / nrow(after_data)) * 100)
    
    for(i in seq_along(metrics)) {
      change <- ((after_values[i] - before_values[i]) / before_values[i]) * 100
      stats_text <- paste(metrics[i], before_values[i], after_values[i], round(change, 2), sep = "\t")
      writeLines(stats_text, stats_file)
    }
    
    # Calculate the percentage of outliers removed
    removed_outliers <- before_outliers_count - after_outliers_count
    percentage_removed <- (removed_outliers / before_outliers_count) * 100
    writeLines(paste("Outliers removed:", removed_outliers, "\tPercentage removed:", round(percentage_removed, 2), "%"), stats_file)
  }
  
  # Creating boxplots for all numeric variables, 6 plots per page
  for (i in seq_len(ceiling(length(unique(combined_data$variable)) / 6))) {
    vars_subset <- unique(combined_data$variable)[((i - 1) * 6 + 1):min(i * 6, length(unique(combined_data$variable)))]
    data_subset <- combined_data %>% filter(variable %in% vars_subset)
    
    p <- ggplot(data_subset, aes(x = Condition, y = value, fill = Condition)) +
      geom_boxplot(outlier.shape = NA, width = 0.6, position = position_dodge(width = 0.8), alpha = 0.5) +  # Added transparency to boxplots
      geom_point(aes(color = Outlier, shape = Outlier), position = position_jitterdodge(jitter.width = 0.02, dodge.width = 0.8), size = 1.5, alpha = 0.5) +  # Added transparency to points
      scale_color_manual(values = c("Outlier" = "orange", "Inlier" = "darkgray"), guide = guide_legend(override.aes = list(alpha = 1))) +  # Ensure legend colors are opaque
      scale_shape_manual(values = c("Outlier" = 1, "Inlier" = 1)) +  # unfilled shapes
      facet_wrap(~variable, scales = "free_y", ncol = 3) +
      scale_fill_manual(values = c("Before" = "lightblue", "After" = "forestgreen"), guide = guide_legend(override.aes = list(alpha = 1))) +  # Ensure legend colors are opaque
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom"
      ) +
      labs(
        title = "Comparison of Numeric Variables Before and After Outliers Treatment",
        x = "",
        y = "Values",
        subtitle = "Each plot corresponds to a different variable"
      )
    
    # Print the plot to PDF
    print(p)
  }
  
  # Close the PDF and text file
  dev.off()
  close(stats_file)
}

# Function to evaluate Interaction and Polynomial Features
analyze_interaction_polynomial_features <- function(data, target_var, feature_list) {
  # Create the PDF for scatter plots
  pdf(paste0(output_dir, "/4.1-scatter_plots_interaction_polynomial_features.pdf"), width = 20, height = 15)
  
  # Create scatter plots for each feature against the target variable
  plots <- list()
  for (feature in feature_list) {
    p <- ggplot(data, aes(x = !!sym(feature), y = !!sym(target_var))) +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "lm", color = "blue") +
      labs(title = paste(target_var, "vs.", feature),
           x = feature,
           y = target_var) +
      theme_minimal()
    
    plots[[length(plots) + 1]] <- p  # Add each plot to the list
  }
  
  # Determine number of plots per page
  plots_per_page <- 6  # 2x3 layout
  page_number <- ceiling(length(plots) / plots_per_page)
  
  for (i in seq_len(page_number)) {
    # Subset plots for the current page
    slice_start <- (i - 1) * plots_per_page + 1
    slice_end <- min(i * plots_per_page, length(plots))
    page_plots <- plots[slice_start:slice_end]
    page_plots_layout <- wrap_plots(page_plots, ncol = 3)
    
    print(page_plots_layout)
  }
  
  # Close the PDF for scatter plots
  dev.off()
  
  # Calculate the correlation matrix for the selected features with the target variable
  relevant_features <- c(target_var, feature_list)
  corr_matrix <- cor(data[relevant_features], use = "complete.obs")
  
  # Save the correlation matrix as a heatmap with correlation values
  pdf(paste0(output_dir, "/4.2-heatmap_interaction_polynomial_features.pdf"), width = 12, height = 10)
  
  # Convert the correlation matrix to a long format for ggplot2
  melted_corr <- melt(corr_matrix)
  
  heatmap <- ggplot(melted_corr, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() +
    geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
    scale_fill_gradient2(low = "blue", high = "forestgreen", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name = "Correlation") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = paste("Correlation Heatmap of", target_var, "and Selected Features"),
         x = "",
         y = "")
  
  print(heatmap)
  
  # Close the PDF for correlation heatmap
  dev.off()
}

# Function to evaluate Encoding Effects
analyze_encoding_effects <- function(data) {
  # Set up the output files
  pdf_path <- paste0(output_dir, "/5.1-encoding_effects_analysis.pdf")
  txt_path <- paste0(output_dir, "/5.1-encoding_effects_analysis_details.txt")
  corr_matrix_rds_path <- paste0(output_dir, "/.5.1-encoding_effects_corr_matrix.rds")
  pca_summary_rds_path <- paste0(output_dir, "/.5.1-encoding_effects_pca_summary.rds")

  cat("Starting analyze_encoding_effects function\n")
  cat("Output files set up\n")

  # Open a PDF device for plots
  pdf(pdf_path, width = 12, height = 10)

  # Open a connection to a text file for diagnostic outputs
  txt_con <- file(txt_path, open = "wt")

  cat("PDF and text connections opened\n")

  # Filter numeric data
  data_numeric <- data %>% select(where(is.numeric))

  # Scale the data
  data_scaled <- scale(data_numeric)

  cat("Data scaled\n")

  # Compute the correlation matrix
  corr_matrix <- cor(data_scaled, use = "pairwise.complete.obs")

  cat("Correlation matrix computed\n")

  # Print the correlation matrix to the text file and save as RDS
  writeLines("Correlation Matrix:\n", txt_con)
  capture.output(print(corr_matrix), file = txt_con, append = TRUE)
  saveRDS(corr_matrix, corr_matrix_rds_path)

  cat("Correlation matrix saved to RDS\n")

  # Plotting the correlation matrix
  corrplot(corr_matrix, method = "circle", type = "lower", order = "hclust",
           tl.col = "black", tl.srt = 45)

  # Perform PCA and plot the results
  pca_result <- prcomp(data_scaled, scale. = TRUE)
  plot(pca_result, type = "lines", main = "PCA Scree Plot")
  biplot(pca_result, main = "PCA Biplot", cex = 0.7)

  cat("PCA performed and plotted\n")

  # Print PCA summary to the text file and save as RDS
  writeLines("\nPCA Summary:\n", txt_con)
  pca_summary <- summary(pca_result)
  capture.output(pca_summary, file = txt_con, append = TRUE)
  saveRDS(pca_summary, pca_summary_rds_path)

  cat("PCA summary saved to RDS\n")

  # Close the PDF and text file
  dev.off()
  close(txt_con)

  cat("PDF and text connections closed\n")
}


generate_analysis_report <- function(data) {
  log_file <- paste0(output_dir, "/generate_analysis_report_debug.log")
  log_con <- file(log_file, open = "wt")
  
  log <- function(message) {
    cat(message, "\n", file = log_con, append = TRUE)
  }
  
  log("Starting generate_analysis_report")
  
  # Paths to the input files
  corr_matrix_rds_path <- paste0(output_dir, "/.5.1-encoding_effects_corr_matrix.rds")
  pca_summary_rds_path <- paste0(output_dir, "/.5.1-encoding_effects_pca_summary.rds")
  
  log(paste("Correlation matrix RDS file:", corr_matrix_rds_path))
  log(paste("PCA summary RDS file:", pca_summary_rds_path))
  
  # Check if the correlation matrix file exists
  if (!file.exists(corr_matrix_rds_path)) {
    log("Error: Correlation matrix file does not exist.")
    stop("Error: Correlation matrix file does not exist.")
  }
  
  # Check if the PCA summary file exists
  if (!file.exists(pca_summary_rds_path)) {
    log("Error: PCA summary file does not exist.")
    stop("Error: PCA summary file does not exist.")
  }
  
  # Read the correlation matrix and PCA summary from RDS files
  tryCatch({
    corr_matrix <- readRDS(corr_matrix_rds_path)
    log("Correlation matrix successfully read.")
  }, error = function(e) {
    log(paste("Error reading correlation matrix:", e$message))
    stop("Failed to read the correlation matrix: ", e$message)
  })
  
  tryCatch({
    pca_summary <- readRDS(pca_summary_rds_path)
    log("PCA summary successfully read.")
  }, error = function(e) {
    log(paste("Error reading PCA summary:", e$message))
    stop("Failed to read the PCA summary: ", e$message)
  })
  
  # Open PDF output for the report
  pdf_path <- paste0(output_dir, "/analysis_report.pdf")
  pdf(pdf_path, width = 12, height = 10)
  
  log("Generating correlation matrix plot.")
  
  # Analysis of the Correlation Matrix
  corrplot(corr_matrix, 
           method = "circle", 
           type = "lower", 
           title = "Correlation Matrix", 
           mar = c(0, 0, 2, 0), 
           tl.cex = 0.8,       # Adjust text size
           tl.col = "black",   # Text color
           tl.srt = 45,        # Rotate text
           number.cex = 0.5)   # Size of correlation coefficients
  
  # Analyze correlation with the target variable
  target_var <- "TotalTransactions"
  corr_with_target <- sort(corr_matrix[target_var, ], decreasing = TRUE)
  corr_with_target <- corr_with_target[!names(corr_with_target) %in% target_var]
  
  log("Generating correlation with target variable plot.")
  
  # Plot correlation with the target variable
  corr_plot <- ggplot(data = data.frame(Variable = names(corr_with_target), Correlation = corr_with_target),
                      aes(x = reorder(Variable, Correlation), y = Correlation)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    labs(title = paste("Correlation with", target_var), x = "Variables", y = "Correlation")
  
  print(corr_plot)
  
  log("Displaying PCA summary.")
  
  # Display PCA summary as text
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(1, 1)))
  grid.text("PCA Summary", x = 0.5, y = 0.9, gp = gpar(fontsize = 20))
  
  pca_summary_text <- capture.output(print(pca_summary))
  pca_summary_text <- paste(pca_summary_text, collapse = "\n")
  grid.text(pca_summary_text, x = 0.5, y = 0.5, just = "center", gp = gpar(fontsize = 10))
  
  log("Generating decision suggestions.")
  
  # Decision Suggestions based on Correlation Analysis
  decision_text <- capture.output({
    cat("### Decision Suggestions based on Correlation Analysis\n\n")
    cat("1. **Features with High Correlation**: \n")
    high_corr_features <- names(corr_with_target[abs(corr_with_target) > 0.3])
    if (length(high_corr_features) > 0) {
      cat("Consider using the following features in your model as they show a significant correlation with the target variable:\n")
      cat(paste0("- ", high_corr_features, "\n"), sep = "")
    } else {
      cat("No features show a significant correlation (above 0.3) with the target variable.\n")
    }
    
    cat("\n2. **Features with Moderate Correlation**: \n")
    moderate_corr_features <- names(corr_with_target[abs(corr_with_target) > 0.1 & abs(corr_with_target) <= 0.3])
    if (length(moderate_corr_features) > 0) {
      cat("Consider using the following features with caution, as they show moderate correlation with the target variable:\n")
      cat(paste0("- ", moderate_corr_features, "\n"), sep = "")
    } else {
      cat("No features show moderate correlation (between 0.1 and 0.3) with the target variable.\n")
    }
    
    cat("\n### Decision Suggestions based on PCA Analysis\n\n")
    cat("3. **Principal Components**: \n")
    cat("Consider the principal components that explain the majority of the variance. Use these components to reduce dimensionality if needed.\n")
  })
  
  decision_text <- paste(decision_text, collapse = "\n")
  
  # Print the decision suggestions
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(1, 1)))
  grid.text("Decision Suggestions", x = 0.5, y = 0.9, gp = gpar(fontsize = 20))
  grid.text(decision_text, x = 0.5, y = 0.5, just = "center", gp = gpar(fontsize = 10))
  
  # Close the PDF
  dev.off()
  
  log("Analysis report generated successfully.")
  
  close(log_con)
  
  # Return the path to the generated PDF
  return(pdf_path)
}

# Function to perform Final Data Integrity Check
final_data_integrity_check <- function(data) {
  pdf(paste0(output_dir, "/final_data_integrity_check.pdf"))

  # Summary statistics and any final checks
  print(summary(data))

  dev.off()
}


