# EDA

# Check the structure
str(data)

# Data loading function
load_data <- function(file_path) {
  data <- read.csv(file_path)
  # Assuming Date needs conversion to POSIXct
  data$Date <- as.POSIXct(data$Date, origin="1970-01-01", tz="UTC")
  return(data)
}

data <- load_data("data/raw/block_data.csv")

# Basic summary statistics
summary_statistics <- function(data) {
  summary(data)
}
print(summary_statistics(data))

# 1. Statistical Tests
# 1.1 Correlation Analysis - For Numeric Variables
correlation_analysis <- function(data) {
  numeric_data <- select(data, where(is.numeric))
  corr_matrix <- cor(numeric_data, use = "complete.obs") # Handling missing values
  pdf("output/exploratory-data-analysis/correlation_analysis.pdf")
  corrplot(corr_matrix, method = "circle")
  dev.off()
}
correlation_analysis(data)

# 1.2 ANOVA or Chi-Square - For Categorical Variables vs Numeric Response
anova_chi_square <- function(data, categorical_var, response_var) {
  # Assuming response_var is numeric and categorical_var is factor
  data[[categorical_var]] <- as.factor(data[[categorical_var]])
  aov_result <- aov(data[[response_var]] ~ data[[categorical_var]], data = data)
  pdf(paste0("output/exploratory-data-analysis/anova_", categorical_var, ".pdf"))
  plot(aov_result)
  dev.off()
  return(summary(aov_result))
}
# Example call - replace 'category_var' and 'numeric_response' with actual variable names
# print(anova_chi_square(data, "category_var", "numeric_response"))

# 2. Feature Importance
# 2.1 Scatter Plots - Numeric vs Numeric
scatter_plots <- function(data) {
  numeric_vars <- names(select(data, where(is.numeric)))
  pdf("output/exploratory-data-analysis/scatter_plots.pdf")
  ggpairs(data, columns = numeric_vars, title = "Scatter plots of Numeric Features")
  dev.off()
}
scatter_plots(data)

# 3. Data Visualization
# Histograms for all numeric variables
plot_histograms <- function(data) {
  numeric_data <- select(data, where(is.numeric))
  pdf("output/exploratory-data-analysis/histograms.pdf")
  numeric_data %>% 
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
    ggplot(aes(x = Value)) +
    geom_histogram(bins = 30) +
    facet_wrap(~ Variable, scales = "free_x") +
    theme_minimal() +
    labs(title = "Histograms of Numeric Variables") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  dev.off()
}
plot_histograms(data)

# Boxplots for all numeric variables
plot_boxplots <- function(data) {
  numeric_data <- select(data, where(is.numeric))
  pdf("output/exploratory-data-analysis/boxplots.pdf")
  numeric_data %>% 
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
    ggplot(aes(x = "", y = Value)) +
    geom_boxplot() +
    facet_wrap(~ Variable, scales = "free_y") +
    theme_minimal() +
    labs(title = "Boxplots of Numeric Variables") +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  dev.off()
}
plot_boxplots(data)
